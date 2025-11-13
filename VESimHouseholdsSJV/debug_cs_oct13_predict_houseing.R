input_folder <- "E:/Projects/Clients/SJV_VE/Model_runs/VisionEval-Dev/built/visioneval/4.3.3/runtime/models/VERSPM-SJV-base/inputs"
results_folder <- "E:/Projects/Clients/SJV_VE/Model_runs/VisionEval-Dev/built/visioneval/4.3.3/runtime/models/VERSPM-SJV-base/results/Datastore"

the_year <- 2022


# Example script -- read in the contents of one or more datastores from rda files to data.tables for analysis
# This doesn't require exporting to csv, just directly imports all of the rda files
# Useful if you prefer to work in R in data.table format
# For larger models will result in very large tables in memory

library(data.table)
library(ggplot2)
library(visioneval)

#------------------------------------------------------------------------------------------
# code to read in results from the datastore for a particular scenario
scen_list <- list()

# specify names for the scenario labels and their paths
project_dir <- "E:/Projects/Clients/SJV_VE/Model_runs/VisionEval-Dev/built/visioneval/4.3.3/runtime/models"
model_names <- c("VERSPM-SJV-base")
scenarios <- file.path(project_dir, model_names, "results/Datastore", the_year) 

# Import results

# cycle through the scenarios 
for(scen_num in 1:length(scenarios)){
  #scen_num <- 1
  
  output_tabs <- list.files(scenarios[scen_num])
  outputs_vars <- paste0(output_tabs, "Vars")
  
  for(tbl in 1:length(output_tabs)){
    
    tblfiles <- list.files(file.path(scenarios[scen_num], output_tabs[tbl]))
    tblfiles_noext <- gsub(".Rda","", tblfiles)
    
    for(i in 1:length(tblfiles)){
      load(file.path(scenarios[scen_num], output_tabs[tbl], tblfiles[i]))  
      assign(tblfiles_noext[i], Dataset)
    }
    
    assign(outputs_vars[tbl], data.table(sapply(tblfiles_noext, get)))  
    
  }
  
  scen_list[[model_names[scen_num]]] <- lapply(outputs_vars, get)
  names(scen_list[[model_names[scen_num]]]) <- outputs_vars
}

# create combined tables across scenarios
scen_hh <- lapply(scen_list, "[[", "HouseholdVars")
scen_hh <- rbindlist(scen_hh, idcol = "scenario")
str(scen_hh)
cols <- c("HhSize", "Income")
scen_hh[,  (cols) := lapply(.SD, as.numeric), .SDcols = cols]

scen_veh <- lapply(scen_list, "[[", "VehicleVars")
scen_veh <- rbindlist(scen_veh, idcol = "scenario")

scen_wrk <- lapply(scen_list, "[[", "WorkerVars")
scen_wrk <- rbindlist(scen_wrk, idcol = "scenario")

scen_bz <- lapply(scen_list, "[[", "BzoneVars")
scen_bz <- rbindlist(scen_bz, idcol = "scenario")

str(scen_bz)
cols <- c("SFDU", "MFDU", "HhPropIncQ1", "HhPropIncQ2", "HhPropIncQ3", "HhPropIncQ4", "GQDU")
scen_bz[,  (cols) := lapply(.SD, as.numeric), .SDcols = cols]

scen_az <- lapply(scen_list, "[[", "AzoneVars")
scen_az <- rbindlist(scen_az, idcol = "scenario")

scen_ma <- lapply(scen_list, "[[", "MareaVars")
scen_ma <- rbindlist(scen_ma, idcol = "scenario")

# assemble into a list 
L <- list()
L$Year <- list(Household = as.data.frame(scen_hh),
               Bzone = as.data.frame(scen_bz))
str(L$Year)

#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================
#This function predicts the housing type each household. It uses the estimated
#binomial choice model for determining the probability that the housing choice
#for each household is single family (SF) vs. multifamily (MF). The group
#quarters population is assigned to group quarters (GQ).

#Iterative proportional fitting function used in Bzone allocation
ipf <- function(Seed_ar, MrgnVals_ls, MrgnDims_ls, RmseTarget = 1e-5, MaxIter = 100) {
  
  # TODO: consolidate package parameter defaults into one place (see HighDensityThreshold)
  packageParams <- visioneval::getRunParameter("VELandUse",Default=list(fixIPF=1))
  ipfFix <- if ( ! "ipfFix" %in% names(packageParams) ) 1 else packageParams$ipfFix
  # packageParams might be set but contain only other parameters
  getZeroCells <- if ( ipfFix == 0 ) function(x){0} else function(x){x==0}
  
  #Eliminate zero values in margins
  MrgnVals_ls <-
    lapply(MrgnVals_ls, function(x) {
      x[getZeroCells(x)] <- 1e-6
      x
    })
  #Starting value for Units_ar
  Units_ar <- Seed_ar
  #Function to sum up Units_ar by margin
  sumArray <- function(MrgnDims_) {
    apply(Units_ar, MrgnDims_, sum, na.rm=TRUE)
  }
  #Function to calculate RMSE error
  rmse <- function() {
    #Make a vector of margin values
    MrgnVals_ <- unlist(lapply(MrgnVals_ls, function(x) as.vector(x)))
    MrgnSums_ <- numeric(0)
    for (i in 1:length(MrgnDims_ls)) {
      MrgnSums_ <- c(MrgnSums_, as.vector(sumArray(MrgnDims_ls[[i]])))
    }
    Err_ <- MrgnVals_ - MrgnSums_
    sqrt(sum(Err_^2) / (length(Err_)))
  }
  #Balance unit match or iterations exceeded
  NumIter <- 0
  RmseErr <- rmse()
  writeLog( paste("RmseErr:",RmseErr,"; RmseTarget:",RmseTarget), Level="debug" )
  while (all(RmseErr > RmseTarget & NumIter < MaxIter)) {
    for (i in 1:length(MrgnDims_ls)) {
      MrgnSum_ar <- sumArray(MrgnDims_ls[[i]])
      MrgnAdj_ar <- MrgnVals_ls[[i]] / MrgnSum_ar
      Units_ar <- sweep(Units_ar, MrgnDims_ls[[i]], MrgnAdj_ar, "*")
      Units_ar[Units_ar == 0] <- 1e-6
    }
    RmseErr <- rmse()
    NumIter <- NumIter + 1
  }
  list(Units_ar = Units_ar, NumIter = NumIter, MaxIter = MaxIter, RmseErr = RmseErr)
}

#Main module function that assigns housing type and Bzone for each household
#---------------------------------------------------------------------------
PredictHousing <- function(L) {
  #Set up
  #------
  #Fix seed as synthesis involves sampling
  #set.seed(L$G$Seed)
  #Initialize a vector to store housing type
  HouseType_Hh <- character(length(L$Year$Household$HhId))
  names(HouseType_Hh) <- L$Year$Household$HhId
  #Identify which households are group quarters
  IsGQ_Hh <- L$Year$Household$HhType == "Grp"
  HouseType_Hh[IsGQ_Hh] <- "GQ"
  
  #Predict housing type for each household
  #---------------------------------------
  #Make data frame of household variables and split by Azone
  Hh_df_Az <-
    split(
      data.frame(L$Year$Household)[!IsGQ_Hh,],
      L$Year$Household$Azone[!IsGQ_Hh]
    )
  Az <- names(Hh_df_Az)
  HouseTypeModel_ls <- loadPackageDataset("HouseTypeModel_ls","VELandUse")
  for (az in Az) {
    #Calculate the single family housing proportion
    SFDU <- sum(L$Year$Bzone$SFDU[L$Year$Bzone$Azone == az])
    MFDU <- sum(L$Year$Bzone$MFDU[L$Year$Bzone$Azone == az])
    AllDU <- SFDU + MFDU
    MissingHouseholds <- AllDU == 0
    PropSFDU <- ifelse( ! MissingHouseholds,  SFDU / AllDU, 1.0 )
    L$Year$Bzone$SFDU[ L$Year$Bzone$Azone == az & MissingHouseholds ] <- 1.0 # Force at least one household
    
    #Predict housing type
    HouseType_ <- applyBinomialModel(
      HouseTypeModel_ls,
      Hh_df_Az[[az]],
      TargetProp = PropSFDU
    )
    Hh_df_Az[[az]]$HouseType <- HouseType_
    names(HouseType_) <- Hh_df_Az[[az]]$HhId
    HouseType_Hh[names(HouseType_)] <- HouseType_
  }
  rm(SFDU, MFDU, PropSFDU, HouseType_)
  
  #Tabulate households by house type, income quartile, and Azone
  #-------------------------------------------------------------
  #Calculate regional income quartiles for households
  IncQBreaks_ <-
    quantile(L$Year$Household$Income[!IsGQ_Hh], c(0, 0.25, 0.5, 0.75, 1))
  Iq <- c("IncQ1", "IncQ2", "IncQ3", "IncQ4")
  #Create overall vector to keep results
  IncQ_Hh <- character(length(L$Year$Household$HhId))
  names(IncQ_Hh) <- L$Year$Household$HhId
  IncQ_Hh[IsGQ_Hh] <- "NA"
  #Calculate income quartile of each household
  for (az in Az) {
    Hh_df_Az[[az]]$IncQ <-
      cut(Hh_df_Az[[az]]$Income,
          breaks = IncQBreaks_,
          labels = Iq,
          include.lowest = TRUE)
    IncQ_ <- as.character(Hh_df_Az[[az]]$IncQ)
    names(IncQ_) <- Hh_df_Az[[az]]$HhId
    IncQ_Hh[names(IncQ_)] <- IncQ_
    rm(IncQ_)
  }
  rm(az, IncQBreaks_)
  #Tabulate households by house type and income quartile by Azone
  Ht <- c("SF", "MF")
  HhTab_HtIq_Az <-
    lapply(Hh_df_Az, function(x) table(x$HouseType, x$IncQ)[Ht,Iq])
  
  #Tabulate housing unit inputs by Bzone and housing type
  #------------------------------------------------------
  InitUnits_BzHt <-
    as.matrix(data.frame(L$Year$Bzone[c("SFDU", "MFDU")]))
  rownames(InitUnits_BzHt) <- L$Year$Bzone$Bzone
  colnames(InitUnits_BzHt) <- Ht
  
  #Tabulate input assumptions of household income distribution for each Bzone
  #--------------------------------------------------------------------------
  #Extract matrix of input assumptions of Bzone unit proportions by income
  HhIqProp_BzIq <-
    cbind(IncQ1 = L$Year$Bzone$HhPropIncQ1,
          IncQ2 = L$Year$Bzone$HhPropIncQ2,
          IncQ3 = L$Year$Bzone$HhPropIncQ3,
          IncQ4 = L$Year$Bzone$HhPropIncQ4)
  Bz <- L$Year$Bzone$Bzone
  rownames(HhIqProp_BzIq) <- Bz
  #Make sure that rows add to 1
  HhIqProp_BzIq <- t(apply(HhIqProp_BzIq, 1, function(x) {
    sumx <- sum(x)
    if ( sumx > 0 ) x / sumx else rep(1/length(x),length(x))
  }))
  
  #Balance housing units with housing demand and assign households to locations
  #----------------------------------------------------------------------------
  #Each Azone is a housing market. The number of housing units by type and
  #income quartile for Bzones in the Azone is balanced with the number of
  #households by house type and income quartile. Iterative proportional fitting
  #(IPF) is used to balance housing units over 3 dimensions: Bzone, unit type,
  #and income quartile. Two matrixes are used as margin control totals for the
  #balancing process. The first is a matrix of demand by housing type and income
  #quartile which is calculated above by Azone (HhTab_HtIq_Az). The second is a
  #matrix of units by Bzone and housing type. This matrix is created by scaling
  #the number of input units by Bzone and housing type to match the demand by
  #housing type. Scaled values are converted to whole numbers in the process.
  #The seed matrix for the IPF uses the input assumptions for proportion of
  #households by type (bzone_hh_inc_qrtl_prop.csv input file) to arrive at a
  #balanced distribution of households by income for each Bzone that reflects
  #household income differences among Bzones. After units are allocated to
  #Bzones to match the number of units demanded by households, each household is
  #assigned to a Bzone. This is done by iterating through each housing type and
  #income quartile combination and:
  #1) Extract the vector of units by Bzone for the type/quartile combination,
  #2) Using the vector as replication weights to replicate the Bzone names
  #3) Randomize the Bzone name vector
  #4) Assign the randomized Bzone name vector to households matching the
  #type/quartile combination.
  #Allocate households to Bzones
  #Create vector of household assignments to Bzones
  Bzone_Hh <- character(length(L$Year$Household$HhId))
  names(Bzone_Hh) <- L$Year$Household$HhId
  #Assign households to Bzones by Azone
  for (az in Az) {
    #Opt out of IPF if only one Bzone in the Azone
    #---------------------------------------------
    #Identify Bzones located in the Azone
    Bx <- L$Year$Bzone$Bzone[L$Year$Bzone$Azone == az]
    #If only one Bzone then all Azone households are in the Bzone
    if (length(Bx) == 1) {
      writeLog( paste("Predict Housing for one Bzone in Azone",az), Level="warn" )
      Hh_df_Az[[az]]$Bzone <- rep(Bx, nrow(Hh_df_Az[[az]]))
      #Put results in Bzone_Hh
      Bzone_Hx <- Hh_df_Az[[az]]$Bzone
      names(Bzone_Hx) <- Hh_df_Az[[az]]$HhId
      Bzone_Hh[names(Bzone_Hx)] <- Bzone_Hx
      rm(Bzone_Hx)
    } else {
      writeLog( paste("Predict Housing for",length(Bx),"Bzones in Azone",az), Level="info" )
      #Create matrices of margin totals
      #--------------------------------
      #Extract the unit demand by type and income quartile for households in Azone
      UnitDemand_HtIq <- HhTab_HtIq_Az[[az]]
      UnitDemand_Ht <- rowSums(UnitDemand_HtIq)
      #Extract the initial number of housing units by type for Bzones in Azone
      InitUnits_BxHt <- InitUnits_BzHt[Bx,]
      #Calculate the initial Bzone proportions of units for each type
      BxPropUnits_BxHt <- sweep(InitUnits_BxHt, 2, colSums(InitUnits_BxHt), "/")
      #Calculate matrix of unit demand by Bzone and type
      UnitDemand_BxHt <- sweep(BxPropUnits_BxHt, 2, UnitDemand_Ht, "*")
      #Convert to whole numbers
      UnitDemand_BxHt <- round(UnitDemand_BxHt)
      UnitDiff_Ht <- UnitDemand_Ht - colSums(UnitDemand_BxHt)
      for (i in 1:2) {
        UnitDiff_By <- table(
          sample(Bx, abs(UnitDiff_Ht[i]), replace = TRUE, prob = BxPropUnits_BxHt[,i]))
        UnitDemand_BxHt[names(UnitDiff_By), i] <-
          UnitDemand_BxHt[names(UnitDiff_By), i] + sign(UnitDiff_Ht[i]) * UnitDiff_By
        rm(UnitDiff_By)
      }
      rm(i, BxPropUnits_BxHt)
      
      #Create seed array for IPF balancing of units by Bzone, type, and income
      #-----------------------------------------------------------------------
      HhIqProp_BxIq <- HhIqProp_BzIq[Bx,]
      Seed_BxHtIq <-
        array(1, dim = c(length(Bx), length(Ht), length(Iq)), dimnames = list(Bx,Ht,Iq))
      for (bx in Bx) {
        Seed_BxHtIq[bx,,] <- outer(UnitDemand_BxHt[bx,], HhIqProp_BxIq[bx,])
      }
      Seed_BxHtIq[Seed_BxHtIq == 0] <- 1e-6
      
      #Balance unit demand for each Bzone by unit type and income quartile
      #-------------------------------------------------------------------
      #Use IPF to allocate unit demand to Bzones, unit types, and income quartile
      Ipf_ls <-
        ipf(Seed_BxHtIq,
            MrgnVals_ls = list(UnitDemand_BxHt, UnitDemand_HtIq),
            MrgnDims_ls = list(c(1,2), c(2,3)),
            RmseTarget=1.4e-5, MaxIter = 100
        )
      Units_BxHtIq <- Ipf_ls$Units_ar
      if (Ipf_ls$NumIter == Ipf_ls$MaxIter) {
        Msg <-
          paste0("Warning for PredictHousing module. ",
                 "Balancing of housing units by Bzone, housing type,",
                 "and income quartile in Azone ", az,
                 " went to maximum number of iterations (", Ipf_ls$MaxIter,
                 ") without achieving RMSE criterion for margin control totals. ",
                 " RMSE error achieved was ", Ipf_ls$RmseErr, ".")
        writeLog(Msg,Level="warn")
        rm(Msg)
      } else if ( Ipf_ls$RmseErr > 1e-5 ) {
        writeLog(paste("Undesirable RmseErr:",Ipf_ls$RmseErr,"after",Ipf_ls$MaxIter,"iterations in Azone",az),Level="info")
      } else {
        writeLog(paste("Total Iterations:",Ipf_ls$NumIter),Level="info")
      }
      rm(Seed_BxHtIq, UnitDemand_BxHt, Ipf_ls)
      #Convert allocation to whole numbers
      Units_BxHtIq <- round(Units_BxHtIq)
      Units_HtIq <- apply(Units_BxHtIq, c(2,3), sum, na.rm=TRUE)
      UnitDiff_HtIq <-  UnitDemand_HtIq - Units_HtIq
      BxPropUnits_BxHtIq <- sweep(Units_BxHtIq, c(2,3), Units_HtIq, "/")
      BxPropUnits_BxHtIq[is.na(BxPropUnits_BxHtIq)] <- 0
      for (ht in Ht) {
        for (iq in Iq) {
          if(sum(BxPropUnits_BxHtIq[,ht,iq])>0){
          UnitDiff_By <- table(
            sample(Bx, abs(UnitDiff_HtIq[ht,iq]), replace = TRUE, prob = BxPropUnits_BxHtIq[,ht,iq]))
          } else {
            UnitDiff_By <- table(
              sample(Bx, abs(UnitDiff_HtIq[ht,iq]), replace = TRUE))
          }
          Units_BxHtIq[names(UnitDiff_By),ht,iq] <-
            Units_BxHtIq[names(UnitDiff_By),ht,iq] + sign(UnitDiff_HtIq[ht,iq]) * UnitDiff_By
          rm(UnitDiff_By)
        }
      }
      rm(UnitDiff_HtIq, BxPropUnits_BxHtIq, ht, iq)
      #Assign Bzones to households based on housing type and income quartile
      #---------------------------------------------------------------------
      Hh_df_Az$Bzone <- ""
      for (ht in Ht) {
        for (iq in Iq) {
          Bzone_ <- sample(rep(Bx, Units_BxHtIq[,ht,iq]))
          IsHh_ <-
            with(Hh_df_Az[[az]], HouseType == ht & IncQ == iq)
          Hh_df_Az[[az]]$Bzone[IsHh_] <- Bzone_
          rm(Bzone_, IsHh_)
        }
      }
      #Put results in Bzone_Hh
      Bzone_Hx <- Hh_df_Az[[az]]$Bzone
      names(Bzone_Hx) <- Hh_df_Az[[az]]$HhId
      Bzone_Hh[names(Bzone_Hx)] <- Bzone_Hx
      rm(Bzone_Hx)
    }
  }
  
  writeLog("Predict Housing Assigning Group Quarters",Level="info")
  
  #Assign group quarters households to Bzones
  #------------------------------------------
  #Iterate through Azones to assign Bzones
  for (az in Az) {
    #Get inventory of group quarters units by Bzone
    GQUnits_Bx <- L$Year$Bzone$GQDU[L$Year$Bzone$Azone == az]
    Bx <- L$Year$Bzone$Bzone[L$Year$Bzone$Azone == az]
    names(GQUnits_Bx) <- Bx
    #Calculate demand
    GQUnitDemand <- sum(IsGQ_Hh[L$Year$Household$Azone == az])
    #Continue calculating if any GQ demand
    if (GQUnitDemand >= 1) {
      writeLog(paste("GQUnitDemand in Azone",az,"is",GQUnitDemand),Level="info")
      #If only one Bzone then all GQ households are in that Bzone
      if (length(Bx) == 1) {
        Bzone_Hx <- rep(Bx, GQUnitDemand)
        names(Bzone_Hx) <-
          L$Year$Household$HhId[IsGQ_Hh & L$Year$Household$Azone == az]
        Bzone_Hh[names(Bzone_Hx)] <- Bzone_Hx
      } else {
        #Scale Bzone demand to match overall demand
        if ( sum(GQUnits_Bx)<=0 ) {
          writeLog(paste("GQUnitDemand",GQUnitDemand,"but no GQUnits_Bx in Azone",az,"- adding to first Bzone"),Level="error")
          GQUnits_Bx[1] <- GQUnitDemand # Put them all in fhe first Bzone
        }
        GQUnitDemand_Bx <- round(GQUnitDemand * GQUnits_Bx / sum(GQUnits_Bx))
        UnitDiff <- GQUnitDemand - sum(GQUnitDemand_Bx)
        UnitDiff_By <-
          table(sample(Bx, abs(UnitDiff), replace = TRUE,
                       prob = GQUnitDemand_Bx / sum(GQUnitDemand_Bx)))
        GQUnitDemand_Bx[names(UnitDiff_By)] <-
          GQUnitDemand_Bx[names(UnitDiff_By)] + UnitDiff_By * sign(UnitDiff)
        #Assign group quarters units in Bzones to group quarters households
        Bzone_Hx <- sample(rep(Bx, GQUnitDemand_Bx))
        names(Bzone_Hx) <-
          L$Year$Household$HhId[IsGQ_Hh & L$Year$Household$Azone == az]
        Bzone_Hh[names(Bzone_Hx)] <- Bzone_Hx
      }
    } else writeLog(paste("No GQUnitDemand in Azone",az),Level="warn")
  }
  
  #Tabulate households, population, workers, and units by Bzone
  #------------------------------------------------------------
  Bz <- L$Year$Bzone$Bzone
  Bz_list <- setNames(rep(0,length(Bz)),Bz)
  NumHh_Bz <- tapply(Bzone_Hh, Bzone_Hh, length)
  t <- match(names(NumHh_Bz),names(Bz_list))
  Bz_list[t] <- NumHh_Bz 
  NumHh_Bz <- Bz_list
  Bz_list <- setNames(rep(0,length(Bz)),Bz)
  Pop_Bz <- tapply(L$Year$Household$HhSize, Bzone_Hh, sum)
  t <- match(names(Pop_Bz),names(Bz_list))
  Bz_list[t] <- Pop_Bz
  Pop_Bz <- Bz_list  
  Bz_list <- setNames(rep(0,length(Bz)),Bz)  
  NumWkr_Bz <- tapply(L$Year$Household$Workers, Bzone_Hh, sum)
  t <- match(names(NumWkr_Bz),names(Bz_list))
  Bz_list[t] <- NumWkr_Bz
  NumWkr_Bz <- Bz_list
  Bz_list <- setNames(rep(0,length(Bz)),Bz)  
  SF_Bz <- tapply(HouseType_Hh == "SF", Bzone_Hh, sum)
  t <- match(names(SF_Bz),names(Bz_list))
  Bz_list[t] <- SF_Bz 
  SF_Bz  <- Bz_list
  Bz_list <- setNames(rep(0,length(Bz)),Bz)  
  MF_Bz <- tapply(HouseType_Hh == "MF", Bzone_Hh, sum)
  t <- match(names(MF_Bz),names(Bz_list))
  Bz_list[t] <- MF_Bz 
  MF_Bz  <- Bz_list
  Bz_list <- setNames(rep(0,length(Bz)),Bz)  
  GQ_Bz <- tapply(HouseType_Hh == "GQ", Bzone_Hh, sum)
  t <- match(names(GQ_Bz),names(Bz_list))
  Bz_list[t] <- GQ_Bz 
  GQ_Bz  <- Bz_list
  
  #Return list of results
  #----------------------
  #Initialize output list
  Out_ls <- initDataList()
  #Add the household Bzone assignments to the list
  Out_ls$Year$Household$Bzone <- unname(Bzone_Hh)
  #Add SIZE attribute for the household Bzone assignments
  attributes(Out_ls$Year$Household$Bzone)$SIZE <- max(nchar(Bzone_Hh))
  #Add the household housing type assignments to the list
  Out_ls$Year$Household$HouseType <- unname(HouseType_Hh)
  #Add the dwelling unit demand numbers by Bzone
  Out_ls$Year$Bzone$SF <- as.integer(unname(SF_Bz))
  Out_ls$Year$Bzone$MF <- as.integer(unname(MF_Bz))
  Out_ls$Year$Bzone$GQ <- as.integer(unname(GQ_Bz))
  #Add the population, households, and workers by Bzone
  Out_ls$Year$Bzone$Pop <- as.integer(unname(Pop_Bz))
  Out_ls$Year$Bzone$NumHh <- as.integer(unname(NumHh_Bz))
  Out_ls$Year$Bzone$NumWkr <- as.integer(unname(NumWkr_Bz))
  #Return the outputs list
  Out_ls
}

### Debug

[Ht,Iq]

HhTab_HtIq_Az_test <-
  lapply(Hh_df_Az, function(x) table(x$HouseType, x$IncQ))

table(Hh_df_Az[["205"]]$HouseType, Hh_df_Az[["205"]]$IncQ)[Ht,Iq]

Hh_df_Az_dt <- rbindlist(Hh_df_Az, idcol = "Azone")
Hh_df_Az_dt_sum <- Hh_df_Az_dt[,.N, keyby = .(HouseType, Azone)]
dcast.data.table(Hh_df_Az_dt_sum[,.(HouseType, HH = N, Azone = as.integer((Azone)))],
                 Azone ~ HouseType, fun.aggregate = sum, value.var = "HH")[MF == 0]

Az[!Az %in% Hh_df_Az_dt_sum[HouseType == "MF"]$Azone]

table(Hh_df_Az[["109"]]$HouseType, Hh_df_Az[["109"]]$IncQ)[Ht,Iq]
Hh_df_Az[["109"]]

table(Hh_df_Az[["267"]]$HouseType, Hh_df_Az[["267"]]$IncQ)[Ht,Iq]
Hh_df_Az[["267"]]

table(Hh_df_Az[["271"]]$HouseType, Hh_df_Az[["271"]]$IncQ)[Ht,Iq]
Hh_df_Az[["271"]]

table(Hh_df_Az[["281"]]$HouseType, Hh_df_Az[["281"]]$IncQ)[Ht,Iq]
Hh_df_Az[["267"]]

table(Hh_df_Az[["257"]]$HouseType, Hh_df_Az[["257"]]$IncQ)[Ht,Iq]
Hh_df_Az[["257"]]

scen_bz[Azone == "109"]
scen_bz[Azone == "267"]

scen_bz[Azone == "271"]
scen_bz[Azone == "281"]

scen_bz[Azone == "257"]

# Test: bump up the number of MFDU in a azone 109 bzone to a few
scen_bz[Azone == "109" & Bzone == "362", MFDU := 5]

scen_bz[Azone == "221"]
table(Hh_df_Az[["221"]]$HouseType, Hh_df_Az[["205"]]$IncQ)[Ht,Iq]
hhtargets <- scen_bz[,.(MFDU = sum(MFDU), SFDU = sum(SFDU)), key = .(Azone = as.integer(Azone))][, pctMFDU := MFDU/(SFDU + MFDU)]
hhalloc <- dcast.data.table(Hh_df_Az_dt_sum[,.(HouseType, HH = N, Azone = as.integer((Azone)))],
                            Azone ~ HouseType, fun.aggregate = sum, value.var = "HH")

hhtargets[hhalloc, c("MFHH", "SFHH") := .(i.MF, i.SF), on = "Azone"]

hhtargets[order(MFHH)][1:50]
fwrite(hhtargets[order(MFHH)], "E:/Projects/Clients/SJV_VE/_debug/azone_du_hh_by_sf_mf.csv")


Hh_df_Az_dt_inc_sum <- Hh_df_Az_dt[,.N, keyby = .(HouseType, IncQ, Azone)]
Hh_df_Az_dt_inc_xt <- dcast.data.table(Hh_df_Az_dt_inc_sum[,.(HouseType, IncQ, HH = N, Azone = as.integer((Azone)))],
                 Azone ~ HouseType + IncQ, fun.aggregate = sum, value.var = "HH")
Hh_df_Az_dt_inc_xt[MF_IncQ1 ==0 | MF_IncQ2 == 0 | MF_IncQ3 == 0 | MF_IncQ4 == 0]
Hh_df_Az_dt_inc_xt[SF_IncQ1 ==0 | SF_IncQ2 == 0 | SF_IncQ3 == 0 | SF_IncQ4 == 0]
Hh_df_Az_dt_inc_xt[Azone == 10]

scen_bz[,.(Azone, Bzone, MFDU, SFDU, HhPropIncQ1, HhPropIncQ2, HhPropIncQ3, HhPropIncQ4)][HhPropIncQ1 == 0 | HhPropIncQ2 == 0 | HhPropIncQ3 == 0 | HhPropIncQ4 == 0][order(Azone, Bzone)]
scen_bz[,.(Azone, Bzone, MFDU, SFDU, HhPropIncQ1, HhPropIncQ2, HhPropIncQ3, HhPropIncQ4)][Azone %in% 100:104][order(Azone, Bzone)]