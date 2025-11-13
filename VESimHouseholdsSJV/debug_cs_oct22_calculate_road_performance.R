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
library(ordinal)

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
    tblfiles <- tblfiles[tblfiles != "OthSpd.Rda"] ### currently an issue with this output
    tblfiles_noext <- gsub(".Rda","", tblfiles)
    
    for(i in 1:length(tblfiles)){
      load(file.path(scenarios[scen_num], output_tabs[tbl], tblfiles[i]))  
      assign(tblfiles_noext[i], Dataset)
    }
    
    assign(outputs_vars[tbl], data.table(sapply(tblfiles_noext, get)))  
    if(output_tabs[tbl]=="Region") get(outputs_vars[tbl])[,parameter := tblfiles_noext]
  }
  
  scen_list[[model_names[scen_num]]] <- lapply(outputs_vars, get)
  names(scen_list[[model_names[scen_num]]]) <- outputs_vars
}

# create combined tables across scenarios
scen_hh <- lapply(scen_list, "[[", "HouseholdVars")
scen_hh <- rbindlist(scen_hh, idcol = "scenario")
str(scen_hh)
cols <- c("HhSize", "Income", "Workers", "Drivers", "IsUrbanMixNbrhd")
scen_hh[,  (cols) := lapply(.SD, as.numeric), .SDcols = cols]

scen_veh <- lapply(scen_list, "[[", "VehicleVars")
scen_veh <- rbindlist(scen_veh, idcol = "scenario")

scen_wrk <- lapply(scen_list, "[[", "WorkerVars")
scen_wrk <- rbindlist(scen_wrk, idcol = "scenario")

scen_bz <- lapply(scen_list, "[[", "BzoneVars")
scen_bz <- rbindlist(scen_bz, idcol = "scenario")

str(scen_bz)
cols <- c("SFDU", "MFDU", "HhPropIncQ1", "HhPropIncQ2", "HhPropIncQ3", "HhPropIncQ4", "GQDU", "D1B")
scen_bz[,  (cols) := lapply(.SD, as.numeric), .SDcols = cols]

scen_az <- lapply(scen_list, "[[", "AzoneVars")
scen_az <- rbindlist(scen_az, idcol = "scenario")

scen_ma <- lapply(scen_list, "[[", "MareaVars")
scen_ma <- rbindlist(scen_ma, idcol = "scenario")

str(scen_ma)
names(scen_ma)
cols <- c("TranRevMiPC", "UrbanPop", "FwyLaneMi", "ArtLaneMi", "LdvAVLvl3Prop", "LdvAVLvl5Prop", "HvyTrkDriverlessProp", "BusDriverlessProp",
          "LdvFwyArtDvmt", "LdvOthDvmt", "HvyTrkFwyDvmt", "HvyTrkArtDvmt", "HvyTrkOthDvmt", "BusFwyDvmt", "BusArtDvmt", "BusOthDvmt",
          "RampMeterDeployProp",
          "IncidentMgtDeployProp",
          "SignalCoordDeployProp",
          "AccessMgtDeployProp",
          "OtherFwyOpsDeployProp",
          "OtherArtOpsDeployProp", 
          "FwyNoneCongChg", "FwyModCongChg", "FwyHvyCongChg", "FwySevCongChg", "FwyExtCongChg",
          "ArtNoneCongChg", "ArtModCongChg", "ArtHvyCongChg", "ArtSevCongChg", "ArtExtCongChg")
scen_ma[,  (cols) := lapply(.SD, as.numeric), .SDcols = cols]

scen_reg <- lapply(scen_list, "[[", "RegionVars")
scen_reg <- rbindlist(scen_reg, idcol = "scenario")
scen_reg <- dcast.data.table(scen_reg,
                             scenario~parameter,
                             fun.aggregate = sum,
                             value.var = "V1")

# Import the globals
globals <- file.path(project_dir, model_names, "results/Datastore/Global") 
globals_list <- list()

# cycle through the scenarios 
for(scen_num in 1:length(globals)){
  #scen_num <- 1
  
  output_tabs <- list.files(globals[scen_num])
  outputs_vars <- paste0(output_tabs, "Vars")
  
  for(tbl in 1:length(output_tabs)){
    
    tblfiles <- list.files(file.path(globals[scen_num], output_tabs[tbl]))
    tblfiles_noext <- gsub(".Rda","", tblfiles)
    
    for(i in 1:length(tblfiles)){
      load(file.path(globals[scen_num], output_tabs[tbl], tblfiles[i]))  
      assign(tblfiles_noext[i], Dataset)
    }
    
    assign(outputs_vars[tbl], data.table(sapply(tblfiles_noext, get)))  
    #if(output_tabs[tbl]=="Region") get(outputs_vars[tbl])[,parameter := tblfiles_noext]
  }
  
  globals_list[[model_names[scen_num]]] <- lapply(outputs_vars, get)
  names(globals_list[[model_names[scen_num]]]) <- outputs_vars
}


cols <- c("LdvFwyDvmtProp", "LdvArtDvmtProp")
globals_list$`VERSPM-SJV-base`$MareaVars[,  (cols) := lapply(.SD, as.numeric), .SDcols = cols]

cols <- c("Art_NonRcr","Art_Rcr", "Fwy_NonRcr", "Fwy_Rcr")
globals_list$`VERSPM-SJV-base`$AVLev5EffectivenessVars[,  (cols) := lapply(.SD, as.numeric), .SDcols = cols]

cols <- c("Beta")
globals_list$`VERSPM-SJV-base`$AVLvl5EffectAdjParamVars[,  (cols) := lapply(.SD, as.numeric), .SDcols = cols]
          
# assemble into a list 
L <- list()
L$Year <- list(Household = as.data.frame(scen_hh),
               Bzone = as.data.frame(scen_bz),
               Azone = as.data.frame(scen_az),
               Marea = as.data.frame(scen_ma),
               Region = as.data.frame(scen_reg))

L$Global$AVLev5Effectiveness <- globals_list$`VERSPM-SJV-base`$AVLev5EffectivenessVars
L$Global$AVLvl5EffectAdjParam  <- globals_list$`VERSPM-SJV-base`$AVLvl5EffectAdjParamVars
L$Global$AVCapacityFactors <- globals_list$`VERSPM-SJV-base`$AVCapacityFactorsVars
L$Global$Model$ValueOfTime <- unlist(globals_list$`VERSPM-SJV-base`$ModelVars)
L$Global$Marea <- globals_list$`VERSPM-SJV-base`$MareaVars

L$G$Year <- the_year
L$G$BaseYear <- the_year


defs_folder <- "E:/Projects/Clients/SJV_VE/Model_runs/VisionEval-Dev/built/visioneval/4.3.3/runtime/models/VERSPM-SJV-base/defs"
geo <- read.csv(file.path(defs_folder, "geo.csv"))

geo[geo$Marea == "Mariposa",]
scen_az[Azone %in% geo[geo$Marea == "Mariposa","Azone"]]
tail(scen_az[,.(Azone, Marea)], 20)
scen_bz[Bzone %in% geo[geo$Marea == "Mariposa","Bzone"]]
scen_bz[,.(UrbanPop = sum(as.numeric(UrbanPop))), keyby = Marea]
scen_ma[,.(Marea, FwyLaneMi, FwyLaneMiPC)]
scen_bz[Bzone %in% geo[geo$Marea == "Amador","Bzone"]]
scen_bz[Bzone %in% geo[geo$Marea == "Calaveras","Bzone"]]
scen_ma[,.(Marea, LdvAVLvl5Prop)]
scen_hh[Marea == "Mariposa",]
scen_ma[Marea == "Mariposa",]
scen_ma[UrbanPop == 0,]
scen_bz[,.(UrbanArea = sum(as.numeric(UrbanArea))), keyby = Marea]

#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================

calculateCongestion <-
  function(RoadType, LaneMi, DVMT, AVAdjFactor = 1) {
    #Extract the lookup table
    CongestedProportions_ls <- loadPackageDataset("CongestedProportions_ls","VETravelPerformance")
    Lookup_LvCl <-
      CongestedProportions_ls[[RoadType]]
    #Calculate roadway demand
    Demand <- DVMT / (LaneMi * AVAdjFactor)
    #Constrain the lookup to the table range
    Demand_ <- as.numeric(rownames(Lookup_LvCl))
    if (Demand < min(Demand_)) Demand <- min(Demand_)
    if (Demand > max(Demand_)) Demand <- max(Demand_)
    #Extract the values
    CongProps_ <- apply(Lookup_LvCl, 2, function(x) {
      approx(Demand_, x, Demand)$y
    })
    #Make sure values add exactly to 1
    CongProps_ <- CongProps_ / sum(CongProps_)
    #Split DVMT into congested components
    DVMT * CongProps_
  }


# OpsDeployment_ <- OpsDeployment_MaOp[ma,]
# OtherOpsEffects_mx <- AVLev5Effects_mx

calculateSpeeds <- function(OpsDeployment_, OtherOpsEffects_mx = NULL,
                            AVLvl5DvmtProp_Rc = NULL,
                            AVLvl5Factor_ls = NULL) {
  #Calculate recurring and non-recurring (incident-related) delay
  BaseSpeeds_df <- loadPackageDataset("BaseSpeeds_df","VETravelPerformance")
  BaseTravelRate_mx <- 1 / as.matrix(BaseSpeeds_df)
  Delay_mx <-
    sweep(BaseTravelRate_mx, 2, BaseTravelRate_mx[1,], "-")
  BaseDelay_mx <- cbind(
    Fwy_Rcr = Delay_mx[,"Fwy_Rcr"],
    Fwy_NonRcr = Delay_mx[,"Fwy"] - Delay_mx[,"Fwy_Rcr"],
    Art_Rcr = Delay_mx[,"Art_Rcr"],
    Art_NonRcr = Delay_mx[,"Art"] - Delay_mx[,"Art_Rcr"]
  )
  rm(Delay_mx)
  #Calculate operations management delay reduction effects
  Ramp_df <- loadPackageDataset("Ramp_df","VETravelPerformance")
  Incident_df <- loadPackageDataset("Incident_df","VETravelPerformance")
  Signal_df <- loadPackageDataset("Signal_df","VETravelPerformance")
  Access_df <- loadPackageDataset("Access_df","VETravelPerformance")
  RampFactor_mx <-
    1 - OpsDeployment_["RampMeterDeployProp"] * as.matrix(Ramp_df) / 100
  IncidentFactor_mx <-
    1 - OpsDeployment_["IncidentMgtDeployProp"] * as.matrix(Incident_df) / 100
  SignalFactor_mx <-
    1 - OpsDeployment_["SignalCoordDeployProp"] * as.matrix(Signal_df) / 100
  AccessFactor_mx <-
    1 - OpsDeployment_["AccessMgtDeployProp"] * as.matrix(Access_df) / 100
  #Calculate joint effect
  DelayFactor_mx <-
    RampFactor_mx * IncidentFactor_mx * SignalFactor_mx * AccessFactor_mx
  
  AVLvl5Factor_ls <- unlist(AVLvl5Factor_ls, recursive = TRUE,
                            use.names = TRUE)
  names(AVLvl5Factor_ls) <- gsub("\\.Delay\\.","_",names(AVLvl5Factor_ls))
  if (!is.null(OtherOpsEffects_mx)) {
    Ty <- colnames(OtherOpsEffects_mx)
    if (!is.null(AVLvl5DvmtProp_Rc)) {
      AVLvl5DvmtProp_Ty <- setNames(numeric(length(Ty)), Ty)
      AVLvl5DvmtProp_Ty[c("Fwy_Rcr", "Fwy_NonRcr")] <-
        AVLvl5DvmtProp_Rc["Fwy"]
      AVLvl5DvmtProp_Ty[c("Art_Rcr", "Art_NonRcr")] <-
        AVLvl5DvmtProp_Rc["Art"]
      OtherOpsDeploy_Ty <- sapply(Ty, function(x) {
        AVLvl5Factor_ls[[x]](AVLvl5DvmtProp_Ty[x])})
      OtherOpsFactor_mx <-
        1 - sweep(OtherOpsEffects_mx, 2, OtherOpsDeploy_Ty, "*") / 100
      #Select the maximum reduction. This is the minimum factor.
      # MaxFactor_mx <- pmax(DelayFactor_mx, OtherOpsFactor_mx)
      DelayFactor_mx <- pmin(DelayFactor_mx, OtherOpsFactor_mx)
      # DelayFactor_mx[DelayFactor_mx==1] <- MaxFactor_mx[DelayFactor_mx==1]
    } else {
      OtherOpsDeploy_Ty <- setNames(numeric(length(Ty)), Ty)
      OtherOpsDeploy_Ty[c("Fwy_Rcr", "Fwy_NonRcr")] <-
        OpsDeployment_["OtherFwyOpsDeployProp"]
      OtherOpsDeploy_Ty[c("Art_Rcr", "Art_NonRcr")] <-
        OpsDeployment_["OtherArtOpsDeployProp"]
      OtherOpsFactor_mx <-
        1 - sweep(OtherOpsEffects_mx, 2, OtherOpsDeploy_Ty, "*") / 100
      DelayFactor_mx <- DelayFactor_mx * OtherOpsFactor_mx
    }
  }
  
  #Calculate adjusted delay
  AdjDelay_mx <- BaseDelay_mx * DelayFactor_mx
  Delay_mx <- cbind(
    Fwy = rowSums(AdjDelay_mx[,c("Fwy_Rcr", "Fwy_NonRcr")]),
    Art = rowSums(AdjDelay_mx[,c("Art_Rcr", "Art_NonRcr")])
  )
  #Calculate adjusted speed
  AdjTravelRate_mx <-
    sweep(Delay_mx, 2, BaseTravelRate_mx["None",c("Fwy", "Art")], "+")
  Speed_mx <- 1 / AdjTravelRate_mx
  #Return the results
  list(
    Speed = Speed_mx,
    Delay = Delay_mx
  )
}



#' Calculate road performance measures
CalculateRoadPerformance <- function(L) {
  
  #Set up
  #------
  #Define naming vectors for Mareas and congestion levels
  Ma <- L$Year$Marea$Marea
  Cl <- c("None", "Mod", "Hvy", "Sev", "Ext")
  Vt <- c("Ldv", "HvyTrk", "Bus")
  Rc <- c("Fwy", "Art", "Oth")
  #Define default speed for other roads (not freeway or arterial)
  OthSpeed <- 20
  #Initialize outputs list
  Out_ls <- initDataList()
  Out_ls$Global$Marea <- list()
  Out_ls$Year$Marea <- list()
  #Function to remove attributes
  unattr <- function(X_) {
    attributes(X_) <- NULL
    X_
  }
  
  #Calculate Lambda value for metropolitan areas
  #---------------------------------------------
  #Calculate Lambda
  DvmtSplitData_df <- data.frame(
    LogPop = log1p(L$Year$Marea$UrbanPop),
    LnMiRatio = L$Year$Marea$FwyLaneMi / L$Year$Marea$ArtLaneMi
  )
  DvmtSplit_LM <- loadPackageDataset("DvmtSplit_LM","VETravelPerformance")
  Lambda_Ma <- unname(predict(DvmtSplit_LM, newdata = DvmtSplitData_df))
  names(Lambda_Ma) <- Ma
  Lambda_Ma[Ma == "None"] <- NA
  #Change Lambda values that are outside expected range (0.1 - 1.5)
  Lambda_Ma[Lambda_Ma < 0.1] <- 0.1
  Lambda_Ma[Lambda_Ma > 1.5] <- 1.5
  #Load Lambda adjustments if they exist
  if (!is.null(L$Global$Marea$LambdaAdj)) {
    LambdaAdj_Ma <- L$Global$Marea$LambdaAdj
    names(LambdaAdj_Ma) <- Ma
    #Also save to Out_ls so Set specifications are satisfied
    Out_ls$Global$Marea$LambdaAdj <- L$Global$Marea$LambdaAdj
  }
  rm(DvmtSplitData_df)
  
  #Calculate speed and delay by congestion level and metropolitan area
  #-------------------------------------------------------------------
  #Create matrix of user-defined other operations effects
  if (!is.null(L$Global$AVLev5Effectiveness)) {
    AVLev5Effects_mx <- cbind(
      Fwy_Rcr = L$Global$AVLev5Effectiveness$Fwy_Rcr,
      Fwy_NonRcr = L$Global$AVLev5Effectiveness$Fwy_NonRcr,
      Art_Rcr = L$Global$AVLev5Effectiveness$Art_Rcr,
      Art_NonRcr = L$Global$AVLev5Effectiveness$Art_NonRcr
    )
    rownames(AVLev5Effects_mx) <- L$Global$AVLev5Effectiveness$Level
  } else {
    AVLev5Effects_mx <- array(
      0,
      dim = c(5, 4),
      dimnames = list(
        c("None", "Mod", "Hvy", "Sev", "Ext"),
        c("Art_Rcr", "Art_NonRcr", "Fwy_Rcr", "Fwy_NonRcr")
      ))
  }
  OpsDeployNames_ <- c(
    "RampMeterDeployProp",
    "IncidentMgtDeployProp",
    "SignalCoordDeployProp",
    "AccessMgtDeployProp",
    "OtherFwyOpsDeployProp",
    "OtherArtOpsDeployProp")
  OpsDeployment_MaOp <- do.call(cbind, L$Year$Marea[OpsDeployNames_])
  rownames(OpsDeployment_MaOp) <- Ma
  #Create matrix of driverless DVMT proportions by Marea and vehicle type
  AVLvl5DvmtProp_MaTy <- cbind(
    Ldv = L$Year$Marea$LdvAVLvl5Prop,
    HvyTrk = L$Year$Marea$HvyTrkDriverlessProp,
    Bus = L$Year$Marea$BusDriverlessProp
  )
  rownames(AVLvl5DvmtProp_MaTy) <- L$Year$Marea$Marea
  
  
  #Calculate speed and delay by Marea and congestion level
  AVLvl5AdjParam_ <- unattr(L$Global$AVLvl5EffectAdjParam$Beta)
  names(AVLvl5AdjParam_) <- L$Global$AVLvl5EffectAdjParam$Measure
  effectAdj <- function(AdjBeta, DvmtProp = NULL){
    function(DvmtProp){
      (DvmtProp ^ AdjBeta)
    }
  }
  AVLvl5Factor_ls <- list(
    Art = list(
      Delay = list(Rcr = effectAdj(AdjBeta = AVLvl5AdjParam_["ArtRcrDelay"]),
                   NonRcr = effectAdj(AdjBeta = AVLvl5AdjParam_["ArtNonRcrDelay"])
      ),
      Smooth = effectAdj(AdjBeta = AVLvl5AdjParam_["ArtSmooth"])
    ),
    Fwy = list(
      Delay = list(Rcr = effectAdj(AdjBeta = AVLvl5AdjParam_["FwyRcrDelay"]),
                   NonRcr = effectAdj(AdjBeta = AVLvl5AdjParam_["FwyNonRcrDelay"])
      ),
      Smooth = effectAdj(AdjBeta = AVLvl5AdjParam_["FwySmooth"])
    )
  )
  
  #Calculate average driverless DVMT proportion by Marea and lane type
  #-------------------------------------------------------------------
  #Define function to calculate average driverless DVMT proportion
  calcAveAVLvl5DvmtProp <- function(LdvDvmt_Rc, LdvAVLvl5Prop,
                                    HvyTrkDvmt_Rc, HvyTrkDriverlessProp,
                                    BusDvmt_Rc, BusDriverlessProp) {
    Dvmt_TyRc <- rbind(LdvDvmt_Rc, HvyTrkDvmt_Rc, BusDvmt_Rc)
    DvmtPropByTy_TyRc <- sweep(Dvmt_TyRc, 2, colSums(Dvmt_TyRc), "/")
    AVLvl5Prop_Ty <- c(LdvAVLvl5Prop, HvyTrkDriverlessProp, BusDriverlessProp)
    colSums(sweep(DvmtPropByTy_TyRc, 1, AVLvl5Prop_Ty, "*"))
  }
  
  # Calculate capacity adjustment factors based on AV penetration
  AVCapacityFactors <- as.data.frame(L$Global$AVCapacityFactors)
  AvMpr_ma <- pmin(L$Year$Marea$LdvAVLvl5Prop + L$Year$Marea$LdvAVLvl3Prop,1)
  names(AvMpr_ma) <- L$Year$Marea$Marea
  
  FwyAdjFactors_ma <- numeric(length = length(Ma))
  names(FwyAdjFactors_ma) <- Ma
  ArtAdjFactors_ma <- numeric(length = length(Ma))
  names(ArtAdjFactors_ma) <- Ma
  
  for(ma in Ma){
    FwyAdjFactors_ma[ma] <- with(subset(AVCapacityFactors, Marea=ma),
                                 approx(MPR,FwyCapacityMultiplier,AvMpr_ma[ma])$y)
    ArtAdjFactors_ma[ma] <- with(subset(AVCapacityFactors, Marea=ma),
                                 approx(MPR,ArtCapacityMultiplier,AvMpr_ma[ma])$y)
  }
  
  #Make matrices of DVMT by metropolitan area, vehicle type and road class
  #-----------------------------------------------------------------------
  #Light-duty vehicle DVMT
  LdvDvmt_MaRx <-
    do.call(cbind, L$Year$Marea[c("LdvFwyArtDvmt", "LdvOthDvmt")])
  colnames(LdvDvmt_MaRx) <- c("FwyArt", "Oth")
  rownames(LdvDvmt_MaRx) <- Ma
  #Heavy-duty vehicle DVMT
  HvyTrkDvmt_MaRc <-
    do.call(cbind, L$Year$Marea[c("HvyTrkFwyDvmt", "HvyTrkArtDvmt", "HvyTrkOthDvmt")])
  colnames(HvyTrkDvmt_MaRc) <- c("Fwy", "Art", "Oth")
  rownames(HvyTrkDvmt_MaRc) <- Ma
  #Bus DVMT
  BusDvmt_MaRc <-
    do.call(cbind, L$Year$Marea[c("BusFwyDvmt", "BusArtDvmt", "BusOthDvmt")])
  colnames(BusDvmt_MaRc) <- c("Fwy", "Art", "Oth")
  rownames(BusDvmt_MaRc) <- Ma
  
  #Create matrices of freeway and arterial congestion prices, and VOT
  #------------------------------------------------------------------
  FwyPrices_MaCl <- do.call(cbind, L$Year$Marea[paste0("Fwy", Cl, "CongChg")])
  rownames(FwyPrices_MaCl) <- Ma
  ArtPrices_MaCl <- do.call(cbind, L$Year$Marea[paste0("Art", Cl, "CongChg")])
  rownames(ArtPrices_MaCl) <- Ma
  VOT <- L$Global$Model$ValueOfTime
  
  #Create matrix of freeway and arterial lane miles
  #------------------------------------------------
  LaneMi_MaRc <- do.call(cbind, L$Year$Marea[c("FwyLaneMi", "ArtLaneMi")])
  rownames(LaneMi_MaRc) <- Ma
  colnames(LaneMi_MaRc) <- c("Fwy", "Art")
  
  #Define function to calculate average speed including price adjustments
  #----------------------------------------------------------------------
  calcAveEqSpeed <- function(Dvmt_Cl, Speed_Cl, Price_Cl, VOT) {
    Dvht <- sum(Dvmt_Cl / Speed_Cl)
    DvhtEq <- sum(Dvmt_Cl * Price_Cl / VOT)
    sum(Dvmt_Cl) / (Dvht + DvhtEq)
  }
  
  #Define function to split freeway & arterial LDV DVMT
  #----------------------------------------------------
  splitLdvDvmt <- function(LdvDvmt_, AveFwySpd, AveArtSpd, Lambda, LambdaAdj) {
    DvmtRatio <- (Lambda + LambdaAdj) * AveFwySpd / AveArtSpd
    FwyDvmt <- unname(LdvDvmt_["FwyArt"] * DvmtRatio / (1 + DvmtRatio))
    ArtDvmt <- unname(LdvDvmt_["FwyArt"] - FwyDvmt)
    SplitLdvDvmt_ <- c(FwyDvmt, ArtDvmt, LdvDvmt_["Oth"])
    names(SplitLdvDvmt_) <- c("Fwy", "Art", "Oth")
    SplitLdvDvmt_
  }
  
  #Define function to balance freeway and arterial DVMT
  #----------------------------------------------------
  balanceFwyArtDvmt <- function(ma) {
    #Initialize values
    LastDvmtRatio <- 0
    FwyAveSpeed <- 60
    ArtAveSpeed <- 30
    #Iterate to find solution
    for (i in 1:100) {
      #Split LDV DVMT
      LdvDvmt_Rc <-
        splitLdvDvmt(LdvDvmt_MaRx[ma,], FwyAveSpeed, ArtAveSpeed, Lambda_Ma[ma], LambdaAdj_Ma[ma])
      #Add heavy truck and bus DVMT to calculate total
      Dvmt_Rc <-
        LdvDvmt_Rc + HvyTrkDvmt_MaRc[ma,] + BusDvmt_MaRc[ma,]
      #Calculate DVMT ratio, compare to last, and terminate if change is very small
      DvmtRatio <- Dvmt_Rc["Fwy"] / Dvmt_Rc["Art"]
      if(abs(1 - LastDvmtRatio / DvmtRatio) < 0.0001) break()
      LastDvmtRatio <- DvmtRatio
      #Split DVMT into congestion levels
      FwyDvmt_Cl <-
        calculateCongestion("Fwy", LaneMi_MaRc[ma,"Fwy"], Dvmt_Rc["Fwy"], FwyAdjFactors_ma[ma])
      ArtDvmt_Cl <-
        calculateCongestion("Art", LaneMi_MaRc[ma,"Art"], Dvmt_Rc["Art"], ArtAdjFactors_ma[ma])
      #Calculate average driverless DVMT proportion by road class
      AVLvl5DvmtProp_Rc <- calcAveAVLvl5DvmtProp(
        LdvDvmt_Rc = LdvDvmt_Rc[c("Fwy", "Art")],
        LdvAVLvl5Prop = AVLvl5DvmtProp_MaTy[ma, "Ldv"],
        HvyTrkDvmt_Rc = HvyTrkDvmt_MaRc[ma, c("Fwy", "Art")],
        HvyTrkDriverlessProp = AVLvl5DvmtProp_MaTy[ma, "HvyTrk"],
        BusDvmt_Rc = BusDvmt_MaRc[ma, c("Fwy", "Art")],
        BusDriverlessProp = AVLvl5DvmtProp_MaTy[ma, "Bus"])
      #Calculate speed by congestion level
      SpeedAndDelay_ls <-
        calculateSpeeds(OpsDeployment_MaOp[ma,], AVLev5Effects_mx, AVLvl5DvmtProp_Rc,
                        AVLvl5Factor_ls)
      #Convert to matrices
      FwySpeed_Cl <- SpeedAndDelay_ls$Speed[,"Fwy"]
      ArtSpeed_Cl <- SpeedAndDelay_ls$Speed[,"Art"]
      FwyDelay_Cl <- SpeedAndDelay_ls$Delay[,"Fwy"]
      ArtDelay_Cl <- SpeedAndDelay_ls$Delay[,"Art"]
      #Calculate equivalent average speed
      FwyAveSpeed <-
        calcAveEqSpeed(FwyDvmt_Cl, FwySpeed_Cl, FwyPrices_MaCl[ma,], VOT)
      ArtAveSpeed <-
        calcAveEqSpeed(ArtDvmt_Cl, ArtSpeed_Cl, ArtPrices_MaCl[ma,], VOT)
    }
    Dvmt_VtRc <- rbind(
      Ldv = LdvDvmt_Rc,
      HvyTrk = HvyTrkDvmt_MaRc[ma,],
      Bus = BusDvmt_MaRc[ma,]
    )
    rownames(Dvmt_VtRc) <- c("Ldv", "HvyTrk", "Bus")
    colnames(Dvmt_VtRc) <- c("Fwy", "Art", "Oth")
    list(Dvmt_VtRc = Dvmt_VtRc,
         FwyDvmt_Cl = FwyDvmt_Cl,
         ArtDvmt_Cl = ArtDvmt_Cl,
         SpeedAndDelay_ls = SpeedAndDelay_ls)
  }
  
  #If base year calculate LambdaAdj factor to match input ratio of freeway and
  #arterial DVMT
  #---------------------------------------------------------------------------
  if (L$G$Year == L$G$BaseYear) {
    #Initialize LambdaAdj
    LambdaAdj_Ma <- numeric(length(Ma))
    names(LambdaAdj_Ma) <- Ma
    if (any(names(LambdaAdj_Ma) == "None")) LambdaAdj_Ma["None"] <- NA
    #Calculate input ratio of freeway to arterial DVMT
    FwyArtTargetRatio_Ma <- with(L$Global$Marea, LdvFwyDvmtProp / LdvArtDvmtProp)
    names(FwyArtTargetRatio_Ma) <- Ma
    FwyArtTargetRatio_Ma["None"] <- NA
    #Define function to find LambdaAdj to match target ratio
    checkMatchLdvSplit <- function(Adj) {
      LambdaAdj_Ma[ma] <<- Adj
      FwyArtResults_ <- balanceFwyArtDvmt(ma)$Dvmt_VtRc["Ldv", 1:2]
      FwyArtRatio <- FwyArtResults_[1] / FwyArtResults_[2]
      FwyArtTargetRatio <- FwyArtTargetRatio_Ma[ma]
      FwyArtRatio - FwyArtTargetRatio
    }
    #Use binary search to find LambdaAdj factor for each marea
    for (ma in Ma[Ma != "None"]) {
      LambdaAdj_Ma[ma] <-
        binarySearch(checkMatchLdvSplit, c(-3, 3), DoWtAve = TRUE, Tolerance = 0.01)
    }
    #Add to outputs list
    Out_ls$Global$Marea$LambdaAdj <- unattr(LambdaAdj_Ma)
  }
  
  #Run model to balance DVMT between freeways and arterials
  #--------------------------------------------------------
  BalanceResults_ls <- sapply(Ma, function(x) list())
  for (ma in Ma[Ma != "None"]) {
    #Calculate the balanced DVMT
    BalanceResults_ls[[ma]] <- balanceFwyArtDvmt(ma)
  }
  if (any(Ma == "None")) {
    Vt <- c("Ldv", "HvyTrk", "Bus")
    Cl <- c("None", "Mod", "Hvy", "Sev", "Ext")
    Rc <- c("Fwy", "Art")
    ma <- "None"
    AVLvl5DvmtProp_Rc <- calcAveAVLvl5DvmtProp(
      LdvDvmt_Rc = array(0, dim = length(Rc), dimnames = list(Rc)),
      LdvAVLvl5Prop = AVLvl5DvmtProp_MaTy[ma, "Ldv"],
      HvyTrkDvmt_Rc = HvyTrkDvmt_MaRc[ma, Rc],
      HvyTrkDriverlessProp = AVLvl5DvmtProp_MaTy[ma, "HvyTrk"],
      BusDvmt_Rc = BusDvmt_MaRc[ma, c("Fwy", "Art")],
      BusDriverlessProp = AVLvl5DvmtProp_MaTy[ma, "Bus"])
    AVLvl5DvmtProp_Rc[is.na(AVLvl5DvmtProp_Rc)] <- 0
    BalanceResults_ls$None <- list(
      Dvmt_VtRc = array(0, dim = c(length(Vt), length(Rc)), dimnames = list(Vt,Rc)),
      FwyDvmt_Cl = setNames(numeric(length(Cl)), Cl),
      ArtDvmt_Cl = setNames(numeric(length(Cl)), Cl),
      SpeedAndDelay_ls = calculateSpeeds(OpsDeployment_MaOp[ma,], 
                                         AVLev5Effects_mx, AVLvl5DvmtProp_Rc,
                                         AVLvl5Factor_ls)
    )
  }
  
  #Calculate average vehicle speed by metropolitan area and road class
  #-------------------------------------------------------------------
  #Define function to calculate average speed
  calcAveSpd <- function(Dvmt_Cl, Spd_Cl) {
    sum(Dvmt_Cl * Spd_Cl) / sum(Dvmt_Cl)
  }
  #Set up matrix to hold average speed
  AveSpeed_MaRc <-
    array(0, dim = c(length(Ma), 3), dimnames = list(Ma, c("Fwy", "Art", "Oth")))
  #Iterate through mareas and calculate averages
  for (ma in Ma[Ma != "None"]) {
    AveSpeed_MaRc[ma, "Fwy"] <- calcAveSpd(
      Dvmt_Cl = BalanceResults_ls[[ma]]$FwyDvmt_Cl,
      Spd_Cl = BalanceResults_ls[[ma]]$SpeedAndDelay_ls$Speed[,"Fwy"]
    )
    AveSpeed_MaRc[ma, "Art"] <- calcAveSpd(
      Dvmt_Cl = BalanceResults_ls[[ma]]$ArtDvmt_Cl,
      Spd_Cl = BalanceResults_ls[[ma]]$SpeedAndDelay_ls$Speed[,"Art"]
    )
    AveSpeed_MaRc[ma, "Oth"] <- OthSpeed
  }
  if (any(Ma == "None")) {
    AveSpeed_MaRc["None",] <- NA
  }
  
  #Calculate average vehicle delay by metropolitan area and road class
  #-------------------------------------------------------------------
  #Define function to calculate average delay per vehicle mile
  calcAveDly <- function(Dvmt_Cl, Spd_Cl) {
    Rate_Cl <- 1 / Spd_Cl
    Delay_Cl <- Rate_Cl - Rate_Cl[1]
    sum(Dvmt_Cl * Delay_Cl) / sum(Dvmt_Cl)
  }
  #Set up matrix to hold average delay
  AveDelay_MaRc <-
    array(0, dim = c(length(Ma), 3), dimnames = list(Ma, c("Fwy", "Art", "Oth")))
  #Iterate through mareas and calculate averages
  for (ma in Ma[Ma != "None"]) {
    AveDelay_MaRc[ma, "Fwy"] <- calcAveDly(
      Dvmt_Cl = BalanceResults_ls[[ma]]$FwyDvmt_Cl,
      Spd_Cl = BalanceResults_ls[[ma]]$SpeedAndDelay_ls$Speed[,"Fwy"]
    )
    AveDelay_MaRc[ma, "Art"] <- calcAveDly(
      Dvmt_Cl = BalanceResults_ls[[ma]]$ArtDvmt_Cl,
      Spd_Cl = BalanceResults_ls[[ma]]$SpeedAndDelay_ls$Speed[,"Art"]
    )
    AveDelay_MaRc[ma, "Oth"] <- 0
  }
  if (any(Ma == "None")) {
    AveDelay_MaRc["None",] <- NA
  }
  
  #Calculate average speed and total vehicle delay by marea and vehicle type
  #-------------------------------------------------------------------------
  #Initialize average speed array
  Vt <- c("Ldv", "HvyTrk", "Bus")
  AveSpeed_MaVt <- array(
    0,
    dim = c(length(Ma), 3),
    dimnames = list(Ma, Vt))
  #Initialize vehicle delay array
  VehDelay_MaVt <- array(
    0,
    dim = c(length(Ma), 3),
    dimnames = list(Ma, Vt))
  #Iterate by marea and calculate average speed and total delay
  for (ma in Ma[Ma != "None"]) {
    Dvmt_VtRc <- BalanceResults_ls[[ma]]$Dvmt_VtRc
    AveSpd_Rc <- AveSpeed_MaRc[ma,]
    AveSpeed_MaVt[ma,] <-
      rowSums(sweep(Dvmt_VtRc, 2, AveSpd_Rc, "*")) / rowSums(Dvmt_VtRc)
    AveDelay_Rc <- AveDelay_MaRc[ma,]
    VehDelay_MaVt[ma,] <-
      rowSums(sweep(Dvmt_VtRc, 2, AveDelay_Rc, "*"))
  }
  if (any(Ma == "None")) {
    AveSpeed_MaVt["None",] <- NA
    VehDelay_MaVt["None",] <- NA
  }
  #Remove NaN (because no DVMT of type)
  AveSpeed_MaVt <- apply(AveSpeed_MaVt, 2, function(x) {
    x[is.nan(x)] <- max(x[!is.nan(x)], na.rm = TRUE)
    x
  })
  #Create 1-d matrix is AveSpeed_MaVt or VehDelay_MaVt are vectors
  AveSpeed_MaVt <-
    array(AveSpeed_MaVt, dim = c(length(Ma), 3), dimnames = list(Ma, Vt))
  VehDelay_MaVt <-
    array(VehDelay_MaVt, dim = c(length(Ma), 3), dimnames = list(Ma, Vt))
  
  #Calculate average vehicle speed for non-urban roads
  #---------------------------------------------------
  #Define function to calculate the rural to urban average road speed ratio
  calcUrbRurHhSpeedRatio <- function() {
    UrbanRuralAveSpeed_ls <- loadPackageDataset("UrbanRuralAveSpeed_ls","VETravelPerformance")
    UHPU_ <- L$Year$Marea$UrbanHhPropUrbanDvmt
    RHPU_ <- L$Year$Marea$NonUrbanHhPropUrbanDvmt
    HSR <- UrbanRuralAveSpeed_ls$SpeedRatio
    setNames((HSR * UHPU_ + RHPU_) / ((1 - RHPU_) - HSR * (1 - UHPU_)), Ma)
  }
  #Calculate speed ratio
  RoadSpeedRatio_Ma <- calcUrbRurHhSpeedRatio()
  #Calculate non-urban road speeds
  if (length(Ma) > 1) {
    NonUrbanAveSpeed_Ma <- RoadSpeedRatio_Ma * AveSpeed_MaVt[,"Ldv"]
  } else {
    NonUrbanAveSpeed_Ma <- RoadSpeedRatio_Ma * AveSpeed_MaVt["Ldv"]
  }
  #Constrain average non-urban speed to be in reasonable bounds
  NonUrbanAveSpeed_Ma[NonUrbanAveSpeed_Ma > 60] <- 60
  #If any marea is named 'None', assign maximum non-urban speed
  if (any(Ma == "None")) {
    MaxSpeed <- max(NonUrbanAveSpeed_Ma, na.rm = TRUE)
    NonUrbanAveSpeed_Ma["None"] <- MaxSpeed
    rm(MaxSpeed)
  }
  #If any NonUrbanAveSpeed is less than 0, then set to 60
  #This condition exists when there is no urban population in the marea
  NonUrbanAveSpeed_Ma[NonUrbanAveSpeed_Ma < 0] <- 60
  
  #Proportions of freeway and arterial DVMT by congestion level
  #------------------------------------------------------------
  Cl <- c("None", "Mod", "Hvy", "Sev", "Ext")
  #Proportion of freeway DVMT by congestion level
  FwyDvmt_MaCl <-
    do.call(rbind, lapply(BalanceResults_ls, function(x) x$FwyDvmt_Cl))[Ma,]
  FwyDvmt_MaCl <-
    array(FwyDvmt_MaCl, dim = c(length(Ma), length(Cl)), dimnames = list(Ma, Cl))
  FwyDvmtPropByLevel_MaCl <- sweep(FwyDvmt_MaCl, 1, rowSums(FwyDvmt_MaCl), "/")
  FwyDvmtPropByLevel_MaCl[is.na(FwyDvmtPropByLevel_MaCl)] <- 0
  FwyDvmtPropByLevel_ls <- as.list(data.frame(FwyDvmtPropByLevel_MaCl))
  names(FwyDvmtPropByLevel_ls) <-
    paste0("FwyDvmtProp", names(FwyDvmtPropByLevel_ls), "Cong")
  #Proportion of arterial DVMT by congestion level
  ArtDvmt_MaCl <-
    do.call(rbind, lapply(BalanceResults_ls, function(x) x$ArtDvmt_Cl))[Ma,]
  ArtDvmt_MaCl <-
    array(ArtDvmt_MaCl, dim = c(length(Ma), length(Cl)), dimnames = list(Ma, Cl))
  ArtDvmtPropByLevel_MaCl <- sweep(ArtDvmt_MaCl, 1, rowSums(ArtDvmt_MaCl), "/")
  ArtDvmtPropByLevel_MaCl[is.na(ArtDvmtPropByLevel_MaCl)] <- 0
  ArtDvmtPropByLevel_ls <- as.list(data.frame(ArtDvmtPropByLevel_MaCl))
  names(ArtDvmtPropByLevel_ls) <-
    paste0("ArtDvmtProp", names(ArtDvmtPropByLevel_ls), "Cong")
  
  #Calculate average congestion cost per mile
  #------------------------------------------
  AveCongPrice_Ma <-
    (rowSums(FwyPrices_MaCl * FwyDvmt_MaCl) + rowSums(ArtPrices_MaCl * ArtDvmt_MaCl)) /
    sum(FwyDvmt_MaCl + ArtDvmt_MaCl)
  
  #Freeway and arterial speeds by congestion level
  #-----------------------------------------------
  #Average freeway speed by congestion level
  FwySpdByLevel_ls <-
    as.list(data.frame(
      do.call(rbind, lapply(BalanceResults_ls, function(x) x$SpeedAndDelay_ls$Speed[,"Fwy"]))
    ))
  names(FwySpdByLevel_ls) <- paste0("Fwy", names(FwySpdByLevel_ls), "CongSpeed")
  FwySpdByLevel_ls <- lapply(FwySpdByLevel_ls,
                             function(x) {names(x) <- unattr(Ma); x})
  #Average arterial speed by congestion level
  ArtSpdByLevel_ls <-
    as.list(data.frame(
      do.call(rbind, lapply(BalanceResults_ls, function(x) x$SpeedAndDelay_ls$Speed[,"Art"]))
    ))
  names(ArtSpdByLevel_ls) <- paste0("Art", names(ArtSpdByLevel_ls), "CongSpeed")
  ArtSpdByLevel_ls <- lapply(ArtSpdByLevel_ls,
                             function(x) {names(x) <- unattr(Ma); x})
  
  #Save performance measures
  #-------------------------
  #LDV DVMT by road class
  Out_ls$Year$Marea$LdvFwyDvmt <-
    unattr(unlist(lapply(BalanceResults_ls, function(x) x$Dvmt_VtRc["Ldv", "Fwy"])))
  Out_ls$Year$Marea$LdvArtDvmt <-
    unname(unlist(lapply(BalanceResults_ls, function(x) x$Dvmt_VtRc["Ldv", "Art"])))
  #Average speed by vehicle type
  Out_ls$Year$Marea$LdvAveSpeed <- AveSpeed_MaVt[,"Ldv"]
  Out_ls$Year$Marea$HvyTrkAveSpeed <- AveSpeed_MaVt[,"HvyTrk"]
  Out_ls$Year$Marea$BusAveSpeed <- AveSpeed_MaVt[,"Bus"]
  #Average non-urban speed
  Out_ls$Year$Marea$NonUrbanAveSpeed <- NonUrbanAveSpeed_Ma
  #Total freeway delay by vehicle type
  Out_ls$Year$Marea$LdvTotDelay <- VehDelay_MaVt[,"Ldv"]
  Out_ls$Year$Marea$HvyTrkTotDelay <- VehDelay_MaVt[,"HvyTrk"]
  Out_ls$Year$Marea$BusTotDelay <- VehDelay_MaVt[,"Bus"]
  #Average congestion cost per mile
  Out_ls$Year$Marea$AveCongPrice <- AveCongPrice_Ma
  #Add list of freeway DVMT proportions by congestion level
  Out_ls$Year$Marea <-
    c(Out_ls$Year$Marea, FwyDvmtPropByLevel_ls); rm(FwyDvmtPropByLevel_ls)
  #Add list of arterial DVMT proportions by congestion level
  Out_ls$Year$Marea <-
    c(Out_ls$Year$Marea, ArtDvmtPropByLevel_ls); rm(ArtDvmtPropByLevel_ls)
  #Add list of speed by freeway congestion level
  Out_ls$Year$Marea <-
    c(Out_ls$Year$Marea, FwySpdByLevel_ls); rm(FwySpdByLevel_ls)
  #Add list of speed by arterial congestion level
  Out_ls$Year$Marea <-
    c(Out_ls$Year$Marea, ArtSpdByLevel_ls); rm(ArtSpdByLevel_ls)
  #Average other road speed
  Out_ls$Year$Marea$OthSpd <- OthSpeed
  #Return the result
  Out_ls
}
