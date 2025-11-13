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
cols <- c("TranRevMiPC")
scen_ma[,  (cols) := lapply(.SD, as.numeric), .SDcols = cols]

scen_reg <- lapply(scen_list, "[[", "RegionVars")
scen_reg <- rbindlist(scen_reg, idcol = "scenario")
scen_reg <- dcast.data.table(scen_reg,
                             scenario~parameter,
                             fun.aggregate = sum,
                             value.var = "V1")

# assemble into a list 
L <- list()
L$Year <- list(Household = as.data.frame(scen_hh),
               Bzone = as.data.frame(scen_bz),
               Azone = as.data.frame(scen_az),
               Marea = as.data.frame(scen_ma),
               Region = as.data.frame(scen_reg))
str(L$Year)


#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================
#This function assigns the number of vehicles a household owns.
AssignVehicleOwnership <- function(L) {
  #Set up
  #------
  #Fix seed as synthesis involves sampling
  #set.seed(L$G$Seed)
  #Define vector of Mareas
  Ma <- L$Year$Marea$Marea
  Bz <- L$Year$Bzone$Bzone
  Az <- L$Year$Azone$Azone
  #Calculate number of households
  NumHh <- length(L$Year$Household[[1]])
  
  #Set up data frame of household data needed for model
  #----------------------------------------------------
  Hh_df <- data.frame(L$Year$Household)
  Hh_df$IsSF <- as.numeric(Hh_df$HouseType == "SF")
  Hh_df$OnlyElderly <- as.numeric(Hh_df$HhSize == Hh_df$Age65Plus)
  Hh_df$LowInc <- as.numeric(Hh_df$Income <= 20000)
  Hh_df$LogIncome <- log1p(Hh_df$Income)
  Density_ <- L$Year$Bzone$D1B[match(L$Year$Household$Bzone, L$Year$Bzone$Bzone)]
  Hh_df$LogDensity <- log(Density_)
  TranRevMiPC_Bz <- L$Year$Marea$TranRevMiPC[match(L$Year$Bzone$Marea, L$Year$Marea$Marea)]
  Hh_df$TranRevMiPC <- TranRevMiPC_Bz[match(L$Year$Household$Bzone, L$Year$Bzone$Bzone)]
  Hh_df$CarSvcCandidate <- as.integer(runif(nrow(Hh_df)) < Hh_df$CarSvcPropensity)
  
  TargetShareLvl5 <- as.vector(L$Year$Region$AVLvl5Share)
  TargetShareLvl3 <- as.vector(L$Year$Region$AVLvl3Share)
  Hh_df$AVLvl5Candidate <- as.integer((runif(nrow(Hh_df)) < Hh_df$AVLvl5Propensity))
  Hh_df$AVLvl3Candidate <- 0
  
  # Update AV candidacy based on target market share
  # ------------------------------------------------
  Hh_df$AVLvl5Candidate <- as.integer(TargetShareLvl5 > 0) * Hh_df$AVLvl5Candidate
  
  
  #Make a vehicle probability matrix
  #---------------------------------
  AutoOwnModels_ls <- loadPackageDataset("AutoOwnModels_ls","VEHouseholdVehicles")
  
  #Identify Urban households
  IsUrban <- Hh_df$LocType == "Urban"
  #No vehicle probability
  NoVehicleProb_ <- numeric(NumHh)
  NoVehicleCarSvcCoef <- 1
  # Inverse logit function
  invlogit <- function(U_){
    return(exp(U_)/(1+exp(U_)))
  }
  if (any(IsUrban)) {
    NoVehicleUtility <-
      predict(AutoOwnModels_ls$Metro$Zero,
              newdata = Hh_df[IsUrban,],
              type = "link")
    NoVehicleUtility <- NoVehicleUtility + 
      NoVehicleCarSvcCoef*Hh_df[IsUrban,"CarSvcCandidate"]
    NoVehicleProb_[IsUrban] <- invlogit(NoVehicleUtility)
  }
  if (any(!IsUrban)) {
    NoVehicleUtility <-
      predict(AutoOwnModels_ls$NonMetro$Zero,
              newdata = Hh_df[!IsUrban,],
              type = "link")
    NoVehicleUtility <- NoVehicleUtility + 
      NoVehicleCarSvcCoef*Hh_df[!IsUrban,"CarSvcCandidate"]
    NoVehicleProb_[!IsUrban] <- invlogit(NoVehicleUtility)
  }
  #Vehicle count probability
  VehicleProb_mx <- array(NA,dim = c(NumHh, 6))
  NumVehAVLvl5Coef <- 1
  NumVehCarSvcCoef <- 1
  if (any(IsUrban)) {
    VehiclePredict_mx <- 
      predict(AutoOwnModels_ls$Metro$Count,
              newdata = Hh_df[IsUrban,],
              type = "linear.predictor")$eta1
    VehiclePredict_mx <- VehiclePredict_mx + 
      NumVehAVLvl5Coef * Hh_df[IsUrban,"AVLvl5Candidate"] +
      NumVehCarSvcCoef * Hh_df[IsUrban,"CarSvcCandidate"]
    VehicleProb_mx[IsUrban,1] <- plogis(VehiclePredict_mx[,1],0,1,1)
    for(i in 2:6){
      VehicleProb_mx[IsUrban,i] <- 
        plogis(VehiclePredict_mx[,i],0,1,1) - 
        plogis(VehiclePredict_mx[,i-1],0,1,1)
    }
  }
  if (any(!IsUrban)) {
    VehiclePredict_mx <-
      predict(AutoOwnModels_ls$NonMetro$Count,
              newdata = Hh_df[!IsUrban,],
              type = "linear.predictor")$eta1
    VehiclePredict_mx <- VehiclePredict_mx + 
      NumVehAVLvl5Coef * Hh_df[!IsUrban,"AVLvl5Candidate"] +
      NumVehCarSvcCoef * Hh_df[!IsUrban,"CarSvcCandidate"]
    VehicleProb_mx[!IsUrban,1] <- plogis(VehiclePredict_mx[,1],0,1,1)
    for(i in 2:6){
      VehicleProb_mx[!IsUrban,i] <- 
        plogis(VehiclePredict_mx[,i],0,1,1) - 
        plogis(VehiclePredict_mx[,i-1],0,1,1)
    }
  }
  
  
  #Combine no-vehicle and vehicle count probabilities
  VehicleProb_HhNv <- cbind(
    NoVehicleProb_,
    sweep(VehicleProb_mx, 1, (1 - NoVehicleProb_), "*")
  )
  rm(VehicleProb_mx, NoVehicleProb_)
  
  #Predict number of vehicles using probabilities
  #----------------------------------------------
  #Predict number of vehicles for each household
  Vehicles_ <- apply(VehicleProb_HhNv, 1, function(x) {
    sample(0:6, 1, prob = x)
  })
  
  #Define function to adjust vehicle predictions to match a target number
  #----------------------------------------------------------------------
  adjVehicles <- function(NumChgVeh, Vehicles_, Hh_df, VehicleProb_mx,
                          MaxVehicles_ = NULL) {
    if(is.null(MaxVehicles_)) MaxVehicles_ <- Inf
    if (NumChgVeh > 0) {
      ChgVehCat <- 0:5
    }
    if (NumChgVeh < 0) {
      ChgVehCat <- 1:6
    }
    #Allocate the changes according to numbers of vehicles in each change category
    NumHhByCategory_ <- sapply(ChgVehCat, function(x) sum(Vehicles_ == x))
    PropHhByCategory_ <- NumHhByCategory_ / sum(NumHhByCategory_)
    ChgVehByCategory_ <- round(NumChgVeh * PropHhByCategory_)
    #Initialize a vector of changes
    VehiclesChg_ <- integer(length(Vehicles_))
    #Iterate through each category and identify changes
    for (Cat in ChgVehCat) {
      HhIdxToChg_ <- which((Vehicles_ == Cat) & (Vehicles_ < MaxVehicles_))
      if (NumChgVeh > 0) {
        NumToChg <- ChgVehByCategory_[Cat + 1]
        ChgProb_ <- VehicleProb_HhNv[HhIdxToChg_, Cat + 2]
      }
      if (NumChgVeh < 0) {
        NumToChg <- ChgVehByCategory_[Cat]
        ChgProb_ <- VehicleProb_HhNv[HhIdxToChg_, Cat]
      }
      IdxToChg_ <- utils::tail(HhIdxToChg_[order(ChgProb_)], abs(NumToChg))
      VehiclesChg_[IdxToChg_] <- sign(NumToChg)
    }
    #Calculate the adjusted number of vehicles
    Vehicles_ <- Vehicles_ + VehiclesChg_
  }
  
  #Adjust number of vehicles if target vehicles/drivers provided
  #-------------------------------------------------------------
  if (!all(is.null(L$Year$Azone$AveVehPerDriver))) {
    #Iterate by Azone to adjust vehicle predictions to match Azone target
    for (az in Az) {
      IsAzone <- Hh_df$Azone == az
      IsElderlyDrvHh <- Hh_df$Drv65Plus > 0
      if (!all(is.null(L$Year$Azone$AveVehPerElderlyDriver))) {
        # Match Elderly first
        TargetRatioElderly <-
          with(L$Year$Azone, AveVehPerElderlyDriver[Azone == az])
        NumDvrElderly <-
          with(Hh_df, sum(Drv65Plus[Azone == az & Drv65Plus > 0]))
        TargetNumVehElderly <-
          round(TargetRatioElderly * NumDvrElderly)
        NumChgVehElderly <-
          TargetNumVehElderly - sum(Vehicles_[IsAzone & IsElderlyDrvHh])
        #Calculate changes if the number of vehicles to change for elderly drivers is not 0
        if (NumChgVehElderly != 0) {
          Vehicles_[IsAzone & IsElderlyDrvHh] <- adjVehicles(
            NumChgVeh = NumChgVehElderly,
            Vehicles_ = Vehicles_[IsAzone & IsElderlyDrvHh],
            Hh_df = Hh_df[IsAzone & IsElderlyDrvHh, ],
            VehicleProb_mx = VehicleProb_mx[IsAzone & IsElderlyDrvHh, ]
          )
        }
        # Match overall Azone
        TargetRatio <- with(L$Year$Azone, AveVehPerDriver[Azone == az])
        NumDvr <- with(Hh_df, sum(Drivers[Azone == az]))
        TargetNumVeh <- round(TargetRatio * NumDvr)
        NumChgVeh <- TargetNumVeh - sum(Vehicles_[IsAzone])
        #Calculate changes if the number of vehicles to change is not 0
        if (NumChgVeh != 0) {
          Vehicles_[IsAzone & !IsElderlyDrvHh] <- adjVehicles(
            NumChgVeh = NumChgVeh,
            Vehicles_ = Vehicles_[IsAzone & !IsElderlyDrvHh],
            Hh_df = Hh_df[IsAzone & !IsElderlyDrvHh,],
            VehicleProb_mx = VehicleProb_mx[IsAzone & !IsElderlyDrvHh,])
        }
      } else {
        TargetRatio <- with(L$Year$Azone, AveVehPerDriver[Azone == az])
        NumDvr <- with(Hh_df, sum(Drivers[Azone == az]))
        TargetNumVeh <- round(TargetRatio * NumDvr)
        NumChgVeh <- TargetNumVeh - sum(Vehicles_[IsAzone])
        #Calculate changes if the number of vehicles to change is not 0
        if (NumChgVeh != 0) {
          Vehicles_[IsAzone] <- adjVehicles(
            NumChgVeh = NumChgVeh,
            Vehicles_ = Vehicles_[IsAzone],
            Hh_df = Hh_df[IsAzone,],
            VehicleProb_mx = VehicleProb_mx[IsAzone,])
        }
      }
    }
  }
  
  #Identify number of households for AV candidacy based on target market shares
  #----------------------------------------------------------------------------
  # AV Level 5 Market Share
  AVLvl5Vehicles_ <- integer(length(Vehicles_))
  if(TargetShareLvl5 > 0){
    # Check the candidacy first
    # Count existing number of Lvl5 candidates
    NumLvl5CandidateHh <- sum(Hh_df$AVLvl5Candidate)
    NumReqdLvl5Hh <- round(nrow(Hh_df) * TargetShareLvl5)
    NumChgLvl5Hh <- NumReqdLvl5Hh - NumLvl5CandidateHh
    if(NumChgLvl5Hh > 0){
      # Add additional households
      # Set AV Level 3 candidacy to 0
      CandidateHhIndex_ <- which(Hh_df$AVLvl5Candidate == 0)
      AddLvl5Hh_ <- sample(CandidateHhIndex_,
                           NumChgLvl5Hh,
                           prob = Hh_df[CandidateHhIndex_, "AVLvl5Propensity"])
      Hh_df$AVLvl5Candidate[AddLvl5Hh_] <- 1
    } else if (NumChgLvl5Hh < 0) {
      # Remove some households
      CandidateHhIndex_ <- which(Hh_df$AVLvl5Candidate == 1)
      RemoveLvl5Hh_ <- sample(CandidateHhIndex_,
                              abs(NumChgLvl5Hh),
                              prob = (1-Hh_df[CandidateHhIndex_, "AVLvl5Propensity"]))
      Hh_df$AVLvl5Candidate[RemoveLvl5Hh_] <- 0
      Hh_df$AVLvl3Candidate[RemoveLvl5Hh_] <- 1
    }
    
    # Match the vehicles
    IsAVLvl5Candidate_ <- Hh_df$AVLvl5Candidate==1 & Vehicles_>0
    AVLvl5Vehicles_ <- integer(length(Vehicles_))
    AVLvl5Prob_HhNv <- cbind(sapply(seq_len(ncol(VehicleProb_HhNv)),
                                    function(x) {
                                      propensity <- Hh_df$AVLvl5Propensity/x**4
                                      propensity[x>Vehicles_] <- 0
                                      propensity
                                    }))
    AVLvl5Prob_HhNv[IsAVLvl5Candidate_,] <- sweep(AVLvl5Prob_HhNv[IsAVLvl5Candidate_,],
                                                  1,
                                                  rowSums(AVLvl5Prob_HhNv[IsAVLvl5Candidate_,]),"/")
    AVLvl5Vehicles_[IsAVLvl5Candidate_] <- apply(
      AVLvl5Prob_HhNv[IsAVLvl5Candidate_,], 1, function(x) {
        sample(1:7, 1, prob = x)
      })
    TargetNumVehLvl5 <- round(sum(Vehicles_) * TargetShareLvl5)
    NumChgVehLvl5 <- TargetNumVehLvl5 - sum(AVLvl5Vehicles_)
    PrevNumChgVehLvl5 <- NumChgVehLvl5 + 1
    iter <- 0
    #Calculate changes if the number of automation level 5 vehicles to 
    # change is not 0
    while ((PrevNumChgVehLvl5 - NumChgVehLvl5)> 0 & iter < 20) {
      if(NumChgVehLvl5 != 0){
        AVLvl5Vehicles_[IsAVLvl5Candidate_] <- adjVehicles(
          NumChgVeh = NumChgVehLvl5,
          Vehicles_ = AVLvl5Vehicles_[IsAVLvl5Candidate_],
          Hh_df = Hh_df[IsAVLvl5Candidate_,],
          VehicleProb_mx = AVLvl5Prob_HhNv[IsAVLvl5Candidate_,],
          MaxVehicles_ = Vehicles_[IsAVLvl5Candidate_])
      }
      PrevNumChgVehLvl5 <- NumChgVehLvl5
      NumChgVehLvl5 <- TargetNumVehLvl5 - sum(AVLvl5Vehicles_)
      iter <- iter + 1
    }
  }
  # AV Level 3 Market Share
  AVLvl3Vehicles_ <- integer(length(Vehicles_))
  if(TargetShareLvl3 > 0){
    # Count existing number of Lvl5 candidates
    Hh_df$AVLvl3Candidate <- Hh_df$AVLvl5Candidate
    NumLvl3CandidateHh <- sum(Hh_df$AVLvl3Candidate)
    NumReqdLvl3Hh <- round(nrow(Hh_df) * (TargetShareLvl3+TargetShareLvl5))
    NumChgLvl3Hh <- NumReqdLvl3Hh - NumLvl3CandidateHh
    if(NumChgLvl3Hh > 0){
      # Add additional households
      # Set AV Level 3 candidacy to 0
      CandidateHhIndex_ <- which(Hh_df$AVLvl3Candidate == 0)
      AddLvl3Hh_ <- sample(CandidateHhIndex_,
                           NumChgLvl3Hh,
                           prob = Hh_df[CandidateHhIndex_, "AVLvl5Propensity"])
      Hh_df$AVLvl3Candidate[AddLvl3Hh_] <- 1
    } else if (NumChgLvl3Hh < 0) {
      # Remove some households
      CandidateHhIndex_ <- which(Hh_df$AVLvl3Candidate == 1)
      RemoveLvl3Hh_ <- sample(CandidateHhIndex_,
                              abs(NumChgLvl3Hh),
                              prob = (1-Hh_df[CandidateHhIndex_, "AVLvl5Propensity"]))
      Hh_df$AVLvl3Candidate[RemoveLvl5Hh_] <- 0
    }
    
    # Match the vehicles
    IsAVLvl3Candidate_ <- Hh_df$AVLvl3Candidate == 1 & 
      ((RemVehicles_ <- Vehicles_ - AVLvl5Vehicles_) > 0)
    AVLvl3Vehicles_ <- integer(length(Vehicles_))
    AVLvl3Prob_HhNv <- cbind(sapply(seq_len(ncol(VehicleProb_HhNv)),
                                    function(x) {
                                      propensity <- Hh_df$AVLvl5Propensity/x**4
                                      propensity[x>RemVehicles_] <- 0
                                      propensity
                                    }))
    AVLvl3Prob_HhNv[IsAVLvl3Candidate_,] <- sweep(AVLvl3Prob_HhNv[IsAVLvl3Candidate_,],
                                                  1,
                                                  rowSums(AVLvl3Prob_HhNv[IsAVLvl3Candidate_,]),"/")
    AVLvl3Vehicles_[IsAVLvl3Candidate_] <- apply(
      AVLvl3Prob_HhNv[IsAVLvl3Candidate_,], 1, function(x) {
        sample(1:7, 1, prob = x)
      })
    TargetNumVehLvl3 <- round(sum(Vehicles_) * TargetShareLvl3)
    NumChgVehLvl3 <- TargetNumVehLvl3 - sum(AVLvl3Vehicles_)
    PrevNumChgVehLvl3 <- NumChgVehLvl3 + 1
    iter <- 0
    #Calculate changes if the number of automation level 5 vehicles to 
    # change is not 0
    while ((PrevNumChgVehLvl3 - NumChgVehLvl3)> 0 & iter < 20) {
      if(NumChgVehLvl3 != 0){
        AVLvl3Vehicles_[IsAVLvl3Candidate_] <- adjVehicles(
          NumChgVeh = NumChgVehLvl3,
          Vehicles_ = AVLvl3Vehicles_[IsAVLvl3Candidate_],
          Hh_df = Hh_df[IsAVLvl3Candidate_,],
          VehicleProb_mx = AVLvl3Prob_HhNv[IsAVLvl3Candidate_,],
          MaxVehicles_ = RemVehicles_[IsAVLvl3Candidate_])
      }
      PrevNumChgVehLvl3 <- NumChgVehLvl3
      NumChgVehLvl3 <- TargetNumVehLvl3 - sum(AVLvl3Vehicles_)
      iter <- iter + 1
    }
  } else {
    Hh_df$AVLvl3Candidate <- 0
    AVLvl5Vehicles_ <- numeric(nrow(Hh_df))
    AVLvl3Vehicles_ <- numeric(nrow(Hh_df))
  }
  
  # Calculate market share by Marea
  NumVehicles_ma <- tapply(Vehicles_, L$Year$Household$Marea, sum)
  AVLvl5Vehicles_ma <- tapply(AVLvl5Vehicles_, L$Year$Household$Marea, sum)
  AVLvl3Vehicles_ma <- tapply(AVLvl3Vehicles_, L$Year$Household$Marea, sum)
  
  AVLvl5Share_ma <- AVLvl5Vehicles_ma/NumVehicles_ma
  AVLvl3Share_ma <- AVLvl3Vehicles_ma/NumVehicles_ma
  
  AVLvl5Share_ma <- AVLvl5Share_ma[L$Year$Marea$Marea]
  AVLvl3Share_ma <- AVLvl3Share_ma[L$Year$Marea$Marea]
  
  #Return the results
  #------------------
  #Initialize output list
  Out_ls <- initDataList()
  Out_ls$Year$Household <-
    list(Vehicles = Vehicles_,
         NumAVLvl5Vehicles = AVLvl5Vehicles_,
         NumAVLvl3Vehicles = AVLvl3Vehicles_,
         AVLvl5Candidate = Hh_df$AVLvl5Candidate,
         AVLvl3Candidate = Hh_df$AVLvl3Candidate,
         CarSvcCandidate = Hh_df$CarSvcCandidate)
  Out_ls$Year$Marea <- list(
    AVLvl5Share = AVLvl5Share_ma,
    AVLvl3Share = AVLvl3Share_ma
  )
  #Return the outputs list
  Out_ls
}

