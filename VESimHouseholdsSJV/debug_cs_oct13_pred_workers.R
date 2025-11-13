# Some debugging of the pred workers function

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

scen_veh <- lapply(scen_list, "[[", "VehicleVars")
scen_veh <- rbindlist(scen_veh, idcol = "scenario")

scen_wrk <- lapply(scen_list, "[[", "WorkerVars")
scen_wrk <- rbindlist(scen_wrk, idcol = "scenario")

scen_bz <- lapply(scen_list, "[[", "BzoneVars")
scen_bz <- rbindlist(scen_bz, idcol = "scenario")

scen_az <- lapply(scen_list, "[[", "AzoneVars")
scen_az <- rbindlist(scen_az, idcol = "scenario")

scen_ma <- lapply(scen_list, "[[", "MareaVars")
scen_ma <- rbindlist(scen_ma, idcol = "scenario")



#Main module function that predicts workers by age for each household
#--------------------------------------------------------------------
#' Main module function to predict workers by age for each household
#'
#' \code{PredictWorkers} predicts the number of workers by age group for each
#' household and tallies the total number of workers for each household.
#'
#' This function predicts the number of workers for each household. Household
#' workers are assigned to 5 age groups: 15-19, 20-29, 30-54, 55-64, and 65Plus.
#'
#' @param L A list containing the components listed in the Get specifications
#' for the module.
#' @return A list containing the components specified in the Set
#' specifications for the module.
#' @name PredictWorkers
#' @import visioneval
#' @include CreateEstimationDatasets.R CreateHouseholds.R
#' @export
PredictWorkers <- function(L) {
  #Define dimension name vectors
  Ag <-
    c("Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus")
  Wk <- gsub("Age", "Wkr", Ag)
  Re <- gsub("Age", "RelEmp", Ag)
  #Az <- as.vector(L$Year$Azone$Azone)
  Az <- scen_az$Azone
  #Fix seed as synthesis involves sampling
  #set.seed(L$G$Seed)
  #Calculate total number of households
  #NumHh <- length(L$Year$Household$HhType)
  NumHh <- length(scen_hh$HhType)
  #Initialize output list
  Out_ls <- initDataList()
  Out_ls$Year$Household <-
    list(
      Workers = integer(NumHh),
      Wkr15to19 = integer(NumHh),
      Wkr20to29 = integer(NumHh),
      Wkr30to54 = integer(NumHh),
      Wkr55to64 = integer(NumHh),
      Wkr65Plus = integer(NumHh)
    )
  Out_ls$Year$Azone <-
    list(
      #NumWkr = integer(length(L$Year$Azone))
      NumWkr = integer(length(scen_az$Azone))
    )
  #Initialize Errors vector
  Errors_ <- character(0)
  #Define function to predict total number of workers for by household given
  #N_ is a vector of the number of persons in the age group by household
  #P_ is the the worker probability for the age group
  #W is the total number of workers
  getNumWkr <- function(N_, P_, W) {
    NumHh <- length(N_)
    NumWkr_Hh <- setNames(integer(NumHh), 1:NumHh) #Initialize count of workers
    HhIdx_Pr <- rep(1:length(N_), N_) #Vector of persons with household index
    PrsnProb_Pr <- rep(P_, N_) #Probability that each person is a worker
    WkrIdx_ <- sample(HhIdx_Pr, W, prob = PrsnProb_Pr ) #Sample household index
    WkrTab_ <- table(WkrIdx_) #Tabulate household index
    NumWkr_Hh[names(WkrTab_)] <- WkrTab_
    NumWkr_Hh
  }
  #Iterate through age groups and Azones and identify number of workers by
  #age group for each household
  #PropHhWkr_HtAg <- loadPackageDataset("PropHhWkr_HtAg","VESimHouseholdsSJV")
  load("VESimHouseholdsSJV/data/PropHhWkr_HtAg.Rda")
  for (i in 1:length(Ag)) {
    NumWkr_Hh <- integer(NumHh)
    # az == "256"
    for (az in Az) {
      IsAz <- scen_hh$Azone == az
      NumWkr_Hh[IsAz] <- {
        NumPrsn_ <- as.integer(scen_hh[[Ag[i]]][IsAz])
        HhType_ <- scen_hh$HhType[IsAz]
        Probs_ <- PropHhWkr_HtAg[HhType_, Ag[i]]
        TotPrsn <- sum(NumPrsn_)
        RelEmp <- as.integer(scen_az[[Re[i]]][scen_az$Azone == az])
        if (is.null(RelEmp)) RelEmp <- 1
        TotWkr <- round(sum(NumPrsn_ * Probs_) * RelEmp)
        if (TotWkr > TotPrsn) {
          #catch error
          MaxVal <- TotPrsn / sum(NumPrsn_ * Probs_)
          Msg <- paste0(
            "Error during run of PredictWorkers module! ",
            "The value of ", Re[i], " for Azone ", az,
            " will result in more workers than people in that age category. ",
            "The maximum value of ", Re[i], " must be less than ",
            round(MaxVal, 2), ".")
          Errors_ <<- c(Errors_, Msg)
          NA
        } else {
          NumWkr_ <- integer(length(NumPrsn_))
          DoPredict_ <- NumPrsn_ > 0 & Probs_ > 0
          NumWkr_[DoPredict_] <-
            getNumWkr(NumPrsn_[DoPredict_], Probs_[DoPredict_], TotWkr)
          NumWkr_
        }
      }
      rm(IsAz)
    }
    Out_ls$Year$Household[[Wk[i]]] <- NumWkr_Hh
    rm(az)
  }
  rm(i)
  #Add error messages if any
  if (length(Errors_) != 0) {
    addErrorMsg("Out_ls", Errors_)
  }
  #Calculate the total number of workers
  Out_ls$Year$Household$Workers <-
    mapply(sum,
           Out_ls$Year$Household$Wkr15to19,
           Out_ls$Year$Household$Wkr20to29,
           Out_ls$Year$Household$Wkr30to54,
           Out_ls$Year$Household$Wkr55to64,
           Out_ls$Year$Household$Wkr65Plus)
  #Calculate the total number of workers by Azone
  Out_ls$Year$Azone$NumWkr <-
    tapply(Out_ls$Year$Household$Workers, scen_hh$Azone, sum)[Az]
  #Return the results
  Out_ls
}



# test the  NumWkr_


# check the population by azone and compare to the inputs
azone_hh_pop_by_age <- fread(file.path(input_folder, "azone_hh_pop_by_age.csv"))
azone_hh_pop_by_age_base <- azone_hh_pop_by_age[Year == the_year]
setnames(azone_hh_pop_by_age_base, "Geo", "Azone")

azone_hh_pop_by_age_base

cols <- c("Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus")
str(scen_hh)
scen_hh[, (cols) := lapply(.SD, as.integer), .SDcols = cols]
fwrite(scen_hh[, lapply(.SD, sum), .SDcols = cols, keyby = Azone][order(as.integer(Azone))], "E:/Projects/Clients/SJV_VE/_debug/age_azone_mod.csv")

# Create long tables and compare
scen_hh_age_mod <- melt.data.table(scen_hh[, lapply(.SD, sum), .SDcols = cols, keyby = .(Azone = as.integer(Azone))],
                                   id.vars = "Azone", variable.name = "Ag", value.name = "Mod")
azone_hh_age_inp <- melt.data.table(azone_hh_pop_by_age_base, 
                                    id.vars = c("Azone", "Year"),
                                    variable.name = "Ag", value.name = "Inp")
azone_hh_age_comp <- merge(azone_hh_age_inp,
                           scen_hh_age_mod,
                           by = c("Azone", "Ag"),
                           all = TRUE)
azone_hh_age_comp[, Diff := Mod - Inp]
azone_hh_age_comp[Inp == 0]
azone_hh_age_comp[Mod == 0]
azone_hh_age_comp[Mod == 0 & Ag != "Age0to14"]

fwrite(azone_hh_age_comp[,.(Inp = sum(Inp), Mod = sum(Mod), Diff = sum(Diff)), keyby = Azone],
       "E:/Projects/Clients/SJV_VE/_debug/azone_mod_inp_diff.csv")

fwrite(azone_hh_age_comp,
       "E:/Projects/Clients/SJV_VE/_debug/azone_age_mod_inp_diff.csv")

azone_hh_age_comp[Mod == 0]
scen_az[Azone == 62]
scen_az[order(AveHhSize)]

# Compare with number of units
scen_bz[,.(Azone, Bzone,SFDU, MFDU)]
du_azone <- scen_bz[,.(SFDU = sum(as.integer(SFDU)), MFDU = sum(as.integer(MFDU)), DU = sum(as.integer(SFDU) + as.integer(MFDU))),
        keyby = Azone]

hh_azone <- scen_az[,.(Azone, NumHh)]

du_azone[hh_azone, NumHh := as.integer(i.NumHh), on = "Azone"]
du_azone[, DUDiff := DU - NumHh]

# Fix the households (age a person in each age group)

scen_hh[Azone == 119]
scen_hh[HhId == "119-1", c("Age65Plus", "HhSize", "HhType") := .(1, 6, "1-2-0-2-0-1")]
scen_hh[HhId == "119-2", c("Age20to29", "Age30to54", "Age55to64", "Age65Plus", "HhSize", "HhType") := .(1, 2, 1, 1, 5, "0-0-1-2-1-1")]
scen_hh[Azone == 169][1:50]
scen_hh[HhId == "169-1", c("Age65Plus", "HhSize", "HhType") := .(2, 5, "1-0-2-0-0-2")]
scen_hh[HhId == "169-3", c("Age0to14", "Age20to29", "Age30to54", "Age55to64", "Age65Plus", "HhSize", "HhType") := .(0, 1, 2, 1, 1, 5, "0-0-1-2-1-1")]

scen_hh[Azone == 263]
scen_hh[Azone == 263 & Age55to64 == 1]
scen_hh[HhId == "263-2", c("Age30to54", "Age55to64", "Age65Plus", "HhSize", "HhType") := .(1, 1, 1, 5, "1-0-1-1-1-1")]
# scen_hh[HhId == "263-2", c("Age30to54", "Age55to64", "Age65Plus", "HhSize", "HhType") := .(4, 0, 2, 8, "0-0-2-4-0-2")]
# scen_hh[HhId == "263-5", c("Age30to54", "Age55to64", "Age65Plus", "HhSize", "HhType") := .(2, 1, 1, 5, "0-0-1-2-1-1")]

rownames(PropHhWkr_HtAg)