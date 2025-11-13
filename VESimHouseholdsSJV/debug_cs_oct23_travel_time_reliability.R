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
    tblfiles <- tblfiles[tblfiles != "OthSpd.Rda"]
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

# names(scen_list)
# scen_ma_length <- lapply(scen_list[["VERSPM-SJV-base"]][["MareaVars"]], length)
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
          "ArtNoneCongChg", "ArtModCongChg", "ArtHvyCongChg", "ArtSevCongChg", "ArtExtCongChg",
          "FwyNoneCongSpeed",
          "FwyModCongSpeed",
          "FwyHvyCongSpeed",
          "FwySevCongSpeed",
          "FwyExtCongSpeed",
          "ArtNoneCongSpeed",
          "ArtModCongSpeed",
          "ArtHvyCongSpeed",
          "ArtSevCongSpeed",
          "ArtExtCongSpeed")
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

L$Year$Marea <- L$Year$Marea[,c("Marea",
"FwyNoneCongSpeed",
"FwyModCongSpeed",
"FwyHvyCongSpeed",
"FwySevCongSpeed",
"FwyExtCongSpeed",
"ArtNoneCongSpeed",
"ArtModCongSpeed",
"ArtHvyCongSpeed",
"ArtSevCongSpeed",
"ArtExtCongSpeed")]


#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================

#' Calculate travel time reliability measures

TravelTimeReliability <- function(L) {
  
  `%>%` <- magrittr::`%>%`
  
  #------
  #Define naming vectors for Mareas and congestion levels
  Ma <- L$Year$Marea$Marea
  Marea_df= data.frame(L$Year$Marea)  
  Cl <- c("None", "Mod", "Hvy", "Sev", "Ext")
  Vt <- c("Ldv", "HvyTrk", "Bus")
  Rc <- c("Fwy", "Art", "Oth")
  #Initialize outputs list
  Out_ls <- initDataList()
  Out_ls$Global$Marea <- list()
  Out_ls$Year$Marea <- list()
 
  #Function to remove attributes
  unattr <- function(X_) {
    attributes(X_) <- NULL
    X_
  }
  
  #Calculate Travel Time Index by congestion level for metropolitan areas
  #---------------------------------------------
  TTR_df <- Marea_df %>%
    dplyr::mutate(FwyModCong_TTI = (.data$FwyNoneCongSpeed)[[1]]/(.data$FwyModCongSpeed)[[1]],
                  FwyHvyCong_TTI = (.data$FwyNoneCongSpeed)[[1]]/(.data$FwyHvyCongSpeed)[[1]],
                  FwySevCong_TTI = (.data$FwyNoneCongSpeed)[[1]]/(.data$FwySevCongSpeed)[[1]],
                  FwyExtCong_TTI = (.data$FwyNoneCongSpeed)[[1]]/(.data$FwyExtCongSpeed)[[1]],
                  ArtModCong_TTI = (.data$ArtNoneCongSpeed)[[1]]/(.data$ArtModCongSpeed)[[1]],
                  ArtHvyCong_TTI = (.data$ArtNoneCongSpeed)[[1]]/(.data$ArtHvyCongSpeed)[[1]],
                  ArtSevCong_TTI = (.data$ArtNoneCongSpeed)[[1]]/(.data$ArtSevCongSpeed)[[1]],
                  ArtExtCong_TTI = (.data$ArtNoneCongSpeed)[[1]]/(.data$ArtExtCongSpeed)[[1]])
  
  TTR_df <- TTR_df %>%
    dplyr::mutate(FwyModCong_PTI = 1+3.67*log(mean((.data$FwyModCong_TTI))),
                  FwyHvyCong_PTI = 1+3.67*log(mean((.data$FwyHvyCong_TTI))),
                  FwySevCong_PTI = 1+3.67*log(mean((.data$FwySevCong_TTI))),
                  FwyExtCong_PTI = 1+3.67*log(mean((.data$FwyExtCong_TTI))),
                  ArtModCong_PTI = 1+3.67*log(mean((.data$ArtModCong_TTI))),
                  ArtHvyCong_PTI = 1+3.67*log(mean((.data$ArtHvyCong_TTI))),
                  ArtSevCong_PTI = 1+3.67*log(mean((.data$ArtSevCong_TTI))),
                  ArtExtCong_PTI = 1+3.67*log(mean((.data$ArtExtCong_TTI))))
  
  TTR_df <- TTR_df %>%
    dplyr::mutate(
      FwyModCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((.data$FwyModCong_TTI))))^(1/0.04953),
      FwyHvyCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((.data$FwyHvyCong_TTI))))^(1/0.04953),
      FwySevCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((.data$FwySevCong_TTI))))^(1/0.04953),
      FwyExtCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((.data$FwyExtCong_TTI))))^(1/0.04953),
      ArtModCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((.data$ArtModCong_TTI))))^(1/0.04953),
      ArtHvyCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((.data$ArtHvyCong_TTI))))^(1/0.04953),
      ArtSevCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((.data$ArtSevCong_TTI))))^(1/0.04953),
      ArtExtCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((.data$ArtExtCong_TTI))))^(1/0.04953)
    )
  

  
  Out_ls$Year$Marea$FwyModCong_TTI <- TTR_df$FwyModCong_TTI 
  Out_ls$Year$Marea$FwyHvyCong_TTI <- TTR_df$FwyHvyCong_TTI 
  Out_ls$Year$Marea$FwySevCong_TTI <- TTR_df$FwySevCong_TTI 
  Out_ls$Year$Marea$FwyExtCong_TTI <- TTR_df$FwyExtCong_TTI  
  Out_ls$Year$Marea$ArtModCong_TTI <- TTR_df$ArtModCong_TTI
  Out_ls$Year$Marea$ArtHvyCong_TTI <- TTR_df$ArtHvyCong_TTI 
  Out_ls$Year$Marea$ArtExtCong_TTI <- TTR_df$ArtSevCong_TTI 
  Out_ls$Year$Marea$ArtSevCong_TTI <- TTR_df$ArtExtCong_TTI   
  
  Out_ls$Year$Marea$FwyModCong_PTI <- TTR_df$FwyModCong_PTI 
  Out_ls$Year$Marea$FwyHvyCong_PTI <- TTR_df$FwyHvyCong_PTI 
  Out_ls$Year$Marea$FwySevCong_PTI <- TTR_df$FwySevCong_PTI 
  Out_ls$Year$Marea$FwyExtCong_PTI <- TTR_df$FwyExtCong_PTI  
  Out_ls$Year$Marea$ArtModCong_PTI <- TTR_df$ArtModCong_PTI
  Out_ls$Year$Marea$ArtHvyCong_PTI <- TTR_df$ArtHvyCong_PTI 
  Out_ls$Year$Marea$ArtExtCong_PTI <- TTR_df$ArtSevCong_PTI 
  Out_ls$Year$Marea$ArtSevCong_PTI <- TTR_df$ArtExtCong_PTI  
  
  Out_ls$Year$Marea$FwyModCong_BTI <- TTR_df$FwyModCong_BTI 
  Out_ls$Year$Marea$FwyHvyCong_BTI <- TTR_df$FwyHvyCong_BTI 
  Out_ls$Year$Marea$FwySevCong_BTI <- TTR_df$FwySevCong_BTI 
  Out_ls$Year$Marea$FwyExtCong_BTI <- TTR_df$FwyExtCong_BTI  
  Out_ls$Year$Marea$ArtModCong_BTI <- TTR_df$ArtModCong_BTI
  Out_ls$Year$Marea$ArtHvyCong_BTI <- TTR_df$ArtHvyCong_BTI 
  Out_ls$Year$Marea$ArtExtCong_BTI <- TTR_df$ArtSevCong_BTI 
  Out_ls$Year$Marea$ArtSevCong_BTI <- TTR_df$ArtExtCong_BTI  
  
  
  
  #Save performance measures
  #-------------------------
  
  #Return the result
  Out_ls
}
