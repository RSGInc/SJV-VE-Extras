#Date. 3 May 2025.  This version includes the alt powertrain.
#working version - correct
#date. 12 June 2025... jon ran into error, setup_structure[[1]] 


library(visioneval)

modelName <- "VERSPM-swrtc-cat"
newModelName <- "SWRTC-sc-ms16June_2"

# Only works if pt_alternatives > 0
pt_alternatives <- c("VEPowertrainsAndFuelsBMCAlt")
cat_level <- "Tech=2"

bmc_cat_model <- openModel(modelName)
bmc_ms_model <- bmc_cat_model$copy(newModelName, copyResults = FALSE, copyArchives = FALSE)
# bmc_ms_model <- openModel(newModelName)

create_scenario_dirs <- function(structure_ls){
  # Get the name of the directory to create
  dir_name <- structure_ls$Name
  dir_path <- file.path(structure_ls$ModelDir, "scenarios", dir_name)
  if (grepl(cat_level, dir_path)){
    dir_paths <- file.path(structure_ls$ModelDir, "scenarios", paste0(dir_name, "Z", 1:length(pt_alternatives)))
  } else {
    dir_paths <- NULL
  }
  if (dir.exists(dir_path) & all(is.null(dir_paths))) {
    unlink(dir_path, recursive = TRUE)
  }
  if (!dir.exists(dir_path) & all(is.null(dir_paths))) {
    dir.create(dir_path)
  }
  if(all(!is.null(dir_paths))){
    for (new_dir_path in dir_paths) {
      if (dir.exists(new_dir_path)) {
        unlink(new_dir_path, recursive = TRUE)
      }
      if (!dir.exists(new_dir_path)) {
        dir.create(new_dir_path)
      }
    }
  }
  valid_dirs <- grepl("\\d$", structure_ls$InputPath)
  all_files <- unname(unlist(sapply(structure_ls$InputPath[valid_dirs], list.files, full.names=TRUE)))
  if (all(is.null(dir_paths))) {
    success <- file.copy(all_files, dir_path, overwrite = TRUE)
    if(sum(!success)>0) cat("Missing files: \n", paste0(all_files[!success], collapse = "\n"))
  }
  if(all(!is.null(dir_paths))){
    for(new_dir_path in dir_paths) {
      success <- file.copy(all_files, new_dir_path, overwrite = TRUE)
    }
  }
  invisible(TRUE)
}

create_sceanrio_configs <- function(structure_ls){
  # Get the name of the directory to create
  dir_name <- structure_ls$Name
  dir_path <- file.path(structure_ls$ModelDir, "scenarios", dir_name)
  use_alias <- FALSE
  if (grepl(cat_level, dir_path)){
    use_alias <- TRUE
  }
  config_all <- list()
  if(use_alias){
    config_all[[paste0(dir_name, "Z", 1:length(pt_alternatives))]] <- list(
      Scenario = paste0(structure_ls$Scenario, paste0("Z", 1:length(pt_alternatives))),
      Description = paste0(structure_ls$Description, " and alternative powertrain"),
      Dir = paste0(dir_name, "Z", 1:length(pt_alternatives)),
      PowertrainsAndFuels = list(AliasFor=pt_alternatives)
    )
  } else {
    config_all[[dir_name]] <- list(
      Scenario = structure_ls$Scenario,
      Description = structure_ls$Description,
      Dir = dir_name
    )
  }
  
  return(invisible(config_all))
}

# Create scenario input directories and inputs.
setup_structure <- lapply(bmc_ms_model$modelScenarios$modelStages, getSetup)
lapply(setup_structure, create_scenario_dirs)


# Create scenario config files
Config_ls <- list(
  StartFrom = setup_structure[[1]]$StartFrom,
  BaseScenario = setup_structure[[1]]$BaseScenario
)

Config_ls[["ModelStages"]] <- lapply(setup_structure,create_sceanrio_configs)

# Write all scenarios to config file
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(Config_ls)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

##-----------------##
# NF SWRTC Division #
##-----------------##

# Set to intervals of 50 to run for SWRTC - the first model ran 263, make sure that lines up with the first 263 models in the list
main_dir <- paste0(getwd(), '/models/SWRTC-sc-ms16June/results')
folders_main_dir <- list.dirs(main_dir, full.names = F, recursive = F)

Config_sub_ls <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = list())
for(i in 1:length(Config_ls$ModelStages)){
  temp <- Config_ls$ModelStages[i]
  temp2 <- temp[[1]]
  name <- names(temp2)
  if(!(name %in% folders_main_dir)){
    Config_sub_ls$ModelStages[[length(Config_sub_ls$ModelStages)+1]] <- temp2
  }
}

###  1-50
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[1:50])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_2.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  51-100
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[51:100])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_3.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  101-150
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[101:150])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_4.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  151-200
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[151:200])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_5.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  201-250
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[201:250])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_6.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  251-300
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[251:300])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_7.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  301-350
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[301:350])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_8.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  351-400
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[351:400])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_9.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  401-450
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[401:450])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_10.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  451-500
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[451:500])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_11.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  501-550
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[501:550])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_12.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  551-600
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[551:600])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_13.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  601-650
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[601:650])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_14.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  651-700
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[651:700])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_15.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

###  700-rest
temp <- list(StartFrom = 'stage-base', BaseScenario = 'stage-future', ModelStages = Config_sub_ls$ModelStages[700:length(Config_sub_ls$ModelStages)])
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_swrtc_16.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(temp)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

# Write second 500 to RSG VM
# Config_sub_ls <- Config_ls
# Config_sub_ls$ModelStages <- Config_sub_ls$ModelStages[501:1000]
# configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_rsgvm.cnf"))
# cat(configFile,"\n")
# yaml_config <- yaml::as.yaml(Config_sub_ls)
# yaml_config <- gsub("- ", "  ", yaml_config)
# writeLines(yaml_config,configFile)
# 
# 
# # Write last 500 to BMC VM
# Config_sub_ls <- Config_ls
# Config_sub_ls$ModelStages <- Config_sub_ls$ModelStages[1001:1500]
# configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval_bmcvm.cnf"))
# cat(configFile,"\n")
# yaml_config <- yaml::as.yaml(Config_sub_ls)
# yaml_config <- gsub("- ", "  ", yaml_config)
# writeLines(yaml_config,configFile)