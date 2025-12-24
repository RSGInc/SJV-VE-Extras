#Date. 3 May 2025.  This version includes the alt powertrain.
#working version - correct
#date. 12 June 2025... jon ran into error, setup_structure[[1]] 


library(visioneval)

modelName <- "VERSPM-scenarios-cat-mccog"
newModelName <- "VERSPM-scenarios-ms-mccog-run"

# Use Powertrain Scenarios
# Format
# Category level = list
# sub-list
# NAME - Name of the module
# ELEMLABEL - Name of the elemenet level use to append the directory name
# ALIAS - If module use as alias then name of the original package else ""
# DYNAMIC - If module used as dynamic module then name dynamic module specs
scen_alternatives <- list(
  "AS=2" = list(
    "NAME" = "VEPopulationSimInputsP1",
    "ELEMLABEL" = "P1",
    "ALIAS" = "",
    "DYNAMIC" = "VEPopulationSimDir"
    ),
  "AS=3" = list(
    "NAME" = "VEPopulationSimInputsP2",
    "ELEMLABEL" = "P2",
    "ALIAS" = "",
    "DYNAMIC" = "VEPopulationSimDir"
    )
)

ve_cat_model <- openModel(modelName)
ve_ms_model <- ve_cat_model$copy(newModelName, copyResults = FALSE, copyArchives = FALSE)

create_scenario_dirs <- function(structure_ls){
  # Get the name of the directory to create
  dir_name <- structure_ls$Name
  dir_path <- file.path(structure_ls$ModelDir, "scenarios", dir_name)
  # cat_levels <- names(scen_alternatives)
  # dir_paths <- c()
  # for (cat_level in cat_levels) {
  #   if (grepl(cat_level, dir_path)) {
  #     dir_paths <- c(file.path(structure_ls$ModelDir,
  #                            "scenarios",
  #                            paste0(dir_name, 
  #                                   scen_alternatives[[cat_level]][["ELEMLABEL"]]),
  #                            dir_paths)
  #     )
  #   }
  # }
  # if (dir.exists(dir_path) & all(is.null(dir_paths))) {
  #   unlink(dir_path, recursive = TRUE)
  # }
  # if (!dir.exists(dir_path) & all(is.null(dir_paths))) {
  #   dir.create(dir_path)
  # }
  if (dir.exists(dir_path)) {
    unlink(dir_path, recursive = TRUE)
  }
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
  # if(all(!is.null(dir_paths))){
  #   for (new_dir_path in dir_paths) {
  #     if (dir.exists(new_dir_path)) {
  #       unlink(new_dir_path, recursive = TRUE)
  #     }
  #     if (!dir.exists(new_dir_path)) {
  #       dir.create(new_dir_path)
  #     }
  #   }
  # }
  valid_dirs <- grepl("\\d$", structure_ls$InputPath)
  all_files <- unname(unlist(sapply(structure_ls$InputPath[valid_dirs], list.files, full.names=TRUE)))
  success <- file.copy(all_files, dir_path, overwrite = TRUE)
  if(sum(!success)>0) cat("Missing files: \n", paste0(all_files[!success], collapse = "\n"))
  invisible(TRUE)
}

create_sceanrio_configs <- function(structure_ls, scen_alternatives=NULL){
  # Get the name of the directory to create
  dir_name <- structure_ls$Name
  dir_path <- file.path(structure_ls$ModelDir, "scenarios", dir_name)
  use_alias <- FALSE
  use_dynamic <- FALSE
  # For Alias
  OriginalModuleName <- ""
  AliasModuleName <- ""
  # For Dyanmic
  DynamicParam <- ""
  DynamicModule <- ""
  cat_levels <- names(scen_alternatives)
  for(cat_level in cat_levels){
    if (grepl(cat_level, dir_path)){
      if(scen_alternatives[[cat_level]][["ALIAS"]] != ""){
        use_alias <- TRUE
        OriginalModuleName <- scen_alternatives[[cat_level]][["ALIAS"]] 
        AliasModuleName <- scen_alternatives[[cat_level]][["NAME"]] 
      }
      if(scen_alternatives[[cat_level]][["DYNAMIC"]] != ""){
        use_dynamic <- TRUE
        DynamicParam <- scen_alternatives[[cat_level]][["DYNAMIC"]] 
        DynamicModule <- scen_alternatives[[cat_level]][["NAME"]] 
      }
    }
  }
  config_all <- list()
  config_all[[dir_name]] <- list(
    Scenario = structure_ls$Scenario,
    Description = structure_ls$Description,
    Dir = dir_name
    )
  if(use_alias){
    config_all[[dir_name]][[OriginalModuleName]] <- list(
      "AliasFor"=AliasModuleName
    )
  }
  if(use_dynamic){
    config_all[[dir_name]][[DynamicParam]] <- list(
      DynamicModule
    )
  }
  return(invisible(config_all))
}

check_scenario_combos <- function(scenario_ls){
  scenario_name <- scenario_ls$Name
  if(grepl("LU=1", scenario_name)|grepl("Transit=1", scenario_name)){ # Check reference scenario
    return(scenario_ls)
  } else if (grepl("LU=2.*Transit=2", scenario_name)){
    return(scenario_ls)
  } else if(grepl("LU=3.*Transit=3", scenario_name)){
    return(scenario_ls)
  } else if(grepl("LU=4.*Transit=4", scenario_name)){
    return(scenario_ls)
  } else if(grepl("LU=5.*Transit=5", scenario_name)){
    return(scenario_ls)
  } else {
    return(invisible(NULL))
  }
}

# Create scenario input directories and inputs.
setup_structure <- lapply(ve_ms_model$modelScenarios$modelStages, getSetup)
RestrictScenarioCombos <- FALSE
if(RestrictScenarioCombos){
  valid_setup_structure <- lapply(setup_structure, check_scenario_combos)
  valid_setup_structure <- valid_setup_structure[!sapply(valid_setup_structure, is.null)]
} else {
  valid_setup_structure <- setup_structure
}
lapply(valid_setup_structure, create_scenario_dirs)


# Create scenario config files
Config_ls <- list(
  StartFrom = setup_structure[[1]]$StartFrom,
  BaseScenario = setup_structure[[1]]$BaseScenario
)

Config_ls[["ModelStages"]] <- lapply(valid_setup_structure,create_sceanrio_configs, scen_alternatives=scen_alternatives)

# Write all scenarios to config file
# configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval.cnf"))
# cat(configFile,"\n")
# yaml_config <- yaml::as.yaml(Config_ls)
# yaml_config <- gsub("- ", "  ", yaml_config)
# writeLines(yaml_config,configFile)

# # Write by 50 to WRJ5
Config_sub_ls <- Config_ls
Config_sub_ls$ModelStages <- Config_sub_ls$ModelStages[601:648]
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(Config_sub_ls)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)


# Write by 50 to RSG WRJ05
# Config_sub_ls <- Config_ls
# Config_sub_ls$ModelStages <- Config_sub_ls$ModelStages[51:100]
# configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval.cnf"))
# cat(configFile,"\n")
# yaml_config <- yaml::as.yaml(Config_sub_ls)
# yaml_config <- gsub("- ", "  ", yaml_config)
# writeLines(yaml_config,configFile)


# Write by 50 to RSG WRJ05
# Config_sub_ls <- Config_ls
# Config_sub_ls$ModelStages <- Config_sub_ls$ModelStages[101:150]
# configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval.cnf"))
# cat(configFile,"\n")
# yaml_config <- yaml::as.yaml(Config_sub_ls)
# yaml_config <- gsub("- ", "  ", yaml_config)
# writeLines(yaml_config,configFile)