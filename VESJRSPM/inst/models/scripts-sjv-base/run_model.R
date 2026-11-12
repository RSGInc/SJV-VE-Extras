#===========
#run_model.R
#===========

#Run all demo module for all years
#---------------------------------
for(Year in getYears()) {
  runModule("CreateHouseholds",                "VESimHouseholdsSJV",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictWorkers",                  "VESimHouseholdsSJV",       RunFor = "AllYears",    RunYear = Year)
  runModule("AssignLifeCycle",                 "VESimHouseholdsSJV",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictIncome",                   "VESimHouseholdsSJV",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictHousing",                  "VELandUseSJVSimHH",             RunFor = "AllYears",    RunYear = Year)
  runModule("LocateEmployment",                "VELandUseSJVSimHH",             RunFor = "AllYears",    RunYear = Year)
  runModule("AssignLocTypes",                  "VELandUseSJVSimHH",             RunFor = "AllYears",    RunYear = Year)
  runModule("Calculate4DMeasures",             "VELandUseSJVSimHH",             RunFor = "AllYears",    RunYear = Year)
  runModule("CalculateUrbanMixMeasure",        "VELandUseSJVSimHH",             RunFor = "AllYears",    RunYear = Year)
  runModule("AssignParkingRestrictions",       "VELandUseSJVSimHH",             RunFor = "AllYears",    RunYear = Year)
  runModule("AssignDemandManagement",          "VELandUseSJVSimHH",             RunFor = "AllYears",    RunYear = Year)
  runModule("AssignCarSvcAvailability",        "VELandUseDL",             RunFor = "AllYears",    RunYear = Year)
  runModule("AssignTransitService",            "VETransportSupply",     RunFor = "AllYears",    RunYear = Year)
  runModule("AssignRoadMiles",                 "VETransportSupply",     RunFor = "AllYears",    RunYear = Year)
  runModule("AssignDrivers",                   "VEHouseholdVehicles",   RunFor = "AllYears",    RunYear = Year)
  runModule("CVAVOrientation",               "VEHouseholdVehiclesDL",   RunFor = "AllYears",    RunYear = Year)
  runModule("AssignVehicleOwnership",          "VEHouseholdVehiclesDL",   RunFor = "AllYears",    RunYear = Year)
  runModule("AssignVehicleType",               "VEHouseholdVehicles",   RunFor = "AllYears",    RunYear = Year)
  runModule("CreateVehicleTable",              "VEHouseholdVehiclesDL",   RunFor = "AllYears",    RunYear = Year)
  runModule("AssignVehicleAge",                "VEHouseholdVehiclesDL",   RunFor = "AllYears",    RunYear = Year)
  runModule("CalculateVehicleOwnCost",         "VEHouseholdVehicles",   RunFor = "AllYears",    RunYear = Year)
  runModule("AdjustVehicleOwnership",          "VEHouseholdVehiclesDL",   RunFor = "AllYears",    RunYear = Year)
  runModule("AssignDriverlessVehicles",        "VEHouseholdVehiclesDL",   RunFor = "AllYears",    RunYear = Year)
  runModule("PredictWFH",                      "VETravelDemandWFH",     RunFor = "AllYears",    RunYear = Year)
  runModule("CalculateHouseholdDvmt",          "VETravelDemandWFH",     RunFor = "AllYears",    RunYear = Year)
  runModule("CalculateAltModeTrips",           "VETravelDemandWFH",     RunFor = "AllYears",    RunYear = Year)
  runModule("CalculateVehicleTrips",           "VEHouseholdTravel",     RunFor = "AllYears",    RunYear = Year)
  runModule("DivertSovTravel",                 "VEHouseholdTravel",     RunFor = "AllYears",    RunYear = Year)
  runModule("CalculateCarbonIntensity",        "VEPowertrainsAndFuelsSJV", RunFor = "AllYears",    RunYear = Year)
  runModule("AssignHhVehiclePowertrain",       "VEPowertrainsAndFuelsSJV", RunFor = "AllYears",    RunYear = Year)
  for (i in 1:2) {
    runModule("CalculateRoadDvmt",             "VETravelPerformanceDL",   RunFor = "AllYear",    RunYear = Year)
    runModule("CalculateRoadPerformance",      "VETravelPerformanceDL",   RunFor = "AllYears",    RunYear = Year)
    runModule("CalculateMpgMpkwhAdjustments",  "VETravelPerformanceDL",   RunFor = "AllYears",    RunYear = Year)
    runModule("AdjustHhVehicleMpgMpkwh",       "VETravelPerformanceDL",   RunFor = "AllYears",    RunYear = Year)
    runModule("CalculateVehicleOperatingCost", "VETravelPerformanceDL",   RunFor = "AllYears",    RunYear = Year)
    runModule("BudgetHouseholdDvmt",           "VETravelPerformanceDL",   RunFor = "AllYears",    RunYear = Year)
    # runModule("BalanceRoadCostsAndRevenues",   "VETravelPerformance",   RunFor = "AllYears",    RunYear = Year)
  }
  runModule("CalculateComEnergyAndEmissions",   "VETravelPerformanceDL",   RunFor = "AllYears",    RunYear = Year)
  runModule("CalculatePtranEnergyAndEmissions", "VETravelPerformanceDL",   RunFor = "AllYears",    RunYear = Year)
  runModule("CalculateSafetyMeasures",          "VETravelPerformanceDL",   RunFor = "AllYears",    RunYear = Year)
  runModule("TravelTimeReliability",            "VETravelPerformance",   RunFor = "AllYears",    RunYear = Year)
}
