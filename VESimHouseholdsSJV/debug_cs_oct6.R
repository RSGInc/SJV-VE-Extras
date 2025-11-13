# Some debugging of the hh function
library(visioneval)

load("VESimHouseholdsSJV/data/Hh_df.rda")
HtProb_HtAp <- calcHhAgeTypes(Hh_df)

the_year <- 2022

Prsn_Ap # A named vector containing the number of persons in each age  category
TargetHhSize # A double specifying a household size target value or NA
TargetProp1PerHh # A double specifying a target for the proportion of households that are one-person households or NA if there is no target.

input_folder <- "E:/Projects/Clients/SJV_VE/Model_runs/VisionEval-Dev/built/visioneval/4.3.3/runtime/models/VERSPM-SJV-base/inputs"
azone_hh_pop_by_age <- read.csv(file.path(input_folder, "azone_hh_pop_by_age.csv"))
azone_hh_pop_by_age$Geo <- as.character(azone_hh_pop_by_age$Geo)
azone_hhsize_targets <- read.csv(file.path(input_folder, "azone_hhsize_targets.csv"))
AveHhSize <- azone_hhsize_targets[azone_hhsize_targets$Year == the_year,c("AveHhSize")]
names(AveHhSize) <- azone_hh_pop_by_age$Geo[azone_hh_pop_by_age$Year == the_year]
Prop1PerHh <- azone_hhsize_targets[azone_hhsize_targets$Year == the_year,c("Prop1PerHh")] 
names(Prop1PerHh) <- azone_hh_pop_by_age$Geo[azone_hh_pop_by_age$Year == the_year]
azone_gq_pop_by_age <- read.csv(file.path(input_folder, "azone_gq_pop_by_age.csv"))
azone_gq_pop_by_age$Geo <- as.character(azone_gq_pop_by_age$Geo)

defs_folder <- "E:/Projects/Clients/SJV_VE/Model_runs/VisionEval-Dev/built/visioneval/4.3.3/runtime/models/VERSPM-SJV-base/defs"
geo <- read.csv(file.path(defs_folder, "geo.csv"))
geo$Azone <- as.character(geo$Azone)
# Prsn_Ap_df <- azone_hh_pop_by_age[azone_hh_pop_by_age$Year == the_year,
#                     c("Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus")]
# Prsn_Ap <- as.matrix.data.frame(Prsn_Ap_df, dim = c(nrow(Prsn_Ap_df), ncol(Prsn_Ap_df)),
#                     dimnames = list(colnames(Prsn_Ap_df)))

# Test on alternative values for zone 129
AveHhSize["129"] <- 1.64
Prop1PerHh["129"] <- 0.14633782

AveHhSize["148"] <- NA
Prop1PerHh["148"] <- NA
#azone_hh_pop_by_age[azone_hh_pop_by_age$Geo == "148",Ap] <- 1
azone_hh_pop_by_age[azone_hh_pop_by_age$Geo == "148",Ap] <- 10

AveHhSize[AveHhSize == 1]
names(AveHhSize)[AveHhSize == 1]
Prop1PerHh[names(AveHhSize)[AveHhSize == 1]] <- NA
AveHhSize[AveHhSize == 1] <- NA

AveHhSize["190"] <- NA
Prop1PerHh["190"] <- NA
azone_hh_pop_by_age[azone_hh_pop_by_age$Geo == "190",Ap] <- 10

AveHhSize["198"] <- NA
Prop1PerHh["198"] <- NA
azone_hh_pop_by_age[azone_hh_pop_by_age$Geo == "198",Ap] <- 10

AveHhSize["215"] <- NA
Prop1PerHh["215"] <- NA
azone_hh_pop_by_age[azone_hh_pop_by_age$Geo == "215",Ap] <- 10

AveHhSize["256"] <- NA
Prop1PerHh["256"] <- NA
azone_hh_pop_by_age[azone_hh_pop_by_age$Geo == "256",Ap] <- 10

dummy_zones <- as.character(267:283)
azone_hh_pop_by_age[azone_hh_pop_by_age$Geo %in% dummy_zones,Ap]
azone_hh_pop_by_age[azone_hh_pop_by_age$Geo %in% dummy_zones,"Age20to29"] <- round(azone_hh_pop_by_age[azone_hh_pop_by_age$Geo %in% dummy_zones,"Age30to54"]/2)
azone_hh_pop_by_age[azone_hh_pop_by_age$Geo %in% dummy_zones,"Age30to54"] <- azone_hh_pop_by_age[azone_hh_pop_by_age$Geo %in% dummy_zones,"Age30to54"] - azone_hh_pop_by_age[azone_hh_pop_by_age$Geo %in% dummy_zones,"Age20to29"]
azone_hh_pop_by_age[azone_hh_pop_by_age$Geo %in% dummy_zones,c("Age0to14","Age15to19","Age55to64","Age65Plus")] <- 1
azone_hh_pop_by_age[azone_hh_pop_by_age$Geo %in% dummy_zones,Ap]


write.csv(azone_hh_pop_by_age, "E:/Projects/Clients/SJV_VE/_debug/azone_hh_pop_by_age_modified.csv", row.names = FALSE)
azone_hhsize_targets_base_year_modified <- data.frame(Azone = Az, AveHhSize, Prop1PerHh)
write.csv(azone_hhsize_targets_base_year_modified, "E:/Projects/Clients/SJV_VE/_debug/azone_hhsize_targets_base_year_modified.csv", row.names = FALSE)

CreateHouseholds <- function(L) {
  #Define dimension name vectors
  Ap <-
    c("Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus")
  Ag <- paste0("Grp", Ap)
  #Az <- L$Year$Azone$Azone
  Az <- azone_hh_pop_by_age[azone_hh_pop_by_age$Year == the_year,]$Geo
  #fix seed as synthesis involves sampling
  #set.seed(L$G$Seed)
  #Initialize output list
  Out_ls <- initDataList()
  Out_ls$Year$Azone$NumHh <- numeric(0)
  Out_ls$Year$Household <-
    list(
      Azone = character(0),
      Marea = character(0),
      HhId = character(0),
      HhSize = integer(0),
      HhType = character(0),
      Age0to14 = integer(0),
      Age15to19 = integer(0),
      Age20to29 = integer(0),
      Age30to54 = integer(0),
      Age55to64 = integer(0),
      Age65Plus = integer(0)
    )
  #Make matrix of regular household persons by Azone and age group
  #Prsn_AzAp <-
  #  as.matrix(data.frame(L$Year$Azone, stringsAsFactors = FALSE)[,Ap])
    Prsn_AzAp <-
      as.matrix(data.frame(azone_hh_pop_by_age[azone_hh_pop_by_age$Year == the_year,], stringsAsFactors = FALSE)[,Ap])
  rownames(Prsn_AzAp) <- Az
  #If values in Prsn_AzAp are not integers, round them and issue warning
  AllInt <- all(Prsn_AzAp == round(Prsn_AzAp))
  if (!AllInt) {
    Prsn_AzAp <- round(Prsn_AzAp)
    Msg <- paste(
      "Inputs for number of persons by age group in 'azone_hh_pop_by_age.csv'",
      "file include some non-integer values for the year", L$G$Year, ".",
      "These have been rounded to the nearest whole number."
    )
    addWarningMsg(Msg) # Attaches to Out_ls in the current frame by default
    rm(Msg)
  }
  rm(AllInt)
  #Make vector of average household size target by Azone
  #TargetHhSize_Az <- L$Year$Azone$AveHhSize
  TargetHhSize_Az <- AveHhSize
  names(TargetHhSize_Az) <- Az
  #Make vector of target proportion of 1-person households
  #TargetProp1PerHh_Az <- L$Year$Azone$Prop1PerHh
  TargetProp1PerHh_Az <- Prop1PerHh
  names(TargetProp1PerHh_Az) <- Az
  #Make matrix of group population households by Azone and age group
  # Prsn_AzAg <-
  #   as.matrix(data.frame(L$Year$Azone, stringsAsFactors = FALSE)[,Ag])
  Prsn_AzAg <-
   as.matrix(data.frame(azone_gq_pop_by_age[azone_gq_pop_by_age$Year == the_year,], stringsAsFactors = FALSE)[,Ag])
  rownames(Prsn_AzAg) <- Az
  #If values in Prsn_AzAg are not integers, round them and issue warning
  AllInt <- all(Prsn_AzAg == round(Prsn_AzAg))
  if (!AllInt) {
    Prsn_AzAg <- round(Prsn_AzAg)
    Msg <- paste(
      "Inputs for number of persons by age group in 'azone_gq_pop_by_age.csv'",
      "file include some non-integer values for the year", L$G$Year, ".",
      "These have been rounded to the nearest whole number."
    )
    addWarningMsg(Msg) # Attaches to Out_ls in the current frame by default
    rm(Msg)
  }
  rm(AllInt)
  #Simulate households for each Azone and add to output list
  for (az in Az) {
    RegHh_ls <-
      createHhByAge(Prsn_AzAp[az,],
                    MaxIter=100,
                    TargetHhSize = TargetHhSize_Az[az],
                    TargetProp1PerHh = TargetProp1PerHh_Az[az])
    GrpHh_ls <-
      createGrpHhByAge(Prsn_AzAg[az,])
    NumRegHh <- length(RegHh_ls[[1]])
    NumGrpHh <- length(GrpHh_ls[[1]])
    NumHh <- NumRegHh + NumGrpHh
    #Marea <- L$Year$Azone$Marea[L$Year$Azone$Azone == az]
    Marea <- geo$Marea[geo$Azone == az]
    Out_ls$Year$Household$Azone <-
      c(Out_ls$Year$Household$Azone, rep(az, NumHh))
    Out_ls$Year$Household$Marea <-
      c(Out_ls$Year$Household$Marea, rep(Marea, NumHh))
    Out_ls$Year$Household$HhId <-
      c(Out_ls$Year$Household$HhId, paste(rep(az, NumHh), 1:NumHh, sep = "-"))
    Out_ls$Year$Household$HhSize <-
      c(Out_ls$Year$Household$HhSize, RegHh_ls$HhSize, GrpHh_ls$HhSize)
    Out_ls$Year$Household$HhType <-
      c(Out_ls$Year$Household$HhType, RegHh_ls$HhType, GrpHh_ls$HhType)
    Out_ls$Year$Household$Age0to14 <-
      c(Out_ls$Year$Household$Age0to14, RegHh_ls$Age0to14, GrpHh_ls$Age0to14)
    Out_ls$Year$Household$Age15to19 <-
      c(Out_ls$Year$Household$Age15to19, RegHh_ls$Age15to19, GrpHh_ls$Age15to19)
    Out_ls$Year$Household$Age20to29 <-
      c(Out_ls$Year$Household$Age20to29, RegHh_ls$Age20to29, GrpHh_ls$Age20to29)
    Out_ls$Year$Household$Age30to54 <-
      c(Out_ls$Year$Household$Age30to54, RegHh_ls$Age30to54, GrpHh_ls$Age30to54)
    Out_ls$Year$Household$Age55to64 <-
      c(Out_ls$Year$Household$Age55to64, RegHh_ls$Age55to64, GrpHh_ls$Age55to64)
    Out_ls$Year$Household$Age65Plus <-
      c(Out_ls$Year$Household$Age65Plus, RegHh_ls$Age65Plus, GrpHh_ls$Age65Plus)
    Out_ls$Year$Azone$NumHh <- c(Out_ls$Year$Azone$NumHh, NumRegHh)
    Out_ls$Year$Azone$NumGq <- c(Out_ls$Year$Azone$NumGq, NumGrpHh)
  }
  Out_ls$Year$Household$HhSize <- as.integer(Out_ls$Year$Household$HhSize)
  Out_ls$Year$Household$Age0to14 <- as.integer(Out_ls$Year$Household$Age0to14)
  Out_ls$Year$Household$Age15to19 <- as.integer(Out_ls$Year$Household$Age15to19)
  Out_ls$Year$Household$Age20to29 <- as.integer(Out_ls$Year$Household$Age20to29)
  Out_ls$Year$Household$Age30to54 <- as.integer(Out_ls$Year$Household$Age30to54)
  Out_ls$Year$Household$Age55to64 <- as.integer(Out_ls$Year$Household$Age55to64)
  Out_ls$Year$Household$Age65Plus <- as.integer(Out_ls$Year$Household$Age65Plus)
  Out_ls$Year$Azone$NumHh <- as.integer(Out_ls$Year$Azone$NumHh)
  Out_ls$Year$Azone$NumGq <- as.integer(Out_ls$Year$Azone$NumGq)
  #Calculate LENGTH attribute for Household table
  attributes(Out_ls$Year$Household)$LENGTH <-
    length(Out_ls$Year$Household$HhId)
  #Calculate SIZE attributes for 'Household$Azone' and 'Household$HhId'
  attributes(Out_ls$Year$Household$Azone)$SIZE <-
    max(nchar(Out_ls$Year$Household$Azone))
  attributes(Out_ls$Year$Household$Marea)$SIZE <-
    max(nchar(Out_ls$Year$Household$Marea))
  attributes(Out_ls$Year$Household$HhId)$SIZE <-
    max(nchar(Out_ls$Year$Household$HhId))
  attributes(Out_ls$Year$Household$HhType)$SIZE <-
    max(nchar(Out_ls$Year$Household$HhType))
  #Return the list
  Out_ls
}


# Test
Prsn_Ap = Prsn_AzAp[az,]
MaxIter = 100
TargetHhSize = TargetHhSize_Az[az]
TargetProp1PerHh = TargetProp1PerHh_Az[az]

test_az <- "129"
test_az <- "148"
test_az <- "267"
test_az <- "228"
test_az <- "128"

Prsn_Ap = Prsn_AzAp[test_az,]
MaxIter = 100
TargetHhSize = TargetHhSize_Az[test_az]
TargetHhSize = 2.5
TargetProp1PerHh = TargetProp1PerHh_Az[test_az]
TargetProp1PerHh = 0.146
TargetProp1PerHh = 0.0.


(1000 * TargetProp1PerHh * 1 + 1000 * (1 - TargetProp1PerHh) * 2)/1000


createHhByAge <-
  function(Prsn_Ap,
           MaxIter = 100,
           TargetHhSize = NA,
           TargetProp1PerHh = NA) {
    #Dimension names
    #HtProb_HtAp <- loadPackageDataset("HtProb_HtAp","VESimHouseholdsSJV")
    Ap <- colnames(HtProb_HtAp)
    Ht <- rownames(HtProb_HtAp)
    #Place persons by age into household types by multiplying person vector
    #by probabilities
    Prsn_HtAp <- sweep(HtProb_HtAp, 2, Prsn_Ap, "*")
    #Make table of factors to convert persons into households and vice verse
    PrsnFactors_Ht_Ap <-
      lapply(strsplit(Ht, "-"), function(x)
        as.numeric(x))
    PrsnFactors_HtAp <- do.call(rbind, PrsnFactors_Ht_Ap)
    dimnames(PrsnFactors_HtAp) <- dimnames(Prsn_HtAp)
    rm(PrsnFactors_Ht_Ap)
    # Calculate household size for each household type
    HsldSize_Ht <- rowSums( PrsnFactors_HtAp )
    #Initial calculation of persons by age for each housing type
    #-----------------------------------------------------------
    #Convert population into households. Each row of Hsld_HtAp contains an
    #estimate of the number of household of the type given the number of persons
    #assigned to the household type
    Hsld_HtAp <- Prsn_HtAp / PrsnFactors_HtAp
    Hsld_HtAp[is.na(Hsld_HtAp)] <- 0
    MaxHh_Ht <- apply(Hsld_HtAp, 1, max)
    #Iterate until "balanced" set of households is created
    #-----------------------------------------------------
    MaxDiff_ <- numeric(MaxIter)
    HsldSize_ <- numeric(MaxIter)
    for (i in 1:MaxIter) {
    # i <- 13
    #for (i in 1:13) {
      #Resolve differences in household type estimates. For each household type
      #if there is more than one estimate of the number of households, take the
      #mean value of the estimates that are non-zero to determine the number of
      #households of the type.
      ResolveHh_HtAp <- t(apply(Hsld_HtAp, 1, function(x) {
        if (sum(x > 0) > 1) {
          x[x > 0] <- mean(x[x > 0])
        }
        x
      }))
      # Exit if the difference between the maximum estimate for each
      # household type is not too different than the resolved estimate
      # for each household type
      ResolveHh_Ht <- apply(ResolveHh_HtAp, 1, max)
      Diff_Ht <- abs(MaxHh_Ht - ResolveHh_Ht)
      PropDiff_Ht <- Diff_Ht / ResolveHh_Ht
      if (all(PropDiff_Ht < 0.001)) break
      MaxDiff_[i] <- max(PropDiff_Ht)
      # Adjust household proportions to match household size target if exists
      if (!is.na(TargetHhSize)) {
        # Calculate average household size and ratio with target household size
        AveHsldSize <-
          sum(ResolveHh_Ht * HsldSize_Ht) / sum(ResolveHh_Ht)
        HsldSize_[i] <- AveHsldSize
        HsldSizeAdj <- TargetHhSize / AveHsldSize
        # Calculate household adjustment factors and adjust households
        HsldAdjFactor_Ht <-
          HsldSize_Ht * 0 + 1 # Start with a vector of ones
        HsldAdjFactor_Ht[HsldSize_Ht > TargetHhSize] <- HsldSizeAdj
        ResolveHh_HtAp <-
          sweep(ResolveHh_HtAp, 1, HsldAdjFactor_Ht, "*")
      }
      # Adjust proportion of 1-person households to match target if there is one
      if (!is.na(TargetProp1PerHh)) {
        Hsld_Ht <- round(apply(ResolveHh_HtAp, 1, max))
        NumHh_Sz <- tapply(Hsld_Ht, HsldSize_Ht, sum)
        NumHh <- sum(NumHh_Sz)
        Add1PerHh <- (TargetProp1PerHh * NumHh) - NumHh_Sz[1]
        Is1PerHh_Ht <- HsldSize_Ht == 1
        Add1PerHh_Ht <-
          Add1PerHh * Hsld_Ht[Is1PerHh_Ht] / sum(Hsld_Ht[Is1PerHh_Ht])
        RmOthHh_Ht <-
          -Add1PerHh * Hsld_Ht[!Is1PerHh_Ht] / sum(Hsld_Ht[!Is1PerHh_Ht])
        ResolveHh_HtAp[Is1PerHh_Ht] <-
          ResolveHh_HtAp[Is1PerHh_Ht] + Add1PerHh_Ht
        ResolveHh_HtAp[!Is1PerHh_Ht] <-
          ResolveHh_HtAp[!Is1PerHh_Ht] + RmOthHh_Ht
      }
      #Calculate the number of persons by age group consistent with the resolved
      #numbers of households of each household type
      ResolvePrsn_HtAp <- ResolveHh_HtAp * PrsnFactors_HtAp
      #Convert the resolved persons tabulation into probabilities
      PrsnProb_HtAp <-
        sweep(ResolvePrsn_HtAp, 2, colSums(ResolvePrsn_HtAp), "/")
      #Calculate the difference in the number of persons by age category
      PrsnDiff_Ap <- Prsn_Ap - colSums(ResolvePrsn_HtAp)
      #Allocate extra persons to households based on probabilities
      AddPrsn_HtAp <- sweep(PrsnProb_HtAp, 2, PrsnDiff_Ap, "*")
      #Add the reallocated persons to the resolved persons matrix
      Prsn_HtAp <- ResolvePrsn_HtAp + AddPrsn_HtAp
      # Recalculate number of households by type
      Hsld_HtAp <- Prsn_HtAp/PrsnFactors_HtAp
      Hsld_HtAp[is.na(Hsld_HtAp)] <- 0
      # Calculate the maximum households by each type for convergence check
      MaxHh_Ht <- apply(ResolveHh_HtAp, 1, max)
      print(paste(i, sum(Hsld_HtAp), MaxDiff_[i]))
    }
    #Calculate number of households by household type
    Hsld_Ht <- round(apply(ResolveHh_HtAp, 1, max))
    #Calculate persons by age group and household type
    Prsn_HtAp <- sweep(PrsnFactors_HtAp, 1, Hsld_Ht, "*")
    #Convert into a matrix of households
    Hsld_Hh <- rep(names(Hsld_Ht), Hsld_Ht)
    Hsld_Hh_Ap <- strsplit(Hsld_Hh, "-")
    Hsld_Hh_Ap <- lapply(Hsld_Hh_Ap, function(x) as.numeric(x))
    Hsld_df <- data.frame(do.call(rbind, Hsld_Hh_Ap))
    names(Hsld_df) <- Ap
    Hsld_df$HhSize <- rowSums(Hsld_df)
    Hsld_df$HhType <-
      apply(Hsld_df[, Ap], 1, function(x) paste(x, collapse = "-"))
    #Randomly order the rows of the matrix and convert into a list of
    #corresponding vectors by age group
    RandomSort <-
      sample(1:nrow(Hsld_df), nrow(Hsld_df), replace = FALSE)
    Hsld_ls <- as.list(Hsld_df[RandomSort, ])
    # Return a list of corresponding age group vectors
    Hsld_ls
  }


createGrpHhByAge <-
  function(GrpPrsn_Ag) {
    if (sum(GrpPrsn_Ag > 0)) {
      GrpHh_df <-
        data.frame(
          Age0to14 = as.integer(rep(c(1,0,0,0,0,0), GrpPrsn_Ag)),
          Age15to19 = as.integer(rep(c(0,1,0,0,0,0), GrpPrsn_Ag)),
          Age20to29 = as.integer(rep(c(0,0,1,0,0,0), GrpPrsn_Ag)),
          Age30to54 = as.integer(rep(c(0,0,0,1,0,0), GrpPrsn_Ag)),
          Age55to64 = as.integer(rep(c(0,0,0,0,1,0), GrpPrsn_Ag)),
          Age65Plus = as.integer(rep(c(0,0,0,0,0,1), GrpPrsn_Ag)),
          HhSize = as.integer(rep(c(1,1,1,1,1,1), GrpPrsn_Ag)),
          HhType = rep("Grp", sum(GrpPrsn_Ag)),
          stringsAsFactors = FALSE)
      RandomSort <-
        sample(1:nrow(GrpHh_df), nrow(GrpHh_df), replace = FALSE)
      GrpHh_ls <- as.list(GrpHh_df[RandomSort, ])
    } else {
      GrpHh_ls <-
        list(
          Age0to14 = integer(0),
          Age15to19 = integer(0),
          Age20to29 = integer(0),
          Age30to54 = integer(0),
          Age55to64 = integer(0),
          Age65Plus = integer(0),
          HhSize = integer(0),
          HhType = character(0))
    }
    GrpHh_ls
  }

