library(dplyr)
library(rpart)
library(rpart.plot)
library(tidycensus)
library(butcher)
library(targets)

#' Retrieve ACS data using tidycensus package
#'
#' This function downloads American Community Survey (ACS) data for all states at the specified geography level and year.
#'
#' @param year The ACS year to download data for (default options: 2010, 2020)
#' @param geography The geographic level for data retrieval (default: "block group")
#' @param acs_vars Named vector of ACS variables to include, where names are the desired
#'        column names and values are the Census variable codes
#'
#' @return A data frame containing ACS data for the specified variables
#' @export
#'
get_acs_data <- function(
    year,
    geography = "block group",
    acs_vars) {
    ## https://api.census.gov/data/2020/acs/acs5/groups/B25024.json

    library(dplyr)
    library(tidycensus)
    library(purrr)

    states <- state.abb # use state abbrevations from tidycensus
    acs_vars_est <- map(acs_vars, ~ paste0(.x, "E")) |> unlist()
    state_df_ls <- vector("list", length(states))

    ## concatenate from state files
    for (i in seq_along(states)) {
        st <- states[i]
        state_ct_df_ <- get_acs(
            geography,
            variables = unname(acs_vars),
            year = year,
            output = "wide",
            state = st
        ) |>
            rename(all_of(acs_vars_est)) |>
            select(GEOID, all_of(names(acs_vars_est)))

        state_df_ls[[i]] <- state_ct_df_
    }

    # Combine all dataframes at once
    acs_df <- bind_rows(state_df_ls)
    acs_df <- acs_sf_mf(acs_df)

    return(acs_df)
}

tar_load(bg_all_df_2017)
# acs_df <- readRDS("output/intermediate/acs_df.rds") |>
#     rename(TransitShare = transit_share, WalkingShare = walking_share)

acs_df <- get_acs_data(
    year = 2019, # pre-covid
    geography = "block group",
    acs_vars = c("total" = "B08301_001", "driving" = "B08301_002", "transit" = "B08301_010", "bike" = "B08301_018", "walk" = "B08301_019", "work_from_home" = "B08301_021", "median_hh_income" = "B19013_001", "total_units" = "B25024_001", "sf_units" = "B25024_002", "mh_units" = "B25024_010", "boat_rv_van_units" = "B25024_011"),
)
acs_df <- acs_df |>
    mutate(
        DrivingShare = if_else(total == 0, 0, driving / total),
        TransitShare = if_else(total == 0, 0, transit / total),
        BikingWalkingShare = if_else(total == 0, 0, (bike + walk) / total),
        WalkingShare = if_else(total == 0, 0, walk / total),
        WorkFromHomeShare = if_else(total == 0, 0, work_from_home / total),
        MFHShare = if_else(total_units == 0, 0, (total_units - sf_units - mh_units - boat_rv_van_units) / total_units)
    )
#                # total,         driving,      transit,      bike,         walk,               work from home
#    variables = c("B08301_001", "B08301_002", "B08301_010", "B08301_018", "B08301_019", "B08301_021")

d4_df <- bg_all_df_2017 |>
    filter(!is.na(UA_NAME)) |>
    left_join(acs_df, by = "GEOID") |>
    select(
        D4A, D4C, DistToStop, DistToFgwSta, HasFgwTransit,
        TranRevMiPC, TransitShare
    )

transit_sh_tree <- rpart(
    TransitShare ~ D4A + D4C + DistToStop + DistToFgwSta + HasFgwTransit + TranRevMiPC,
    data = d4_df,
    control = rpart.control(minbucket = 500, maxdepth = 3, cp = 0.01)
    # maxdepth = 5
)

D4LevelDTree <- butcher(transit_sh_tree, verbose = TRUE)
save(D4LevelDTree, file = "VELandUse/data/D4LevelDTree.rda")



d3_df <- sld_v3_df_x |>
    left_join(acs_df, by = "GEOID") |>
    select(
        WalkingShare,
        D1D_hmbuf,
        D2A_JPHH_hmbuf,
        D3bpo4,
    )

pdf("output/figs/walking_sh_tree.pdf")
walking_sh_tree <- rpart(WalkingShare ~ D1D_hmbuf + D2A_JPHH_hmbuf + D3bpo4,
    data = d3_df,
    control = rpart.control(minbucket = 1000, maxdepth = 3, cp = 0.001)
    # maxdepth = 5
)
rpart.plot(walking_sh_tree)
dev.off()

D3LevelDTree <- butcher(walking_sh_tree, verbose = TRUE)
save(D3LevelDTree, file = "VELandUse/data/D3LevelDTree.rda")
