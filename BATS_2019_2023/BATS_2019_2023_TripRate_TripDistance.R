# Load required libraries
library(readr)
library(dplyr)
library(glue)


# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023"

# Read the linked trip data with distance appended (this is the output from BATS_2019_2023_geocode_OD_to_TAZ.R)
LinkedTrips_2019_2023_df <- read.csv(glue("{working_dir}/LinkedTrips_2019_2023_withDist_withStrata.csv"))

# Display basic information about the dataset
cat("Dataset loaded.\n")
cat("Dimensions:", nrow(LinkedTrips_2019_2023_df), "rows x", ncol(LinkedTrips_2019_2023_df), "columns\n")

# Number of rows by survey_cycle
cat("\n\nNumber of rows by survey_cycle:\n")
table(LinkedTrips_2019_2023_df$survey_cycle)

# -------------------------
# Label the data
# -------------------------

# Label the mode variable
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(mode_label = case_when(
    mode == 0 ~ "OTHER",
    mode == 1 ~ "WALK",
    mode == 2 ~ "BIKE",
    mode == 3 ~ "DA",
    mode == 4 ~ "HOV2",
    mode == 5 ~ "HOV3",
    mode == 6 ~ "WALKTRAN",
    mode == 7 ~ "DRIVETRAN",
    mode == 8 ~ "SCHBUS",
    mode == 9 ~ "TNC",
    TRUE ~ NA_character_
  ))


# Group the mode variable
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(mode4cat_label = case_when(
    mode == 0 ~ "Bike and Other",
    mode == 1 ~ "Walk",
    mode == 2 ~ "Bike and Other",
    mode == 3 ~ "Drive",
    mode == 4 ~ "Drive",
    mode == 5 ~ "Drive",
    mode == 6 ~ "Transit",
    mode == 7 ~ "Transit",
    mode == 8 ~ "Transit",
    mode == 9 ~ "Drive",
    TRUE ~ NA_character_
  ))


# Label the dpurp variable
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(dpurp_label = case_when(
    dpurp == 0 ~ "HOME",
    dpurp == 1 ~ "WORK",
    dpurp == 2 ~ "SCHOOL",
    dpurp == 3 ~ "ESCORT",
    dpurp == 4 ~ "PERS_BUS",
    dpurp == 5 ~ "SHOP",
    dpurp == 6 ~ "MEAL",
    dpurp == 7 ~ "SOCREC",
    dpurp == 10 ~ "CHANGE_MODE",
    dpurp == 11 ~ "OTHER",
    dpurp == -1 ~ "MISSING",
    TRUE ~ NA_character_
  ))

# -------------------------
# Create person-day level dataset
# -------------------------

# TODO: include people who telecommuted and didn't make any trip at all
# start from day.csv (use the variable telecommute_time)

# Group by hhno, pno, day and sum distances
PersonDay_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  group_by(hhno, pno, day, survey_cycle) %>%
  summarise(
    personDay_dist_in_miles   = sum(crow_fly_miles_cap200, na.rm = TRUE),
    employment                = first(employment),
    num_trips                 = n(),
    commuted_on_travel_day    = as.integer(any(dpurp_label == "WORK", na.rm = TRUE)),
    trexpfac                  = first(trexpfac),
    mean_trexpfac             = mean(trexpfac),
    stratification_var        = first(stratification_var),
    .groups = "drop"
  )

# full time worker only for this analysis
PersonDay_2019_2023_df <- PersonDay_2019_2023_df %>% filter(employment == 1)

# keep only people with trexpfac > 0
PersonDay_2019_2023_df <- PersonDay_2019_2023_df %>% filter(trexpfac > 0)

# Write LinkedTrips_2019_2023_df to csv for checking
output_trips_csv <- glue("{working_dir}/LinkedTrips_2019_2023_employment1.csv")
write.csv(LinkedTrips_2019_2023_df, file = output_trips_csv, row.names = FALSE)
print(glue("Wrote {nrow(LinkedTrips_2019_2023_df)} rows to {output_trips_csv}"))

# -------------------------
# Calculate mean, se, ci, cv etc
# -------------------------
library(srvyr)

# Create survey design object
srv_design <- PersonDay_2019_2023_df %>%
  as_survey_design(
    weights = trexpfac,
    strata = c(survey_cycle, stratification_var)
  )


# Mean distance (and SE/CI/CV) by subgroup
srv_results_dist <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_dist = survey_mean(personDay_dist_in_miles, vartype = c("se", "ci", "cv"))
  )

# Mean number of trips (and SE/CI/CV) by subgroup
srv_results_trips <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips = survey_mean(num_trips, vartype = c("se", "ci", "cv"))
  )

srv_results_dist
srv_results_trips