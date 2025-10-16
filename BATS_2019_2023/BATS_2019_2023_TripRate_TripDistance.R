# -------------------------
# This trip rate and trip distance analysis is a person-day level analysis
# The universe is full-time workers who worked
# i.e., the person on that they have either made a work trip, or telecommuted from home (for 4+ hours)
# -------------------------


# Load required libraries
library(readr)
library(dplyr)
library(glue)
library(tidyr) # to use replace_na


# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023"

# Start a log file
log_file <- glue("{working_dir}/BATS_multi_year_TripRate_TripDistance_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for person-day trip rate and trip distance calculations: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

# -------------------------
# The universe is a person-day level dataset, for full-time workers who worked
# i.e., the person on that they have either made a work trip, or telecommuted from home (for 4+ hours)
# -------------------------

# Run the script that create the person-day level dataset
source("E:/GitHub/Travel-Diary-Surveys/BATS_2019_2023/Create_PersonDay_df_with_demographic_and_strata_vars.R")
# alternatively, one can just read the output from the above process:
# PersonDays_2019_2023_df <- read_csv("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023/ProcessedPersonDays_2019_2023.csv")

#-----------------------------------------
# Select only full time workers who worked
# This excludes people who took a sick day or the day off
#-----------------------------------------
PersonDays_2019_2023_FTworkersWorked_df <- ProcessedPersonDays_2019_2023_df %>%
  filter(employment == 1, telecommute_time >= 240 | commuted_on_travel_day)

# Write PersonDays_2019_2023_df to csv for subsequent processes
output_trips_csv <- glue("{working_dir}/PersonDays_2019_2023_FTworkersWorked.csv")
write.csv(PersonDays_2019_2023_FTworkersWorked_df, file = output_trips_csv, row.names = FALSE)
print(glue("Wrote {nrow(PersonDays_2019_2023_FTworkersWorked_df)} rows to {output_trips_csv}"))


# -------------------------
# Calculate mean, se, ci, cv etc
# -------------------------
library(srvyr)

# Create survey design object
srv_design <- PersonDays_2019_2023_FTworkersWorked_df %>%
  as_survey_design(
    weights = pdexpfac,
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

# Mean non-work distance (and SE/CI/CV) by subgroup
srv_results_non_work_dist <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_non_work_dist = survey_mean(personDay_dist_non_work_miles, vartype = c("se", "ci", "cv"))
  )

# Mean number of trips (and SE/CI/CV) by subgroup
srv_results_trips <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips = survey_mean(num_trips, vartype = c("se", "ci", "cv"))
  )

# Mean number of discretionary trips (and SE/CI/CV) by subgroup
srv_results_disc_trips <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_disc_trips = survey_mean(num_disc_trips, vartype = c("se", "ci", "cv"))
  )

# Mean number of HOME trips (and SE/CI/CV) by subgroup
srv_results_trips_HOME <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_HOME = survey_mean(num_trips_HOME, vartype = c("se", "ci", "cv"))
  )

# Mean number of WORK trips (and SE/CI/CV) by subgroup
srv_results_trips_WORK <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_WORK = survey_mean(num_trips_WORK, vartype = c("se", "ci", "cv"))
  )

# Mean number of SCHOOL trips (and SE/CI/CV) by subgroup
srv_results_trips_SCHOOL <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SCHOOL = survey_mean(num_trips_SCHOOL, vartype = c("se", "ci", "cv"))
  )

# Mean number of ESCORT trips (and SE/CI/CV) by subgroup
srv_results_trips_ESCORT <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_ESCORT = survey_mean(num_trips_ESCORT, vartype = c("se", "ci", "cv"))
  )

# Mean number of PERS_BUS trips (and SE/CI/CV) by subgroup
srv_results_trips_PERS_BUS <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_PERS_BUS = survey_mean(num_trips_PERS_BUS, vartype = c("se", "ci", "cv"))
  )

# Mean number of SHOP trips (and SE/CI/CV) by subgroup
srv_results_trips_SHOP <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SHOP = survey_mean(num_trips_SHOP, vartype = c("se", "ci", "cv"))
  )

# Mean number of MEAL trips (and SE/CI/CV) by subgroup
srv_results_trips_MEAL <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_MEAL = survey_mean(num_trips_MEAL, vartype = c("se", "ci", "cv"))
  )

# Mean number of SOCREC trips (and SE/CI/CV) by subgroup
srv_results_trips_SOCREC <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SOCREC = survey_mean(num_trips_SOCREC, vartype = c("se", "ci", "cv"))
  )

# Mean number of OTHER trips (and SE/CI/CV) by subgroup
srv_results_trips_OTHER <- srv_design %>%
  group_by(commuted_on_travel_day, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_OTHER = survey_mean(num_trips_OTHER, vartype = c("se", "ci", "cv"))
  )


# Display all results
srv_results_dist
srv_results_non_work_dist
srv_results_trips
srv_results_disc_trips
srv_results_trips_HOME
srv_results_trips_WORK
srv_results_trips_SCHOOL
srv_results_trips_ESCORT
srv_results_trips_PERS_BUS
srv_results_trips_SHOP
srv_results_trips_MEAL
srv_results_trips_SOCREC
srv_results_trips_OTHER

# -------------------------
# Create summary table
# -------------------------

# Function to process each survey result and add metadata
process_survey_result <- function(srv_result, summary_col_name) {
  srv_result %>%
    mutate(
      summary_col = summary_col_name,
      universe = case_when(
        commuted_on_travel_day == 1 ~ "Trips by full-time employed persons who commuted that day",
        commuted_on_travel_day == 0 ~ "Trips by full-time employed persons who telecommuted that day",
        TRUE ~ "Unknown"
      )
    ) %>%
    select(-commuted_on_travel_day)
}

# Process each result and standardize column names
summary_list <- list(
  process_survey_result(srv_results_dist, "personDay_dist_in_miles") %>%
    rename(mean = mean_dist, se = mean_dist_se, ci_lower_95 = mean_dist_low, 
           ci_upper_95 = mean_dist_upp, coeff_of_var = mean_dist_cv),
  
  process_survey_result(srv_results_non_work_dist, "personDay_dist_non_work_miles") %>%
    rename(mean = mean_non_work_dist, se = mean_non_work_dist_se, 
           ci_lower_95 = mean_non_work_dist_low, ci_upper_95 = mean_non_work_dist_upp, 
           coeff_of_var = mean_non_work_dist_cv),
  
  process_survey_result(srv_results_trips, "num_trips") %>%
    rename(mean = mean_num_trips, se = mean_num_trips_se, 
           ci_lower_95 = mean_num_trips_low, ci_upper_95 = mean_num_trips_upp, 
           coeff_of_var = mean_num_trips_cv),
  
  process_survey_result(srv_results_disc_trips, "num_disc_trips") %>%
    rename(mean = mean_num_disc_trips, se = mean_num_disc_trips_se, 
           ci_lower_95 = mean_num_disc_trips_low, ci_upper_95 = mean_num_disc_trips_upp, 
           coeff_of_var = mean_num_disc_trips_cv),
  
  process_survey_result(srv_results_trips_HOME, "num_trips_HOME") %>%
    rename(mean = mean_num_trips_HOME, se = mean_num_trips_HOME_se, 
           ci_lower_95 = mean_num_trips_HOME_low, ci_upper_95 = mean_num_trips_HOME_upp, 
           coeff_of_var = mean_num_trips_HOME_cv),
  
  process_survey_result(srv_results_trips_WORK, "num_trips_WORK") %>%
    rename(mean = mean_num_trips_WORK, se = mean_num_trips_WORK_se, 
           ci_lower_95 = mean_num_trips_WORK_low, ci_upper_95 = mean_num_trips_WORK_upp, 
           coeff_of_var = mean_num_trips_WORK_cv),
  
  process_survey_result(srv_results_trips_SCHOOL, "num_trips_SCHOOL") %>%
    rename(mean = mean_num_trips_SCHOOL, se = mean_num_trips_SCHOOL_se, 
           ci_lower_95 = mean_num_trips_SCHOOL_low, ci_upper_95 = mean_num_trips_SCHOOL_upp, 
           coeff_of_var = mean_num_trips_SCHOOL_cv),
  
  process_survey_result(srv_results_trips_ESCORT, "num_trips_ESCORT") %>%
    rename(mean = mean_num_trips_ESCORT, se = mean_num_trips_ESCORT_se, 
           ci_lower_95 = mean_num_trips_ESCORT_low, ci_upper_95 = mean_num_trips_ESCORT_upp, 
           coeff_of_var = mean_num_trips_ESCORT_cv),
  
  process_survey_result(srv_results_trips_PERS_BUS, "num_trips_PERS_BUS") %>%
    rename(mean = mean_num_trips_PERS_BUS, se = mean_num_trips_PERS_BUS_se, 
           ci_lower_95 = mean_num_trips_PERS_BUS_low, ci_upper_95 = mean_num_trips_PERS_BUS_upp, 
           coeff_of_var = mean_num_trips_PERS_BUS_cv),
  
  process_survey_result(srv_results_trips_SHOP, "num_trips_SHOP") %>%
    rename(mean = mean_num_trips_SHOP, se = mean_num_trips_SHOP_se, 
           ci_lower_95 = mean_num_trips_SHOP_low, ci_upper_95 = mean_num_trips_SHOP_upp, 
           coeff_of_var = mean_num_trips_SHOP_cv),
  
  process_survey_result(srv_results_trips_MEAL, "num_trips_MEAL") %>%
    rename(mean = mean_num_trips_MEAL, se = mean_num_trips_MEAL_se, 
           ci_lower_95 = mean_num_trips_MEAL_low, ci_upper_95 = mean_num_trips_MEAL_upp, 
           coeff_of_var = mean_num_trips_MEAL_cv),
  
  process_survey_result(srv_results_trips_SOCREC, "num_trips_SOCREC") %>%
    rename(mean = mean_num_trips_SOCREC, se = mean_num_trips_SOCREC_se, 
           ci_lower_95 = mean_num_trips_SOCREC_low, ci_upper_95 = mean_num_trips_SOCREC_upp, 
           coeff_of_var = mean_num_trips_SOCREC_cv),
  
  process_survey_result(srv_results_trips_OTHER, "num_trips_OTHER") %>%
    rename(mean = mean_num_trips_OTHER, se = mean_num_trips_OTHER_se, 
           ci_lower_95 = mean_num_trips_OTHER_low, ci_upper_95 = mean_num_trips_OTHER_upp, 
           coeff_of_var = mean_num_trips_OTHER_cv)
)

# Combine all results into one table
comprehensive_summary <- bind_rows(summary_list)

# Calculate additional columns
comprehensive_summary <- comprehensive_summary %>%
  mutate(
    # Calculate CI width (95% CI range)
    ci_95 = ci_upper_95 - ci_lower_95,
    
    # Estimate reliability based on coefficient of variation
    # Common thresholds: CV < 0.15 = "Excellent", 0.15-0.30 = "Good", 0.30-0.50 = "Fair", > 0.50 = "Poor"
    estimate_reliability = case_when(
      is.na(coeff_of_var) ~ "N/A",
      coeff_of_var < 0.15 ~ "Excellent",
      coeff_of_var < 0.30 ~ "Good",
      coeff_of_var < 0.50 ~ "Fair",
      TRUE ~ "Poor"
    ),
    
    # Rename existing columns
    unweighted_count = n_unweighted,
    weighted_count = round(n_weighted, 0)
  )

# Reorder columns to match requested output
comprehensive_summary <- comprehensive_summary %>%
  select(
    survey_cycle,
    summary_col,
    universe,
    mean,
    se,
    ci_95,
    ci_lower_95,
    ci_upper_95,
    coeff_of_var,
    estimate_reliability,
    unweighted_count,
    weighted_count
  ) %>%
  arrange(survey_cycle, summary_col, universe)

# Display the comprehensive summary table
print("\n=== Comprehensive Summary Table ===")
print(comprehensive_summary)

# Write comprehensive summary to CSV
output_summary_csv <- glue("{working_dir}/Summary_TripRate_TripDistance_{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")
write.csv(comprehensive_summary, file = output_summary_csv, row.names = FALSE)
print(glue("\nWrote comprehensive summary with {nrow(comprehensive_summary)} rows to {output_summary_csv}"))

# Close the log file
sink()