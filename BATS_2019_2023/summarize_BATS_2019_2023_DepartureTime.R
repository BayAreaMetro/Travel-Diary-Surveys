# BATS 2019-2023 Departure Time Analysis
# Weighted share of trips by hour of day

# Load required packages
library(tidyverse)
library(srvyr)

# Read the data
data <- read_csv("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023/LinkedTrips_2019_2023_withDist_withStrata.csv")

# Label the variable opurp
# labelling should belong to an upstream script but just to be fast...
data <- data %>%
  mutate(opurp_label = case_when(
    opurp == 0 ~ "HOME",
    opurp == 1 ~ "WORK",
    opurp == 2 ~ "SCHOOL",
    opurp == 3 ~ "ESCORT",
    opurp == 4 ~ "PERS_BUS",
    opurp == 5 ~ "SHOP",
    opurp == 6 ~ "MEAL",
    opurp == 7 ~ "SOCREC",
    opurp == 10 ~ "CHANGE_MODE",
    opurp == 11 ~ "OTHER",
    opurp == -1 ~ "MISSING",
    TRUE ~ NA_character_
  ))


# keep only adults, also drop rows with 0 expfac
# write the file out
data <- data %>%
  filter(age >= 4) %>%
  filter(trexpfac > 0)

write_csv(data, 
          "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023/LinkedTrips_2019_2023_withDist_withStrata_AdultOnly_No0trexpfac.csv")

# Create hourly bins from deptm
# deptm format is HHMM (e.g., 700 = 7:00 AM, 1445 = 2:45 PM)
data <- data %>%
  mutate(
    # Extract hour from deptm (divide by 100 and take floor)
    departure_hour = floor(deptm / 100),
    # Create hour bin label
    hour_bin = sprintf("%02d:00-%02d:59", departure_hour, departure_hour)
  )

# check if there any deptm that didn't get binned
table(data$departure_hour, useNA = "ifany") 

nrows_by_cycle <- data %>%
  group_by(survey_cycle) %>%
  summarise(n = n())

nrows_by_cycle

# Function to calculate weighted statistics for a given survey cycle
analyze_departure_by_hour <- function(df, cycle_year, group_vars = NULL, 
                                      HomeToWork_filter = NULL,
                                      conf_level = 0.90) {
  
  # Filter for specific survey cycle
  df_filtered <- df %>%
    filter(survey_cycle == cycle_year)
  
  # Apply purpose filter if specified
  if (!is.null(HomeToWork_filter)) {
    df_filtered <- df_filtered %>%
      filter(opurp_label == "HOME" & dpurp_label == "WORK")
  }

  # Create survey design object
  svy_design <- df_filtered %>%
    as_survey_design(
      ids    = c(hhno, pno),      
      strata = stratification_var,
      weights = trexpfac
    )  

  # Build grouping variables dynamically
  if (!is.null(group_vars)) {
    group_syms <- syms(c("departure_hour", "hour_bin", group_vars))
  } else {
    group_syms <- syms(c("departure_hour", "hour_bin"))
  }

  # Calculate weighted statistics by hour

  results <- svy_design %>%
    group_by(!!!group_syms) %>%
    summarise(
      # Unweighted count
      n_unweighted = unweighted(n()),
      # Weighted count
      n_weighted = survey_total(vartype = c("se", "ci", "cv"), level = conf_level),
      .groups = "drop"
    )
  
  # Calculate shares within each subgroup

  # Calculate z-score for the confidence level (1.96 for 95%, 1.645 for 90%)
  z_score <- qnorm(1 - (1 - conf_level) / 2)


  if (!is.null(group_vars)) {
    results <- results %>%
      group_by(across(all_of(group_vars))) %>%
      mutate(
        weighted_share = n_weighted / sum(n_weighted),
        share_se = n_weighted_se / sum(n_weighted),
        share_cv = share_se / weighted_share,
        share_ci_lower = weighted_share - z_score * share_se,
        share_ci_upper = weighted_share + z_score * share_se
      ) %>%
      ungroup()
  } else {
    results <- results %>%
      mutate(
        weighted_share = n_weighted / sum(n_weighted),
        share_se = n_weighted_se / sum(n_weighted),
        share_cv = share_se / weighted_share,
        share_ci_lower = weighted_share - z_score * share_se,
        share_ci_upper = weighted_share + z_score * share_se
      )
  }

  # Add identifiers
  results <- results %>%
    mutate(
      # Add survey cycle identifier
      survey_cycle = cycle_year,
      # Add trip purpose identifier
      trip_purpose = ifelse(is.null(HomeToWork_filter), "All Trips", HomeToWork_filter),
      # Add grouping variable indicator
      grouping_vars = ifelse(is.null(group_vars), "None", paste(group_vars, collapse = ", "))
    ) %>%
    select(
      survey_cycle,
      trip_purpose,
      grouping_vars,
      departure_hour,
      hour_bin,
      everything()
    ) %>%
    arrange(departure_hour)  
  
  return(results)
}

# Analyze both survey cycles - ALL TRIPS (Overall)
results_2019_all <- analyze_departure_by_hour(data, 2019)
results_2023_all <- analyze_departure_by_hour(data, 2023)

# Analyze both survey cycles - WORK TRIPS ONLY (Overall)
results_2019_work <- analyze_departure_by_hour(data, 2019, HomeToWork_filter = "Home To Work")
results_2023_work <- analyze_departure_by_hour(data, 2023, HomeToWork_filter = "Home To Work")

# Analyze by income group - ALL TRIPS
results_2019_all_income <- analyze_departure_by_hour(data, 2019, group_vars = "income_detailed_grouped")
results_2023_all_income <- analyze_departure_by_hour(data, 2023, group_vars = "income_detailed_grouped")

# Analyze by income group - WORK TRIPS
results_2019_work_income <- analyze_departure_by_hour(data, 2019, group_vars = "income_detailed_grouped",
                                                       HomeToWork_filter = "Home To Work")
results_2023_work_income <- analyze_departure_by_hour(data, 2023, group_vars = "income_detailed_grouped",
                                                       HomeToWork_filter = "Home To Work")

# Combine results
all_results <- bind_rows(
  results_2019_all, 
  results_2023_all,
  results_2019_work,
  results_2023_work,
  results_2019_all_income,
  results_2023_all_income,
  results_2019_work_income,
  results_2023_work_income
)


# Print results
cat("\n=== Departure Time Analysis - ALL TRIPS ===\n")
cat("\n2019 - All Trips:\n")
print(results_2019_all, n = Inf)
cat("\n2023 - All Trips:\n")
print(results_2023_all, n = Inf)

cat("\n\n=== Departure Time Analysis - WORK TRIPS ===\n")
cat("\n2019 - Work Trips:\n")
print(results_2019_work, n = Inf)
cat("\n2023 - Work Trips:\n")
print(results_2023_work, n = Inf)

cat("\n\n=== Departure Time Analysis - ALL TRIPS BY INCOME ===\n")
cat("\n2019 - All Trips by Income:\n")
print(results_2019_all_income, n = Inf)
cat("\n2023 - All Trips by Income:\n")
print(results_2023_all_income, n = Inf)

cat("\n\n=== Departure Time Analysis - WORK TRIPS BY INCOME ===\n")
cat("\n2019 - Work Trips by Income:\n")
print(results_2019_work_income, n = Inf)
cat("\n2023 - Work Trips by Income:\n")
print(results_2023_work_income, n = Inf)



# Save combined results to CSV
output_filename <- sprintf("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023/departure_time_analysis_by_cycle_and_purpose_%s.csv",
                          format(Sys.time(), "%Y%m%d_%H%M%S"))
write_csv(all_results, output_filename)
cat("\nResults saved to:", output_filename, "\n")

# Create summary comparison
cat("\n\n=== Summary Statistics ===\n")
cat("\nALL TRIPS:\n")
cat("2019 - Total unweighted:", sum(results_2019_all$n_unweighted), "| Total weighted:", round(sum(results_2019_all$n_weighted)), "\n")
cat("2023 - Total unweighted:", sum(results_2023_all$n_unweighted), "| Total weighted:", round(sum(results_2023_all$n_weighted)), "\n")

cat("\nWORK TRIPS:\n")
cat("2019 - Total unweighted:", sum(results_2019_work$n_unweighted), "| Total weighted:", round(sum(results_2019_work$n_weighted)), "\n")
cat("2023 - Total unweighted:", sum(results_2023_work$n_unweighted), "| Total weighted:", round(sum(results_2023_work$n_weighted)), "\n")
