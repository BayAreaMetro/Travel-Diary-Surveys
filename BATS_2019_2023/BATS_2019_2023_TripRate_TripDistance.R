# -------------------------
# This trip rate and trip distance analysis is a person-day level analysis
# The universe is all adults (18+) because the 2019 survey was adult-only
# -------------------------

# Load required libraries
library(readr)
library(dplyr)
library(glue)
library(tidyr)
library(srvyr)

# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis"

# Start a log file
log_file <- glue("{working_dir}/BATS_multi_year_TripRate_TripDistance_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for person-day trip rate and trip distance calculations: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n")

# Set confidence level for all analyses
CONF_LEVEL <- 0.90 

# -------------------------
# REUSABLE FUNCTIONS
# -------------------------

#there are 4 functions:

# calculate_trip_metrics() - Calculates all trip rate/distance metrics for any grouping variable(s)
# standardize_results() - Standardizes column names across all results
# add_reliability_flags() - Adds quality control flags (CV, sample size, etc.)
# create_chart_labels() - Creates human-readable labels for charts


#' Calculate trip rate and distance metrics for any grouping variable(s)
#' 
#' @param survey_design A survey design object created with srvyr::as_survey_design
#' @param group_vars Character vector of grouping variable names (e.g., c("income_cat"), c("employment"))
#'                   Set to NULL for overall totals (all adults)
#' @param summary_level_name Name for this analysis level (e.g., "By Income", "By Employment")
#' @return A list of survey result dataframes
calculate_trip_metrics <- function(survey_design, group_vars = NULL, summary_level_name = "All adults", conf_level = CONF_LEVEL) {
  
  # Add survey_cycle to grouping vars if not already included
  if (!is.null(group_vars)) {
    group_vars <- c(group_vars, "survey_cycle")
  } else {
    group_vars <- "survey_cycle"
  }
  
  # List of metrics to calculate (means)
  metrics <- list(
    list(var = "personDay_dist_in_miles", name = "dist"),
    list(var = "personDay_dist_PbShMeSo_miles", name = "PbShMeSo_dist"),
    list(var = "num_trips", name = "trips"),
    list(var = "num_PbShMeSo_trips", name = "PbShMeSo_trips"),
    list(var = "num_EscOthPbSch_trips", name = "EscOthPbSch_trips"),
    list(var = "num_ShMeSo_trips", name = "ShMeSo_trips"),
    list(var = "num_MeSo_trips", name = "MeSo_trips"),
    list(var = "num_trips_HOME", name = "trips_HOME"),
    list(var = "num_trips_WORK", name = "trips_WORK"),
    list(var = "num_trips_SCHOOL", name = "trips_SCHOOL"),
    list(var = "num_trips_ESCORT", name = "trips_ESCORT"),
    list(var = "num_trips_PERS_BUS", name = "trips_PERS_BUS"),
    list(var = "num_trips_SHOP", name = "trips_SHOP"),
    list(var = "num_trips_MEAL", name = "trips_MEAL"),
    list(var = "num_trips_SOCREC", name = "trips_SOCREC"),
    list(var = "num_trips_OTHER", name = "trips_OTHER")
  )
  
  results <- list()
  
  # Calculate means for each metric
  for (metric in metrics) {
    result <- survey_design %>%
      group_by(!!!syms(group_vars)) %>%
      summarize(
        n_unweighted = unweighted(n()),
        n_weighted = survey_total(),
        !!paste0("mean_", metric$name) := survey_mean(.data[[metric$var]], vartype = c("se", "ci", "cv"), level = conf_level)
      ) %>%
      mutate(
        summary_col = metric$var,
        summary_level = summary_level_name
      )
    
    results[[metric$name]] <- result
  }
  
  # Calculate average trip length (ratio)
  avg_triplen <- survey_design %>%
    group_by(!!!syms(group_vars)) %>%
    summarize(
      n_unweighted = unweighted(n()),
      n_weighted = survey_total(),
      avg_trip_length = survey_ratio(
        numerator = personDay_dist_in_miles,
        denominator = num_trips,
        vartype = c("se", "ci", "cv"),
        level = conf_level
      )
    ) %>%
    mutate(
      summary_col = "avg_trip_length_miles",
      summary_level = summary_level_name
    )
  
  results[["avg_triplen"]] <- avg_triplen
  
  return(results)
}

#' Standardize survey results to common format
#' 
#' @param results_list List of survey result dataframes from calculate_trip_metrics
#' @param summary_level_name Name for this analysis level
#' @return A single dataframe with standardized columns
standardize_results <- function(results_list, summary_level_name) {
  
  standardized <- list()
  
  for (name in names(results_list)) {
    df <- results_list[[name]]
    
    # Get the mean column name (it varies by metric)
    mean_col <- grep("^mean_|^avg_", names(df), value = TRUE)[1]
    
    # Standardize column names
    df_std <- df %>%
      rename(
        mean = !!sym(mean_col),
        se = !!sym(paste0(mean_col, "_se")),
        ci_lower = !!sym(paste0(mean_col, "_low")),
        ci_upper = !!sym(paste0(mean_col, "_upp")),
        coeff_of_var = !!sym(paste0(mean_col, "_cv"))
      )
    
    standardized[[name]] <- df_std
  }
  
  # Combine all metrics
  combined <- bind_rows(standardized)
  
  return(combined)
}

#' Add reliability flags to survey results
#' 
#' @param summary_df Dataframe with survey results
#' @return Dataframe with added reliability columns
add_reliability_flags <- function(summary_df) {
  summary_df %>%
    mutate(
      unweighted_count = n_unweighted,
      weighted_count = n_weighted,
      ci_width = ci_upper - ci_lower,
      cv_flag = coeff_of_var > 0.30,
      sample_size_flag = unweighted_count < 30,
      suppress = cv_flag | sample_size_flag,
      estimate_reliability = case_when(
        cv_flag ~ "Poor (High CV >30%)",
        sample_size_flag ~ "Poor (Small sample n<30)",
        TRUE ~ "Acceptable"
      )
    )
}

#' Create chart labels for different grouping types
#' 
#' @param df Dataframe with survey results
#' @param group_vars Character vector of grouping variable names. If NULL, creates "All" label.
#'                   survey_cycle is automatically excluded from chart labels.
#' @return Dataframe with chart_label column added
create_chart_labels <- function(df, group_vars = NULL) {
  
  if (is.null(group_vars)) {
    # All adults - no grouping
    df <- df %>%
      mutate(chart_label = "All")
  } else {
    # Remove survey_cycle from group_vars for labeling
    label_vars <- setdiff(group_vars, "survey_cycle")
    
    # Build chart label by pasting all grouping variables together
    df <- df %>%
      unite("chart_label", all_of(label_vars), sep = " - ", remove = FALSE)
  }
  
  return(df)
}

# -------------------------
# DATA PREPARATION
# -------------------------

# Run the script that creates the person-day level dataset
source("E:/GitHub/Travel-Diary-Surveys/BATS_2019_2023/Create_PersonDay_df_with_demographic_and_strata_vars.R")
# Alternatively, read the processed file:
# ProcessedPersonDays_2019_2023_df <- read_csv("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis/ProcessedPersonDays_2019_2023.csv")

# Filter to adults (18+) - universe is adults only because 2019 survey was adult-only
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  filter(age >= 4)

# Drop if weight is 0 (to get correct unweighted count)
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  filter(pdexpfac > 0)

# Write PersonDays to csv for subsequent processes
#output_trips_csv <- glue("{working_dir}/PersonDays_2019_2023_Adults.csv")
#write.csv(ProcessedPersonDays_2019_2023_df, file = output_trips_csv, row.names = FALSE)
#print(glue("Wrote {nrow(ProcessedPersonDays_2019_2023_df)} rows to {output_trips_csv}"))

# -------------------------
# Create derived variables for analysis
# -------------------------

# Create commute categories
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  mutate(
    commute_cat = case_when(
      employment == 1 & commuted_on_travel_day == 1                           ~ "1. Commuted",
      employment == 1 & telecommute_time >= 360 & commuted_on_travel_day == 0 ~ "2. Telecommuted 6+ hours and not Commuted",
      employment == 1 & telecommute_time > 0 & commuted_on_travel_day == 0    ~ "3. Telecommuted <6 hours and not Commuted",
      employment == 1 & telecommute_time == 0 & commuted_on_travel_day == 0   ~ "4. Did not work",
      TRUE                                                                     ~ "5. Not full-time worker"
    )
  )



# -------------------------
# Create survey design object
# -------------------------

srv_design <- ProcessedPersonDays_2019_2023_df %>%
  as_survey_design(
    ids     = hhno, 
    weights = pdexpfac,
    strata = c(survey_cycle, stratification_var)
  )

# -------------------------
# RUN ANALYSES
# -------------------------

print("\n=== Running survey analyses ===\n")

# 1. All adults (overall)
print("Calculating metrics for all adults...")
results_all_adults <- calculate_trip_metrics(
  srv_design,
  group_vars = NULL,
  summary_level_name = "All adults"
)
summary_all_adults <- standardize_results(results_all_adults, "All adults") %>%
  add_reliability_flags() %>%
  create_chart_labels(group_vars = NULL)

# 2. By commute category
print("Calculating metrics by commute category...")
results_by_commute <- calculate_trip_metrics(
  srv_design,
  group_vars = "commute_cat",
  summary_level_name = "By Commute Category"
)
summary_by_commute <- standardize_results(results_by_commute, "By Commute Category") %>%
  add_reliability_flags() %>%
  create_chart_labels(group_vars = "commute_cat")

# 3. By county
print("Calculating metrics by county...")
results_by_county <- calculate_trip_metrics(
  srv_design,
  group_vars = "home_county_label",
  summary_level_name = "By County"
)
summary_by_county <- standardize_results(results_by_county, "By County") %>%
  add_reliability_flags() %>%
  create_chart_labels(group_vars = "home_county_label")

# 4. By commute category and county
print("Calculating metrics by commute category and county...")
results_commute_county <- calculate_trip_metrics(
  srv_design,
  group_vars = c("commute_cat", "home_county_label"),
  summary_level_name = "By Commute Category and County"
)
summary_commute_county <- standardize_results(results_commute_county, "By Commute Category and County") %>%
  add_reliability_flags() %>%
  create_chart_labels(group_vars = c("commute_cat", "home_county_label"))

# 5. By employment status
print("Calculating metrics by employment status...")
results_by_employment <- calculate_trip_metrics(
   srv_design,
   group_vars = "employment_label",
   summary_level_name = "By Employment Status"
 )
summary_by_employment <- standardize_results(results_by_employment, "By Employment Status") %>%
   add_reliability_flags() %>%
   create_chart_labels(group_vars = "employment_label")

# 7. By income (detailed)
print("Calculating metrics by income (detailed)...")
results_by_income_detailed <- calculate_trip_metrics(
   srv_design,
   group_vars = "income_detailed_label",
   summary_level_name = "By Income (detailed)"
 )
 summary_by_income_detailed <- standardize_results(results_by_income_detailed, "By Income (detailed)") %>%
   add_reliability_flags() %>%
   create_chart_labels(group_vars = "income_detailed_label")

# 8. By income (detailed, then grouped)
print("Calculating metrics by income...")
results_by_income4cat <- calculate_trip_metrics(
   srv_design,
   group_vars = "income_detailed_grouped",
   summary_level_name = "By Income"
 )
 summary_by_income4cat <- standardize_results(results_by_income4cat, "By Income") %>%
   add_reliability_flags() %>%
   create_chart_labels(group_vars = "income_detailed_grouped")

# 9. By race and ethnicity
print("Calculating metrics by race and ethnicity...")
results_by_race_eth <- calculate_trip_metrics(
   srv_design,
   group_vars = "race_eth",
   summary_level_name = "By Race and Ethnicity"
 )
 summary_by_race_eth <- standardize_results(results_by_race_eth, "By Race and Ethnicity") %>%
   add_reliability_flags() %>%
   create_chart_labels(group_vars = "race_eth")

# 10. By income and county
print("Calculating metrics by income and county...")
results_income4cat_county <- calculate_trip_metrics(
  srv_design,
  group_vars = c("income_detailed_grouped", "home_county_label"),
  summary_level_name = "By Income and County"
)
summary_income4cat_county <- standardize_results(results_income4cat_county, "By Income and County") %>%
  add_reliability_flags() %>%
  create_chart_labels(group_vars = c("income_detailed_grouped", "home_county_label"))

# 11. By race/ethnicity and county
print("Calculating metrics by race/ethnicity and county...")
results_raceEth_county <- calculate_trip_metrics(
  srv_design,
  group_vars = c("race_eth", "home_county_label"),
  summary_level_name = "By Race/Ethnicity and County"
)
summary_raceEth_county <- standardize_results(results_raceEth_county, "By Race/Ethnicity and County") %>%
  add_reliability_flags() %>%
  create_chart_labels(group_vars = c("race_eth", "home_county_label"))


# ADD NEW ANALYSES HERE


# Example: By age group (if you create age_group variable)
# print("Calculating metrics by age group...")
# results_by_age <- calculate_trip_metrics(
#   srv_design,
#   group_vars = "age_group",
#   summary_level_name = "By Age Group"
# )
# summary_by_age <- standardize_results(results_by_age, "By Age Group") %>%
#   add_reliability_flags() %>%
#   create_chart_labels(group_vars = "age_group")

# -------------------------
# COMBINE ALL RESULTS
# -------------------------

comprehensive_summary <- bind_rows(
  summary_all_adults,
  summary_by_commute,
  summary_by_county,
  summary_commute_county,
  summary_by_employment,
  summary_by_race_eth,
  summary_by_income_detailed,
  summary_by_income4cat,
  summary_income4cat_county,
  summary_raceEth_county
  # summary_by_age
)

# Reorder columns for clarity
comprehensive_summary <- comprehensive_summary %>%
  select(
    summary_col,
    summary_level,
    survey_cycle,
    # Include all possible grouping variables (will be NA if not used)
    any_of(c("commute_cat", "home_county_label", "income_cat", "employment", "age_group")),
    chart_label,
    mean,
    se,
    ci_width,
    ci_lower,
    ci_upper,
    coeff_of_var,
    estimate_reliability,
    unweighted_count,
    weighted_count
  ) %>%
  arrange(summary_col, summary_level, survey_cycle)

# -------------------------
# OUTPUT RESULTS
# -------------------------

# Display summary
print("\n=== Comprehensive Summary Table (first 20 rows) ===")
print(head(comprehensive_summary, 20))
print(glue("\nTotal rows in comprehensive summary: {nrow(comprehensive_summary)}"))

# Write comprehensive summary to CSV
output_summary_csv <- glue("{working_dir}/Summary_TripRate_TripDistance_{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")
write.csv(comprehensive_summary, file = output_summary_csv, row.names = FALSE)
print(glue("\nWrote comprehensive summary with {nrow(comprehensive_summary)} rows to {output_summary_csv}"))

# Display summary by estimate reliability
reliability_summary <- comprehensive_summary %>%
  group_by(summary_level, estimate_reliability) %>%
  summarize(count = n(), .groups = "drop")

print("\n=== Estimate Reliability Summary ===")
print(reliability_summary)

# Save comprehensive summary to Rdata file
output_rdata <- glue("{working_dir}/Summary_TripRate_TripDistance_{format(Sys.time(), '%Y%m%d_%H%M%S')}.Rdata")
save(comprehensive_summary, file = output_rdata)
print(glue("Wrote {nrow(comprehensive_summary)} rows to {output_rdata}"))


# Close the log file
sink()

print(glue("\nAnalysis complete! Log file saved to: {log_file}"))