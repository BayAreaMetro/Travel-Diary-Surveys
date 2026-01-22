# -------------------------
# This mode share analysis is a trip level analysis
# Note that the key focus is trips by all adults (18+) because the 2019 survey was adult-only
# -------------------------


# Load required libraries
library(readr)
library(dplyr)
library(glue)
library(tidyr) # to use replace_na
library(srvyr)


# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis"

# Set confidence level for all analyses
CONF_LEVEL <- 0.90 

# Start a log file
log_file <- glue("{working_dir}/BATS_multi_year_Shares_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for trip-level mode, purpose etc share calculations: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

print(glue("Confidence Level: {CONF_LEVEL * 100}%"))
cat("\n")

# -------------------------
# The unit of analysis is trip level
# -------------------------

# Run the script that create the trip level dataset
source("E:/GitHub/Travel-Diary-Surveys/BATS_2019_2023/Add_Stratification_Variables_And_Labels_to_LinkedTrips.R")
# alternatively, one can just read the output from the above process:
# PersonDays_2019_2023_df <- read_csv("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis/LinkedTrips_2019_2023_withDist_withStrata.csv")

# Drop if weight is 0 (to get correct unweighted count), although this only have negligible impacts on the weighted shares, se, ci, cv calc (4 or 5 digits after the decimal)
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  filter(trexpfac > 0)

# Add Commuted_on_travel_day variable
# For each hhno, pno, and day: if at least one trip has dpurp == 1 (WORK), 
# then Commuted_on_travel_day = 1, otherwise 0
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  group_by(hhno, pno, day) %>%
  mutate(Commuted_on_travel_day = as.integer(any(dpurp == 1))) %>%
  ungroup()

# Add age group variable for analysis
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(adult_yn = case_when(
    age < 4 ~ "Under 18",
    age >= 4 ~ "18 and over",
    TRUE ~ NA_character_
  ))

#-----------------------------------------
# Create 5 "commute categories" 
# - full time workers who commuted
# - full time workers who telecommuted (6+ hours)
# - full time workers who telecommuted (less than 6 hours)
# - full time workres who didn't work (took time off or sick day)
# - adults (18+) who are not full time workers
#
# if someone both commuted and telecommuted, they will be counted as commuted
#-----------------------------------------
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(
    commute_cat = case_when(
      employment == 1 & Commuted_on_travel_day == 1                           ~ "1. Commuted",
      employment == 1 & telecommute_time >= 360 & Commuted_on_travel_day == 0 ~ "2. Telecommuted 6+ hours and not Commuted",
      employment == 1 & telecommute_time > 0 & Commuted_on_travel_day == 0    ~ "3. Telecommuted <6 hours and not Commuted",
      employment == 1 & telecommute_time == 0 & Commuted_on_travel_day == 0   ~ "4. Did not work",
      TRUE                                                                    ~ "5. Not full-time worker" 
    )
  ) 

# -------------------------
# Calculate shares
# Following the structure in https://github.com/BayAreaMetro/transit-passenger-surveys/blame/master/summaries/summarize_snapshot_2023_for_dashboard.R
# -------------------------

# Function to summarize BATS data by attribute
summarize_for_attr <- function(survey_data, summary_col, strata_vars = c("survey_cycle", "stratification_var"), 
                               summary_levels = c("survey_cycle"), conf_level = CONF_LEVEL) {
  summary_col_str <- as_label(enquo(summary_col))
  print(glue("===== Summarizing for {summary_col_str}"))
  
  return_table <- tibble()
  
  # Summarize by specified levels
  for (summary_level in summary_levels) {  
    
    # Split summary_level by comma to handle multiple grouping variables
    group_vars <- trimws(strsplit(summary_level, ",")[[1]])
    
    print(glue("Summarizing for summary_level={summary_level}, summary_col={summary_col_str}"))
    
    # Filter to rows where data exists
    data_to_summarize <- survey_data %>% 
      filter(
        across(all_of(group_vars), ~ !is.na(.x)),  # Check all grouping vars are not NA
        !is.na({{ summary_col }}) &
        !is.na(trexpfac) &
        (trexpfac > 0)  # Exclude records with zero weight
      )
    
    print(glue("  Filtered to {nrow(data_to_summarize)} records"))
    
    # Calculate actual unweighted counts by group and category
    actual_counts <- data_to_summarize %>%
      group_by(across(all_of(c(group_vars, summary_col_str)))) %>%
      summarise(
        weighted_count_actual = sum(trexpfac), 
        unweighted_count_actual = n(), 
        .groups = "drop"
      )
    
    # Create dummy variables for each level of summary_col
    df_dummy <- data_to_summarize %>%
      mutate(across(all_of(summary_col_str), as.factor)) %>%
      mutate(dummy = 1) %>%
      pivot_wider(
        names_from = all_of(summary_col_str),
        values_from = dummy,
        values_fill = 0,
        names_prefix = "pref_"
      )
    
    # Create survey design with stratification
    srv_design <- df_dummy %>%
      as_survey_design(
        ids     = c(hhno, pno),
        weights = trexpfac, 
        strata = all_of(strata_vars)
      )
    
    # Compute within-group weighted shares and counts
    srv_results <- srv_design %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        across(
          starts_with("pref_"),
          ~ survey_mean(.x, vartype = c("se", "ci", "cv"), level = conf_level),
          .names = "{.col}_{.fn}"
        ),
        total_weighted = survey_total(),
        total_unweighted = unweighted(n()),
        .groups = 'drop'
      )
    
    # Reshape to get attribute as column with weighted_share, SE, and CI as separate columns
    srv_results <- srv_results %>%
      pivot_longer(
        cols = starts_with("pref_"),
        names_pattern = "^pref_(.*)(_1|_1_se|_1_low|_1_upp|_1_cv)$",
        names_to = c(summary_col_str, "stat_type"),
        values_to = "value"
      ) %>%
      mutate(
        stat_type = case_when(
          stat_type == "_1" ~ "weighted_share",
          stat_type == "_1_se" ~ "se",
          stat_type == "_1_low" ~ "ci_lower",
          stat_type == "_1_upp" ~ "ci_upper",
          stat_type == "_1_cv"  ~ "coeff_of_var"
        ))
    
    # Pivot the stat_type back to columns
    srv_results <- srv_results %>%
      pivot_wider(
        names_from = stat_type,
        values_from = value
      )
    
    # Calculate additional metrics and merge actual counts
    srv_results <- srv_results %>%
      mutate(
        ci_width = ci_upper - ci_lower,
        summary_level = summary_level,
        summary_col = summary_col_str,
        confidence_level = conf_level
      ) %>%
      # Join with actual unweighted counts
      left_join(
        actual_counts,
        by = c(group_vars, summary_col_str)
      ) %>%
      rename(
        weighted_count = weighted_count_actual, 
        unweighted_count = unweighted_count_actual
      )
    
    # Apply criteria for poor estimate reliability 
    srv_results <- srv_results %>%
      mutate(
        unweighted_count = replace_na(unweighted_count, 0),
        weighted_count = replace_na(weighted_count, 0),
        cv_flag = coeff_of_var > 0.30,
        sample_size_flag = unweighted_count < 30,
        ci_width_flag = (ci_upper - ci_lower) > 0.40,
        extreme_values_flag = ci_lower < 0 | ci_upper > 1,
        suppress = cv_flag | sample_size_flag | ci_width_flag | extreme_values_flag,
        estimate_reliability = case_when(
          cv_flag ~ "Poor (High CV >30%)",
          sample_size_flag ~ "Poor (Small sample n<30)",
          ci_width_flag ~ "Poor (Wide CI >40pp)",
          extreme_values_flag ~ "Poor (Invalid range)",
          TRUE ~ "Acceptable"
        )
      ) %>%
      select(
        all_of(group_vars), all_of(summary_col_str),
        weighted_share, se, ci_width, ci_lower, ci_upper, coeff_of_var,
        weighted_count, unweighted_count, total_weighted, total_unweighted, confidence_level,
        estimate_reliability, summary_level, summary_col
      )
    
    print("Results:")
    print(srv_results, n = 30)
    
    return_table <- bind_rows(return_table, srv_results)
  }
  
  print("===== End of summarizing")
  return(return_table)
}

mode_summary <- summarize_for_attr(
  LinkedTrips_2019_2023_df, 
  mode_label,
  summary_levels = c("survey_cycle", "adult_yn", "commute_cat",
                     "survey_cycle,adult_yn", 
                     "survey_cycle,income_detailed_grouped,adult_yn",
                     "survey_cycle,race_eth,adult_yn",
#                     "survey_cycle,home_county_label,adult_yn",
                    "survey_cycle,disability_status_label,adult_yn",
                    "survey_cycle,disability_type_label,adult_yn",
                     "survey_cycle,home_county_label_grouped,adult_yn",
                     "survey_cycle,home_county_label_grouped,income_detailed_grouped,adult_yn",
                     "survey_cycle,home_county_label_grouped,race_eth,adult_yn",
                    "survey_cycle,home_county_label_grouped,disability_status_label,adult_yn",
                    "survey_cycle,home_county_label_grouped,disability_type_label,adult_yn",
                     "survey_cycle,home_county_label_grouped2,adult_yn",
                     "survey_cycle,home_county_label_grouped2,income_detailed_grouped,adult_yn",
                     "survey_cycle,home_county_label_grouped2,race_eth,adult_yn",
                     "survey_cycle,home_county_label_grouped2,disability_status_label,adult_yn",
                     "survey_cycle,home_county_label_grouped2,disability_type_label,adult_yn",
                     "survey_cycle,commute_cat,adult_yn")
)

mode5cat_summary <- summarize_for_attr(
  LinkedTrips_2019_2023_df, 
  mode5cat_label,
  summary_levels = c("survey_cycle", "adult_yn", "commute_cat",
                     "survey_cycle,adult_yn", 
                     "survey_cycle,income_detailed_grouped,adult_yn",
                     "survey_cycle,race_eth,adult_yn",
#                     "survey_cycle,home_county_label,adult_yn",
                    "survey_cycle,disability_status_label,adult_yn",
                    "survey_cycle,disability_type_label,adult_yn",
                     "survey_cycle,home_county_label_grouped,adult_yn",
                     "survey_cycle,home_county_label_grouped,income_detailed_grouped,adult_yn",
                     "survey_cycle,home_county_label_grouped,race_eth,adult_yn",
                    "survey_cycle,home_county_label_grouped,disability_status_label,adult_yn",
                    "survey_cycle,home_county_label_grouped,disability_type_label,adult_yn",
                     "survey_cycle,home_county_label_grouped2,adult_yn",
                     "survey_cycle,home_county_label_grouped2,income_detailed_grouped,adult_yn",
                     "survey_cycle,home_county_label_grouped2,race_eth,adult_yn",
                     "survey_cycle,home_county_label_grouped2,disability_status_label,adult_yn",
                     "survey_cycle,home_county_label_grouped2,disability_type_label,adult_yn",
                     "survey_cycle,commute_cat,adult_yn")
)

dpurp_summary <- summarize_for_attr(
  LinkedTrips_2019_2023_df, 
  dpurp_label,
  summary_levels = c("survey_cycle", "adult_yn", "commute_cat",
                     "survey_cycle,adult_yn", 
                     "survey_cycle,income_detailed_grouped,adult_yn",
                     "survey_cycle,race_eth,adult_yn",
#                     "survey_cycle,home_county_label,adult_yn",
                    "survey_cycle,disability_status_label,adult_yn",
                    "survey_cycle,disability_type_label,adult_yn",
                     "survey_cycle,home_county_label_grouped,adult_yn",
                     "survey_cycle,home_county_label_grouped,income_detailed_grouped,adult_yn",
                     "survey_cycle,home_county_label_grouped,race_eth,adult_yn",
                    "survey_cycle,home_county_label_grouped,disability_status_label,adult_yn",
                    "survey_cycle,home_county_label_grouped,disability_type_label,adult_yn",
                     "survey_cycle,home_county_label_grouped2,adult_yn",
                     "survey_cycle,home_county_label_grouped2,income_detailed_grouped,adult_yn",
                     "survey_cycle,home_county_label_grouped2,race_eth,adult_yn",
                     "survey_cycle,home_county_label_grouped2,disability_status_label,adult_yn",
                     "survey_cycle,home_county_label_grouped2,disability_type_label,adult_yn",
                     "survey_cycle,commute_cat,adult_yn")
)

trip_dist_bin_summary <- summarize_for_attr(
  LinkedTrips_2019_2023_df, 
  trip_dist_bin,
  summary_levels = c("survey_cycle", "adult_yn", "commute_cat",
                     "survey_cycle,adult_yn", 
                     "survey_cycle,income_detailed_grouped,adult_yn",
                     "survey_cycle,race_eth,adult_yn",
#                     "survey_cycle,home_county_label,adult_yn",
                    "survey_cycle,disability_status_label,adult_yn",
                    "survey_cycle,disability_type_label,adult_yn",
                     "survey_cycle,home_county_label_grouped,adult_yn",
                     "survey_cycle,home_county_label_grouped,income_detailed_grouped,adult_yn",
                     "survey_cycle,home_county_label_grouped,race_eth,adult_yn",
                    "survey_cycle,home_county_label_grouped,disability_status_label,adult_yn",
                    "survey_cycle,home_county_label_grouped,disability_type_label,adult_yn",
                     "survey_cycle,home_county_label_grouped2,adult_yn",
                     "survey_cycle,home_county_label_grouped2,income_detailed_grouped,adult_yn",
                     "survey_cycle,home_county_label_grouped2,race_eth,adult_yn",
                     "survey_cycle,home_county_label_grouped2,disability_status_label,adult_yn",
                     "survey_cycle,home_county_label_grouped2,disability_type_label,adult_yn",
                     "survey_cycle,commute_cat,adult_yn")
)

# Function to replace NA values with descriptive labels
replace_na_with_labels <- function(df) {
  result <- df
  
  # Only replace county labels with "Bay Area" if ALL three county columns are NA
  has_county_label <- "home_county_label" %in% names(result)
  has_county_grouped <- "home_county_label_grouped" %in% names(result)
  has_county_grouped2 <- "home_county_label_grouped2" %in% names(result)
  
  if (has_county_label | has_county_grouped | has_county_grouped2) {
    # Build the condition by checking each column that exists
    result <- result %>%
      mutate(all_county_na = TRUE)  # Start with TRUE
    
    if (has_county_label) {
      result <- result %>%
        mutate(all_county_na = all_county_na & is.na(home_county_label))
    }
    if (has_county_grouped) {
      result <- result %>%
        mutate(all_county_na = all_county_na & is.na(home_county_label_grouped))
    }
    if (has_county_grouped2) {
      result <- result %>%
        mutate(all_county_na = all_county_na & is.na(home_county_label_grouped2))
    }
    
    # Replace with "Bay Area" only if all are NA
    if (has_county_label) {
      result <- result %>%
        mutate(home_county_label = if_else(all_county_na, "Bay Area", home_county_label))
    }
    if (has_county_grouped) {
      result <- result %>%
        mutate(home_county_label_grouped = if_else(all_county_na, "Bay Area", home_county_label_grouped))
    }
    if (has_county_grouped2) {
      result <- result %>%
        mutate(home_county_label_grouped2 = if_else(all_county_na, "Bay Area", home_county_label_grouped2))
    }
    
    result <- result %>% select(-all_county_na)
  }

  if ("income_detailed_grouped" %in% names(result)) {
    result <- result %>%
      mutate(income_detailed_grouped = if_else(is.na(income_detailed_grouped), "All Income Levels", income_detailed_grouped))
  }
  
  if ("race_eth" %in% names(result)) {
    result <- result %>%
      mutate(race_eth = if_else(is.na(race_eth), "All Race/Ethnicity Groups", race_eth))
  }
  
  if ("disability_status_label" %in% names(result)) {
  result <- result %>%
    mutate(disability_status_label = if_else(is.na(disability_status_label), 
                                              "All Disability Statuses", 
                                              disability_status_label))
  }

  if ("disability_type_label" %in% names(result)) {
  result <- result %>%
    mutate(disability_type_label = if_else(is.na(disability_type_label), 
                                            "All Disability Types", 
                                            disability_type_label))
  }

  if ("adult_yn" %in% names(result)) {
    result <- result %>%
      mutate(adult_yn = if_else(is.na(adult_yn), "All Ages", adult_yn))
  }
  
  if ("commute_cat" %in% names(result)) {
    result <- result %>%
      mutate(commute_cat = if_else(is.na(commute_cat), "All Commute Categories", commute_cat))
  }
  
  return(result)
}

# Apply to all summary tables
mode_summary <- replace_na_with_labels(mode_summary)
mode5cat_summary <- replace_na_with_labels(mode5cat_summary)
dpurp_summary <- replace_na_with_labels(dpurp_summary)
trip_dist_bin_summary <- replace_na_with_labels(trip_dist_bin_summary)



# Combine summaries
full_summary <- bind_rows(mode5cat_summary, mode_summary, dpurp_summary, trip_dist_bin_summary)

# Add formatted unweighted count string
full_summary <- full_summary %>% 
  mutate(
    total_unweighted_str = paste0("N=", prettyNum(total_unweighted, big.mark = ",", scientific = FALSE))
  )

# reorder variables
full_summary <- full_summary %>%
  select(
    survey_cycle,
    adult_yn,       
    commute_cat,
    income_detailed_grouped,  
    race_eth,  
    disability_status_label,    
    disability_type_label,      
    #home_county_label,
    home_county_label_grouped,
    home_county_label_grouped2,
    summary_col,
    mode5cat_label,
    mode_label,
    dpurp_label, 
    trip_dist_bin,  
    weighted_share,
    se,
    ci_width,
    ci_lower,
    ci_upper,
    coeff_of_var,
    unweighted_count,   
    weighted_count,
    total_unweighted,
    total_unweighted_str,
    total_weighted,
    confidence_level, 
    estimate_reliability,
    summary_level
  )

# Sort the output
full_summary <- full_summary %>%
  arrange(
    survey_cycle,
    adult_yn,
    commute_cat,
    income_detailed_grouped, 
    race_eth,
    disability_status_label,    
    disability_type_label,  
    #home_county_label,
    home_county_label_grouped,
    home_county_label_grouped2, 
    summary_col,
    mode5cat_label,
    mode_label,
    dpurp_label,
    trip_dist_bin
  )

# Save results
output_file <- glue("{working_dir}/summarize_BATS_TripLevel_Share_2019_2023_for_dashboard{format(Sys.time(), '%Y%m%d_%H%M%S')}.Rdata")
save(full_summary, file = output_file)
print(glue("Wrote {nrow(full_summary)} rows to {output_file}"))

output_csv <- glue("{working_dir}/summarize_BATS_TripLevel_Share_2019_2023_for_dashboard{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")
write.csv(full_summary, file = output_csv, row.names = FALSE)
print(glue("Wrote {nrow(full_summary)} rows to {output_csv}"))


sink() # to close the log file connection