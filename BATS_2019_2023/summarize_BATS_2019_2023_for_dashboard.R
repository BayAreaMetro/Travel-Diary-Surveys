# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(glue)
library(srvyr)
library(stringr) # so I can use str_sub()

# -------------------------
# Read BATS data
# -------------------------

# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023"

# Start a log file
log_file <- glue("{working_dir}/multi-cycle_BATS_processing_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for creating a multi-cycle BATS dataset: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

# Read 2023 linked trip file
# Suppress progress bar for cleaner log output
LinkedTrips2023_df <- read_csv("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/TripLinking_20251012/03b-assign_day/wt-wkday_4day/trip.csv",
                                progress = FALSE) %>% 
  mutate(survey_cycle = 2023)

print(glue("LinkedTrips2023_df:"))
print(glue("  Observations: {nrow(LinkedTrips2023_df)}"))
print(glue("  Columns: {ncol(LinkedTrips2023_df)}"))
cat("\n")

# Read 2019 linked trip file
# Note that the 2019 file is space delimited
LinkedTrips2019_df <- read.table("E:/Box/Modeling and Surveys/Surveys/Travel Diary Survey/MPO Partner Household Travel Survey (Includes 2018_2019 TNC Survey)/Bay Area Travel Study 2018-2019/Data from 2018_2019 Survey/BATS2018_2019_SFCTA_processed/20200228/survey2018_tripx.dat",
                                  header = TRUE,
                                  sep = "") %>% 
  mutate(survey_cycle = 2019)

print(glue("LinkedTrips2019_df:"))
print(glue("  Observations: {nrow(LinkedTrips2019_df)}"))
print(glue("  Columns: {ncol(LinkedTrips2019_df)}"))
cat("\n")

# Union the two dataframes
LinkedTrips_2019_2023_df <- bind_rows(LinkedTrips2023_df, LinkedTrips2019_df)

# Check the combined data frame
print(glue("LinkedTrips_2019_2023_df:"))
print(glue("  Observations: {nrow(LinkedTrips_2019_2023_df)}"))
print(glue("  Columns: {ncol(LinkedTrips_2019_2023_df)}"))
cat("\n")

print(glue("Observations by survey cycle:"))
print(table(LinkedTrips_2019_2023_df$survey_cycle))
cat("\n")

# Check column differences
cols_2023_only <- setdiff(names(LinkedTrips2023_df), names(LinkedTrips2019_df))
cols_2019_only <- setdiff(names(LinkedTrips2019_df), names(LinkedTrips2023_df))
cols_common <- intersect(names(LinkedTrips2023_df), names(LinkedTrips2019_df))

print(glue("\nColumn comparison:"))
print(glue("  Columns in 2023 only ({length(cols_2023_only)}): {paste(cols_2023_only, collapse=', ')}"))
print(glue("  Columns in 2019 only ({length(cols_2019_only)}): {paste(cols_2019_only, collapse=', ')}"))
print(glue("  Columns in both ({length(cols_common)}): {paste(cols_common, collapse=', ')}"))
cat("\n")

# Check if trexpfac and pdexpfac are identical
are_identical <- identical(LinkedTrips2019_df$trexpfac, LinkedTrips2019_df$pdexpfac)
print(glue("Are trexpfac and pdexpfac identical? {are_identical}"))

# If not identical, show differences
if (!are_identical) {
  num_differences <- sum(LinkedTrips2019_df$trexpfac != LinkedTrips2019_df$pdexpfac, na.rm = TRUE)
  print(glue("  Number of differing values: {num_differences}"))
}


# -------------------------
# Bring in background variables so we can define the strata 
# -------------------------

# Read 2023 household data
# Referencing the unweighted datase, since this is the most upstreamed dataset
# See config in the weighting repo: https://github.com/BayAreaMetro/mtc_hts_weighting_scripts/blob/c93f914167af534b7340bdad5f4dff5895d18598/configs/project_settings.yaml#L10
background_dataset_2023_dir <- "E://Box/Modeling and Surveys/Surveys/Travel Diary Survey/BATS_2023/MTC_RSG_Partner Repository/5.Deliverables/Task 10 - Weighting and Expansion Data Files/UnweightedDataset"

hh2023_file <- "hh.csv"
hh2023_path <- file.path(background_dataset_2023_dir, hh2023_file)
hh2023_df <- read_csv(hh2023_path)

hh2023_df <- hh2023_df %>%
  select(hh_id, sample_segment) %>%
  mutate(survey_cycle = 2023) %>%
  rename(hhno = hh_id,
         stratification_var = sample_segment)

# --- person2023 ---
person2023_file <- "person.csv"
person2023_path <- file.path(background_dataset_2023_dir, person2023_file)
person2023_df <- read_csv(person2023_path)

person2023_df <- person2023_df %>%
  select(hh_id, person_id, age, employment, telework_freq) %>%
  mutate(survey_cycle = 2023) %>%
  rename(hhno = hh_id) %>%
  mutate(pno = as.numeric(str_sub(person_id, -2, -1)))  


# Read 2019 data
background_dataset_2019_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations/Final Updated Dataset as of 10-18-2021"

# --- hh2019 ---
hh2019_file <- "hh.tsv"
hh2019_path <- file.path(background_dataset_2019_dir, hh2019_file)
hh2019_df <- read_tsv(hh2019_path)

hh2019_df <- hh2019_df %>%
  select(hh_id, sample_stratum) %>%
  mutate(survey_cycle = 2019) %>%
  rename(hhno = hh_id,
         stratification_var = sample_stratum)

# --- person2019 ---
person2019_file <- "person.tsv"
person2019_path <- file.path(background_dataset_2019_dir, person2019_file)
person2019_df <- read_tsv(person2019_path)

person2019_df <- person2019_df %>%
  select(hh_id, person_id, age, employment, telework_freq) %>%
  mutate(survey_cycle = 2019) %>%
  rename(hhno = hh_id) %>%
  mutate(pno = as.numeric(str_sub(person_id, -2, -1)))  

# Union the two cycles
hh_2019_2023_df <- bind_rows(hh2019_df, hh2023_df)
person_2019_2023_df <- bind_rows(person2019_df, person2023_df)

# Join to LinkedTrips_2019_2023_df
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  left_join(hh_2019_2023_df, by = c("hhno", "survey_cycle"))

LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  left_join(person_2019_2023_df, by = c("hhno", "pno", "survey_cycle"))

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

# Add Commuted_on_travel_day variable
# For each hhno, pno, and day: if at least one trip has dpurp == 1 (WORK), 
# then Commuted_on_travel_day = 1, otherwise 0
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  group_by(hhno, pno, day) %>%
  mutate(Commuted_on_travel_day = as.integer(any(dpurp == 1))) %>%
  ungroup()

# -------------------------
# Calculate shares
# Following the structure in https://github.com/BayAreaMetro/transit-passenger-surveys/blame/master/summaries/summarize_snapshot_2023_for_dashboard.R
# -------------------------

# Function to summarize BATS data by attribute
summarize_for_attr <- function(survey_data, summary_col, strata_vars = c("survey_cycle", "stratification_var")) {
  summary_col_str <- as_label(enquo(summary_col))
  print(glue("===== Summarizing for {summary_col_str}"))
  
  return_table <- tibble()
  
  # Summarize by survey cycle
  for (summary_level in c("survey_cycle")) {  
    
    print(glue("Summarizing for summary_level={summary_level}, summary_col={summary_col_str}"))
    
    # Filter to rows where data exists
    data_to_summarize <- survey_data %>% 
      filter(
        !is.na(!!sym(summary_level)) &
        !is.na({{ summary_col }}) &
        !is.na(trexpfac) &
        (trexpfac > 0)  # Exclude records with zero weight
      )
    
    print(glue("  Filtered to {nrow(data_to_summarize)} records"))
    
    # Calculate actual unweighted counts by group and category
    actual_counts <- data_to_summarize %>%
      group_by(across(all_of(c(summary_level, summary_col_str)))) %>%
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
    # Add additional strata if available (e.g., county, household size)
    srv_design <- df_dummy %>%
      as_survey_design(
        weights = trexpfac, 
        strata = all_of(strata_vars)
      )
    
    # Compute within-group weighted shares and counts
    srv_results <- srv_design %>%
      group_by(across(all_of(summary_level))) %>%
      summarise(
        across(
          starts_with("pref_"),
          ~ survey_mean(.x, vartype = c("se", "ci", "cv")),
          .names = "{.col}_{.fn}"
        ),
        # Add total counts for each group
        total_weighted = survey_total(vartype = "se"),
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
          stat_type == "_1_low" ~ "ci_lower_95",
          stat_type == "_1_upp" ~ "ci_upper_95",
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
        ci_95 = ci_upper_95 - weighted_share,
        summary_level = summary_level,
        summary_col = summary_col_str
      ) %>%
      # Join with actual unweighted counts
      left_join(
        actual_counts,
        by = setNames(c(summary_level, summary_col_str), c(summary_level, summary_col_str))
      ) %>%
      rename(
        weighted_count = weighted_count_actual, 
        unweighted_count = unweighted_count_actual
      )
    
    # Apply criteria for poor estimate reliability 
    # Same as: https://github.com/BayAreaMetro/transit-passenger-surveys/blob/master/summaries/summarize_snapshot_2023_for_dashboard.R)
    srv_results <- srv_results %>%
      mutate(
        unweighted_count = replace_na(unweighted_count, 0),
        weighted_count = replace_na(weighted_count, 0),
        # Calculate poor estimate reliability flags
        cv_flag = coeff_of_var > 0.30,  # CV > 30%
        sample_size_flag = unweighted_count < 30,  # Minimum sample size
        ci_width_flag = (ci_upper_95 - ci_lower_95) > 0.40,  # CI width > 40pp
        extreme_values_flag = ci_lower_95 < 0 | ci_upper_95 > 1,  # Impossible values
        
        # Overall poor reliability decision
        suppress = cv_flag | sample_size_flag | ci_width_flag | extreme_values_flag,
        
        # Create consolidated estimate reliability flag
        estimate_reliability = case_when(
          cv_flag ~ "Poor (High CV >30%)",
          sample_size_flag ~ "Poor (Small sample n<30)",
          ci_width_flag ~ "Poor (Wide CI >40pp)",
          extreme_values_flag ~ "Poor (Invalid range)",
          TRUE ~ "Acceptable"
        )
      ) %>%
      select(
        all_of(summary_level), all_of(summary_col_str),
        weighted_share, se, ci_95, ci_lower_95, ci_upper_95, coeff_of_var,
        weighted_count, unweighted_count, total_weighted, total_unweighted,
        estimate_reliability, summary_level, summary_col
      )
    
    print("Results:")
    print(srv_results, n = 30)
    
    # Add to return table
    return_table <- bind_rows(return_table, srv_results)
  }
  
  print("===== End of summarizing")
  return(return_table)
}

# Summarize mode (as 4 categories)
mode4cat_summary <- summarize_for_attr(LinkedTrips_2019_2023_df, mode4cat_label) %>%
  mutate(universe = "All trips")

# Summarize mode (as 4 categories) by those commuted that day
mode4cat_Commuted_summary <- summarize_for_attr(LinkedTrips_2019_2023_df %>% filter(employment == 1 & Commuted_on_travel_day == 1), mode4cat_label) %>%
  mutate(universe = "Trips by full-time employed persons who commuted that day")

# Summarize mode (as 4 categories) by those commuted that day
mode4cat_NotCommuted_summary <- summarize_for_attr(LinkedTrips_2019_2023_df %>% filter(employment == 1 & Commuted_on_travel_day == 0), mode4cat_label) %>%
  mutate(universe = "Trips by full-time employed persons who did not commute that day")  

# Summarize mode
mode_summary <- summarize_for_attr(LinkedTrips_2019_2023_df, mode_label) %>%
  mutate(universe = "All trips")

# Summarize destination purpose
dpurp_summary <- summarize_for_attr(LinkedTrips_2019_2023_df, dpurp_label) %>%
  mutate(universe = "All trips")

# Combine summaries
full_summary <- bind_rows(mode4cat_summary, mode4cat_Commuted_summary, mode4cat_NotCommuted_summary, mode_summary, dpurp_summary)

# Add formatted unweighted count string
full_summary <- full_summary %>% 
  mutate(
    total_unweighted_str = paste0("N=", prettyNum(total_unweighted, big.mark = ",", scientific = FALSE))
  )

# reorder variables
full_summary <- full_summary %>%
  select(
    survey_cycle,
    summary_col,
    universe, 
    mode4cat_label,
    mode_label,
    dpurp_label,  
    weighted_share,
    se,
    ci_95,
    ci_lower_95,
    ci_upper_95,
    coeff_of_var,
    unweighted_count,   
    weighted_count,
    total_unweighted,
    total_unweighted_str,
    total_weighted,
    estimate_reliability,
    summary_level
  )

# Save results
output_file <- glue("{working_dir}/summarize_BATS_2019_2023_for_dashboard{format(Sys.time(), '%Y%m%d_%H%M%S')}.Rdata")
save(full_summary, file = output_file)
print(glue("Wrote {nrow(full_summary)} rows to {output_file}"))

output_csv <- glue("{working_dir}/summarize_BATS_2019_2023_for_dashboard{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")
write.csv(full_summary, file = output_csv, row.names = FALSE)
print(glue("Wrote {nrow(full_summary)} rows to {output_csv}"))


sink() # to close the log file connection


# Write LinkedTrips_2019_2023_df to csv for checking
#output_trips_csv <- glue("{working_dir}/LinkedTrips_2019_2023.csv")
#write.csv(LinkedTrips_2019_2023_df, file = output_trips_csv, row.names = FALSE)
#print(glue("Wrote {nrow(LinkedTrips_2019_2023_df)} rows to {output_trips_csv}"))