# -----------
# Calculate calculate telework_freq share
# Universe is all workers (full-time, part-time, self-employed, volunteer)
# Unit is person-level
# -----------

# Load required libraries
library(readr)
library(dplyr)
library(glue)

# -------------------------
# Initial set up and read the person-level data frame
# -------------------------

# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis"

# Set confidence level for all analyses
CONF_LEVEL <- 0.90 

# Start a log file
log_file <- glue("{working_dir}/calculate_telework_freq_shares{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for calculating telework frequency shares: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

print(glue("Confidence Level: {CONF_LEVEL * 100}%"))
cat("\n")

# Run the script that load the person level dataset
source("E:/GitHub/Travel-Diary-Surveys/BATS_2019_2023/Load_Person_df_with_demographic_and_strata_vars.R")
# alternatively, one can just read the output from the above process

# -------------------------
# Universe is all workers (full-time, part-time, self-employed, volunteer)
# -------------------------

cat("\nBefore Any Filter:\n")
person_2019_2023_df %>%
  group_by(survey_cycle) %>%
  summarise(n_records = n(),
            sum_weight = sum(person_weight_rmove_only, na.rm = TRUE),
            .groups = 'drop') %>%
  print()

# Employment filter
person_2019_2023_df <- person_2019_2023_df %>% 
  filter(employment_label %in% c("1. Employed full-time (paid)", 
                                  "2. Employed part-time (paid)", 
                                  "3. Self-employed"))

cat("\nAfter Employment Filter:\n")
person_2019_2023_df %>%
  group_by(survey_cycle) %>%
  summarise(n_records = n(),
            sum_weight = sum(person_weight_rmove_only, na.rm = TRUE),
            .groups = 'drop') %>%
  print()


# explicitly drop all 0 weight records
person_2019_2023_df <- person_2019_2023_df %>% 
   filter(person_weight_rmove_only>0)
cat("\nAfter Weight > 0 Filter:\n")
person_2019_2023_df %>%
  group_by(survey_cycle) %>%
  summarise(n_records = n(),
            sum_weight = sum(person_weight_rmove_only, na.rm = TRUE),
            .groups = 'drop') %>%
  print()

# Telework x job_type3 variable has to be valid
person_2019_2023_df <- person_2019_2023_df %>%
   filter(!is.na(telework_jobtype3_label))
cat("\nAfter Telework x job_type3 Filter:\n")
person_2019_2023_df %>%
  group_by(survey_cycle) %>%
  summarise(n_records = n(),
            sum_weight = sum(person_weight_rmove_only, na.rm = TRUE),
            .groups = 'drop') %>%
  print()


# -------------------------
# simple tabulation
# -------------------------
simple_table <- person_2019_2023_df %>%
  group_by(survey_cycle, telework_jobtype3_label) %>%
  summarise(
    count = n(),
    total_weight = sum(person_weight_rmove_only, na.rm = TRUE)
  )

# -------------------------
# Calculate shares by commute_cat with SE, CI, CV
# -------------------------

library(srvyr)

# Create survey design object
srv_design <- person_2019_2023_df %>%
  as_survey_design(
    ids     = hh_id,
    weights = person_weight_rmove_only,
    strata =  stratification_var
  )
  
# -------------------------
# ENHANCED FUNCTION: Calculate telework shares by one or more demographic segments
# -------------------------
calculate_telework_by_segment <- function(srv_design, segment_vars, segment_label) {
  
  cat("\n")
  print(glue("=== Processing segmentation by: {segment_label} ==="))

  # Table 1: Detailed telework frequency by segment(s)
  results_detailed <- srv_design %>%
    group_by(survey_cycle, !!!segment_vars, telework_jobtype3_label) %>%
    summarize(
      n_unweighted = unweighted(n()),
      n_weighted = survey_total(vartype = NULL),
      proportion = survey_prop(vartype = c("se", "ci", "cv"), level = CONF_LEVEL)
    )  %>%
    mutate(segment_type = segment_label) 

  # Table 2: 3-category telework frequency by segment(s)
  results_3cat <- srv_design %>%
    group_by(survey_cycle, !!!segment_vars, telework_freq_jobtype3_temp) %>%
    summarize(
      n_unweighted = unweighted(n()),
      n_weighted = survey_total(vartype = NULL),
      proportion = survey_prop(vartype = c("se", "ci", "cv"), level = CONF_LEVEL)
    )  %>%
    mutate(segment_type = segment_label) 

  # Table 3: WFH 2+ days share by segment(s)
  results_wfh2plus <- srv_design %>%
    group_by(survey_cycle, !!!segment_vars) %>%
    summarize(
      n_total_unweighted = unweighted(n()),
      total_weighted = survey_total(vartype = NULL),
      n_WFH2OrMoreDays_unweighted = unweighted(sum(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week")),
      WFH2OrMoreDays_weighted = survey_total(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week", vartype = NULL),
      WFH2OrMoreDays_share = survey_mean(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week", 
                                         vartype = c("se", "ci", "cv"), level = CONF_LEVEL)
    )  %>%
    mutate(segment_type = segment_label) 
  
  # Print results
  print(glue("\n=== Telework Frequency Shares (Detailed) by {segment_label} ==="))
  print(results_detailed, n = Inf)
  
  print(glue("\n=== Telework Frequency Shares (3-Category) by {segment_label} ==="))
  print(results_3cat, n = Inf)
  
  print(glue("\n=== WFH 2+ Days Share by {segment_label} ==="))
  print(results_wfh2plus, n = Inf)
  
  return(list(detailed = results_detailed, 
              three_cat = results_3cat,
              wfh2plus = results_wfh2plus))
}

# -------------------------
# OVERALL ANALYSIS (no segmentation)
# -------------------------

# Table 1: Shares by telework freq categories (detailed proportions)
srv_results_telework_freq1 <- srv_design %>%
  group_by(survey_cycle, telework_jobtype3_label) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(vartype = NULL),
    proportion = survey_prop(vartype = c("se", "ci", "cv"), level = CONF_LEVEL)
  )  %>%
  mutate(segment_type = "Overall", segment_value = "All Workers")

print("\n=== Telework Frequency Shares (Overall - Detailed) ===")
print(srv_results_telework_freq1, n = Inf)

# Table 2: Shares by telework freq categories (even more detailed)
srv_results_telework_freq2 <- srv_design %>%
  group_by(survey_cycle, telework_freq_jobtype3_temp) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(vartype = NULL),
    proportion = survey_prop(vartype = c("se", "ci", "cv"), level = CONF_LEVEL)
  ) %>%
  mutate(segment_type = "Overall", segment_value = "All Workers")

print("\n=== Telework Frequency Shares (Overall - 3 Category) ===")
print(srv_results_telework_freq2, n = Inf)

# Table 3: Remote or Hybrid share (overall)
srv_results_WFH2OrMoreDays_share <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_total_unweighted = unweighted(n()),
    total_weighted = survey_total(vartype = NULL),
    n_WFH2OrMoreDays_unweighted = unweighted(sum(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week")),
    WFH2OrMoreDays_weighted = survey_total(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week", vartype = NULL),
    WFH2OrMoreDays_share = survey_mean(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week", 
                                       vartype = c("se", "ci", "cv"), level = CONF_LEVEL)
  ) %>%
  mutate(segment_type = "Overall", segment_value = "All Workers")

print("\n=== Remote or Hybrid Worker Share (Overall) ===")
print(srv_results_WFH2OrMoreDays_share, n = Inf)

# -------------------------
# SEGMENTED ANALYSIS - BY DEMOGRAPHICS
# -------------------------

# By Employment Status
#employment_results <- calculate_telework_by_segment(srv_design, employment_label, "employment_label")
employment_results <- calculate_telework_by_segment(srv_design, rlang::quos(employment_label), "employment_label")

# By Gender
#gender_results <- calculate_telework_by_segment(srv_design, gender_label, "gender_label")
gender_results <- calculate_telework_by_segment(srv_design, rlang::quos(gender_label), "gender_label")

# By Income
#income_results <- calculate_telework_by_segment(srv_design, income_detailed_grouped, "income_detailed_grouped")
income_results <- calculate_telework_by_segment(srv_design, rlang::quos(income_detailed_grouped), "income_detailed_grouped")

# By County
#county_results <- calculate_telework_by_segment(srv_design, home_county_grouped_label, "home_county_grouped_label")
county_results <- calculate_telework_by_segment(srv_design, rlang::quos(home_county_grouped_label), "home_county_grouped_label")

# By Education
#education_results <- calculate_telework_by_segment(srv_design, education_grouped_label, "education_grouped_label")
education_results <- calculate_telework_by_segment(srv_design, rlang::quos(education_grouped_label), "education_grouped_label")

# By Industry (for 2023 only)
#industry_results <- calculate_telework_by_segment(srv_design, industry_label, "industry_label")
industry_results <- calculate_telework_by_segment(srv_design, rlang::quos(industry_label), "industry_label")

# By Occupation (for 2023 only)
#occupation_results <- calculate_telework_by_segment(srv_design, occupation_label, "occupation_label")
occupation_results <- calculate_telework_by_segment(srv_design, rlang::quos(occupation_label), "occupation_label")

# By County x Income
county_income_results <- calculate_telework_by_segment(
  srv_design, 
  rlang::quos(home_county_grouped_label, income_detailed_grouped), 
  "home_county_grouped_label x income_detailed_grouped"
)

# By County x Education
county_education_results <- calculate_telework_by_segment(
  srv_design,
  rlang::quos(home_county_grouped_label, education_grouped_label),
  "home_county_grouped_label x education_grouped_label"
)

# By County x Employment
county_employment_results <- calculate_telework_by_segment(
  srv_design,
  rlang::quos(home_county_grouped_label, employment_label),
  "home_county_grouped_label x employment_label"
)

# -------------------------
# CONSOLIDATE AND SAVE RESULTS
# -------------------------

# Helper function to rename segment column to "segment_value"
rename_segment_col <- function(df) {
  standard_cols <- c("survey_cycle", "telework_jobtype3_label", "telework_freq_jobtype3_temp",
                     "n_unweighted", "n_weighted", "proportion", "proportion_se", 
                     "proportion_low", "proportion_upp", "proportion_cv",
                     "n_total_unweighted", "total_weighted", "n_WFH2OrMoreDays_unweighted",
                     "WFH2OrMoreDays_weighted", "WFH2OrMoreDays_share", "WFH2OrMoreDays_share_se",
                     "WFH2OrMoreDays_share_low", "WFH2OrMoreDays_share_upp", "WFH2OrMoreDays_share_cv",
                     "segment_type", "segment_value")
  
  cols <- names(df)
  segment_cols <- setdiff(cols, standard_cols)
  
  # If multiple segment columns, combine them into one
  if (length(segment_cols) > 0) {
    df <- df %>% 
      unite("segment_value", all_of(segment_cols), sep = " | ", remove = TRUE)
  }
  
  return(df)
}

# Consolidate all detailed results
# Consolidate all detailed results
consolidated_detailed <- bind_rows(
  srv_results_telework_freq1,
  rename_segment_col(employment_results$detailed),
  rename_segment_col(gender_results$detailed),
  rename_segment_col(income_results$detailed),
  rename_segment_col(county_results$detailed),
  rename_segment_col(education_results$detailed),
  rename_segment_col(industry_results$detailed),
  rename_segment_col(occupation_results$detailed),
  # Add multi-dimensional results
  rename_segment_col(county_income_results$detailed),
  rename_segment_col(county_education_results$detailed),
  rename_segment_col(county_employment_results$detailed)
)

# Consolidate all 3-cat results
consolidated_3cat <- bind_rows(
  srv_results_telework_freq2,
  rename_segment_col(employment_results$three_cat),
  rename_segment_col(gender_results$three_cat),
  rename_segment_col(income_results$three_cat),
  rename_segment_col(county_results$three_cat),
  rename_segment_col(education_results$three_cat),
  rename_segment_col(industry_results$three_cat),
  rename_segment_col(occupation_results$three_cat),
  # Add multi-dimensional results
  rename_segment_col(county_income_results$three_cat),
  rename_segment_col(county_education_results$three_cat),
  rename_segment_col(county_employment_results$three_cat)
)

# Consolidate all WFH 2+ days results
consolidated_wfh2plus <- bind_rows(
  srv_results_WFH2OrMoreDays_share,
  rename_segment_col(employment_results$wfh2plus),
  rename_segment_col(gender_results$wfh2plus),
  rename_segment_col(income_results$wfh2plus),
  rename_segment_col(county_results$wfh2plus),
  rename_segment_col(education_results$wfh2plus),
  rename_segment_col(industry_results$wfh2plus),
  rename_segment_col(occupation_results$wfh2plus),
  # Add multi-dimensional results
  rename_segment_col(county_income_results$wfh2plus),
  rename_segment_col(county_education_results$wfh2plus),
  rename_segment_col(county_employment_results$wfh2plus)
)

# Reorder columns for better readability
consolidated_detailed <- consolidated_detailed %>%
  select(segment_type, segment_value, survey_cycle, telework_jobtype3_label, everything())

consolidated_3cat <- consolidated_3cat %>%
  select(segment_type, segment_value, survey_cycle, telework_freq_jobtype3_temp, everything())

consolidated_wfh2plus <- consolidated_wfh2plus %>%
  select(segment_type, segment_value, survey_cycle, everything())

# Save consolidated results
timestamp <- format(Sys.time(), '%Y%m%d_%H%M%S')
write_csv(consolidated_detailed, glue("{working_dir}/telework_freq_DETAILED_all_segments_{timestamp}.csv"))
write_csv(consolidated_3cat, glue("{working_dir}/telework_freq_MoreDETAILED_all_segments_{timestamp}.csv"))
write_csv(consolidated_wfh2plus, glue("{working_dir}/WFH2OrMoreDays_all_segments_{timestamp}.csv"))

# Save as RData files
save(consolidated_detailed, file = glue("{working_dir}/telework_freq_DETAILED_all_segments_{timestamp}.RData"))
save(consolidated_3cat, file = glue("{working_dir}/telework_freq_MoreDETAILED_all_segments_{timestamp}.RData"))
save(consolidated_wfh2plus, file = glue("{working_dir}/WFH2OrMoreDays_all_segments_{timestamp}.RData"))

# -------------------------
# Summary message
# -------------------------
print("\n=== Analysis Complete ===")
print(glue("Consolidated results saved to: {working_dir}"))
print(glue("  - telework_freq_DETAILED_all_segments_{timestamp}.csv"))
print(glue("  - telework_freq_MoreDETAILED_all_segments_{timestamp}.csv"))
print(glue("  - WFH2OrMoreDays_all_segments_{timestamp}.csv"))
print(glue("Log file: {log_file}"))

sink()
