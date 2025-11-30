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
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023"

# Start a log file
log_file <- glue("{working_dir}/calculate_telework_freq_shares{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for calculating telework frequency shares: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

# Run the script that load the person level dataset
source("E:/GitHub/Travel-Diary-Surveys/BATS_2019_2023/Load_Person_df_with_demographic_and_strata_vars.R")
# alternatively, one can just read the output from the above process

# -------------------------
# Universe is all workers (full-time, part-time, self-employed, volunteer)
# -------------------------
person_2019_2023_df <- person_2019_2023_df %>% 
  filter(employment_label %in% c("1. Employed full-time (paid)", 
                                  "2. Employed part-time (paid)", 
                                  "3. Self-employed", 
                                  "6. Unpaid volunteer or intern")) 

# explicitly drop all 0 weight records
person_2019_2023_df <- person_2019_2023_df %>% 
   filter(person_weight_rmove_only>0) %>%
   filter(!is.na(telework_jobtype3_label)) 

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
# FUNCTION: Calculate telework shares by demographic segment
# -------------------------
calculate_telework_by_segment <- function(srv_design, segment_var, segment_label) {
  
  cat("\n")
  print(glue("=== Processing segmentation by: {segment_label} ==="))

  # Table 1: Detailed telework frequency by segment
  results_detailed <- srv_design %>%
    group_by(survey_cycle, {{segment_var}}, telework_jobtype3_label) %>%
    summarize(
      n_unweighted = unweighted(n()),
      n_weighted = survey_total(vartype = NULL),
      proportion = survey_prop(vartype = c("se", "ci", "cv"))
    )

  # Table 2: 3-category telework frequency by segment
  results_3cat <- srv_design %>%
    group_by(survey_cycle, {{segment_var}}, telework_freq_3cat_label) %>%
    summarize(
      n_unweighted = unweighted(n()),
      n_weighted = survey_total(vartype = NULL),
      proportion = survey_prop(vartype = c("se", "ci", "cv"))
    )

  # Table 3: WFH 2+ days share by segment
  results_wfh2plus <- srv_design %>%
    group_by(survey_cycle, {{segment_var}}) %>%
    summarize(
      n_total_unweighted = unweighted(n()),
      total_weighted = survey_total(vartype = NULL),
      n_WFH2OrMoreDays_unweighted = unweighted(sum(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week")),
      WFH2OrMoreDays_weighted = survey_total(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week", vartype = NULL),
      WFH2OrMoreDays_share = survey_mean(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week", 
                                         vartype = c("se", "ci", "cv"))
    )
  
  # Print results
  print(glue("\n=== Telework Frequency Shares (Detailed) by {segment_label} ==="))
  print(results_detailed, n = Inf)
  
  print(glue("\n=== Telework Frequency Shares (3-Category) by {segment_label} ==="))
  print(results_3cat, n = Inf)
  
  print(glue("\n=== WFH 2+ Days Share by {segment_label} ==="))
  print(results_wfh2plus, n = Inf)
  
  # Save results with timestamp
  timestamp <- format(Sys.time(), '%Y%m%d_%H%M%S')
  write_csv(results_detailed, glue("{working_dir}/telework_freq_detailed_by_{segment_label}_{timestamp}.csv"))
  write_csv(results_3cat, glue("{working_dir}/telework_freq_3cat_by_{segment_label}_{timestamp}.csv"))
  write_csv(results_wfh2plus, glue("{working_dir}/WFH2OrMoreDays_by_{segment_label}_{timestamp}.csv"))
  
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
    proportion = survey_prop(vartype = c("se", "ci", "cv"))
  )

print("\n=== Telework Frequency Shares (Overall - Detailed) ===")
print(srv_results_telework_freq1, n = Inf)

# Table 2: Shares by telework freq categories (3-category)
srv_results_telework_freq2 <- srv_design %>%
  group_by(survey_cycle, telework_freq_3cat_label) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(vartype = NULL),
    proportion = survey_prop(vartype = c("se", "ci", "cv"))
  )

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
                                       vartype = c("se", "ci", "cv"))
  )

print("\n=== Remote or Hybrid Worker Share (Overall) ===")
print(srv_results_WFH2OrMoreDays_share, n = Inf)

# Save overall results
timestamp <- format(Sys.time(), '%Y%m%d_%H%M%S')
write_csv(srv_results_telework_freq1, glue("{working_dir}/telework_freq_shares_overall_{timestamp}.csv"))
write_csv(srv_results_WFH2OrMoreDays_share, glue("{working_dir}/WFH2OrMoreDays_share_overall_{timestamp}.csv"))

# -------------------------
# SEGMENTED ANALYSIS - BY DEMOGRAPHICS
# -------------------------

# By Employment Status
employment_results <- calculate_telework_by_segment(srv_design, employment_label, "employment_label")

# By Gender
gender_results <- calculate_telework_by_segment(srv_design, gender_label, "gender_label")

# By Income
income_results <- calculate_telework_by_segment(srv_design, income_detailed_grouped, "income_detailed_grouped")

# By County
county_results <- calculate_telework_by_segment(srv_design, home_county_grouped_label, "home_county_grouped_label")


# -------------------------
# Summary message
# -------------------------
print("\n=== Analysis Complete ===")
print(glue("All results saved to: {working_dir}"))
print(glue("Log file: {log_file}"))

sink()
