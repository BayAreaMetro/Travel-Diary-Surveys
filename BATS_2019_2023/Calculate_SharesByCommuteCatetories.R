# -------------------------
# This is a person-day level analysis
# the universe of analysis is full-time workers who either commuted, or worked 7+ hour day at home
# -------------------------


# Load required libraries
library(readr)
library(dplyr)
library(glue)
library(tidyr) # to use replace_na


# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis"

# Start a log file
log_file <- glue("{working_dir}/BATS_SharesByCommuteCat_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

# -------------------------
# The unit of analysis is person-day level
# -------------------------

# Run the script that create the person-day level dataset
source("E:/GitHub/Travel-Diary-Surveys/BATS_2019_2023/Create_PersonDay_df_with_demographic_and_strata_vars.R")
# alternatively, one can just read the output from the above process:
# PersonDays_2019_2023_df <- read_csv("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis/ProcessedPersonDays_2019_2023.csv")

# all adults (18+) because the 2019 survey was adult-only
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  filter(age>=4)

# Drop if weight is 0 (to get correct unweighted count), although only have negligible impacts on the weighted shares, se, ci, cv calc (4 or 5 digits after the decimal)
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  filter(pdexpfac>0)

# Write PersonDays_2019_2023_df to csv for subsequent processes
output_trips_csv <- glue("{working_dir}/PersonDays_2019_2023_Adults.csv")
write.csv(ProcessedPersonDays_2019_2023_df, file = output_trips_csv, row.names = FALSE)
print(glue("Wrote {nrow(ProcessedPersonDays_2019_2023_df)} rows to {output_trips_csv}"))

#-----------------------------------------
# Create 5 "commute categories" 
# - full time workers who commuted
# - full time workers who telecommuted (4+ hours)
# - full time workers who telecommuted (less than 4 hours)
# - full time workres who didn't work (took time off or sick day)
# - adults (18+) who are not full time workers
#
# if someone both commuted and telecommuted, they will be counted as commuted
#-----------------------------------------
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  mutate(
    commute_cat = case_when(
      employment == 1 & commuted_on_travel_day == 1                           ~ "1. Commuted",
      employment == 1 & telecommute_time >= 420 & commuted_on_travel_day == 0 ~ "2. Telecommuted 7+ hours and not Commuted",
      employment == 1 & telecommute_time > 0 & commuted_on_travel_day == 0    ~ "3. Telecommuted <7 hours and not Commuted",
      employment == 1 & telecommute_time == 0 & commuted_on_travel_day == 0   ~ "4. Did not work",
      TRUE                                                                    ~ "5. Not full-time worker" 
    )
  ) 

# the universe of analysis is full-time workers who either commuted, or worked 7+ hour day at home
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  filter(employment==1)   %>%  
  filter(commute_cat == "1. Commuted" | commute_cat == "2. Telecommuted 7+ hours and not Commuted") %>%
  filter(pdexpfac > 0) 

# -------------------------
# Calculate shares by commute_cat with SE, CI, CV
# -------------------------

library(srvyr)

# Create survey design object
srv_design <- ProcessedPersonDays_2019_2023_df %>%
  as_survey_design(
    weights = pdexpfac,
    strata = c(survey_cycle, stratification_var)
  )


# Shares by commute_cat (proportions)
srv_results_commute_cat_shares <- srv_design %>%
  group_by(survey_cycle, commute_cat) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(vartype = c("se", "ci", "cv")),
    proportion = survey_prop(vartype = c("se", "ci", "cv"))
  )

# Display results
print("\n=== Commute Category Shares ===")
print(srv_results_commute_cat_shares)

# Process for summary table
commute_cat_shares_summary <- srv_results_commute_cat_shares %>%
  mutate(
    summary_col = "commute_category_share",
    summary_level = "By Commute Category",
    home_county_label = NA_character_,
    chart_label = paste(survey_cycle, " - ", case_when(
      commute_cat == "1. Commuted"              ~ "Commuted",
      commute_cat == "2. Telecommuted 7+ hours and not Commuted" ~ "Telecommuted 7+ hours",
      commute_cat == "3. Telecommuted <7 hours and not Commuted" ~ "Telecommuted <7 hours",
      commute_cat == "4. Did not work"          ~ "Did not work",
      commute_cat == "5. Not full-time worker"  ~ "Not full-time worker",
      TRUE ~ "Unknown"
    )),
    # Rename columns to match comprehensive_summary structure
    mean = proportion,
    se = proportion_se,
    ci_lower_95 = proportion_low,
    ci_upper_95 = proportion_upp,
    coeff_of_var = proportion_cv,
    unweighted_count = n_unweighted,
    weighted_count = n_weighted,
    # Calculate CI width
    ci_95 = ci_upper_95 - ci_lower_95,
    # Calculate reliability flags
    cv_flag = coeff_of_var > 0.30,
    sample_size_flag = unweighted_count < 30,
    ci_width_flag = ci_95 > 0.40,  # CI width > 40 percentage points
    extreme_values_flag = ci_lower_95 < 0 | ci_upper_95 > 1,
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
    summary_col,
    summary_level,
    survey_cycle,
    commute_cat,
    home_county_label,
    chart_label,
    mean,
    se,
    ci_95,
    ci_lower_95,
    ci_upper_95,
    coeff_of_var,
    estimate_reliability,
    unweighted_count,
    weighted_count
  )


# Display results
commute_cat_shares_summary <- commute_cat_shares_summary %>%
  arrange(summary_col, summary_level, survey_cycle, commute_cat)

print("\n=== Commute Category Shares Summary ===")
print(commute_cat_shares_summary)


# Write updated comprehensive summary to CSV
output_summary_with_shares_csv <- glue("{working_dir}/Summary_CommuteCatShares_{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")
write.csv(commute_cat_shares_summary, file = output_summary_with_shares_csv, row.names = FALSE)
print(glue("\nWrote comprehensive summary with shares ({nrow(commute_cat_shares_summary)} rows) to {output_summary_with_shares_csv}"))