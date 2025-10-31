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
   filter(!is.na(telework_freq_grouped_label)) 

# -------------------------
# simple tabulation
# -------------------------
simple_table <- person_2019_2023_df %>%
  group_by(survey_cycle, telework_freq_grouped_label) %>%
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


# Table 1: Shares by telework freq categories (proportions)
srv_results_telework_freq1 <- srv_design %>%
  group_by(survey_cycle, telework_freq_grouped_label) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(vartype = NULL),
    proportion = survey_prop(vartype = c("se", "ci", "cv"))
  )


# Display results
print("\n=== Telecommute Frequency Shares ===")
print(srv_results_telework_freq1, n = Inf)

# Table 2: Shares by telework freq categories (proportions)
srv_results_telework_freq2 <- srv_design %>%
  group_by(survey_cycle, telework_freq_3cat_label) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(vartype = NULL),
    proportion = survey_prop(vartype = c("se", "ci", "cv"))
  )


# Display results
print("\n=== Telecommute Frequency Shares ===")
print(srv_results_telework_freq2, n = Inf)


# Table 3: Shares by telework freq categories -- BY HOME COUNTY
srv_results_telework_freq_COUNTY <- srv_design %>%
  group_by(survey_cycle, telework_freq_3cat_label2, home_county_grouped_label) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(vartype = NULL),
    proportion = survey_prop(vartype = c("se", "ci", "cv"))
  )


# Display results
print("\n=== Telecommute Frequency Shares ===")
print(srv_results_telework_freq_COUNTY, n = Inf)


# Save to CSV with timestamp (only for the telework_freq_grouped_label1), as I decided that Table 2 is too aggregate
output_file <- glue("{working_dir}/telework_freq_shares_{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")
write_csv(srv_results_telework_freq1, output_file)


# Save to CSV with timestamp BY HOME COUNTY
output_file <- glue("{working_dir}/telework_freq_shares_ByCounty{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")
write_csv(srv_results_telework_freq_COUNTY, output_file)

# -------------------------
# Table 4: Calculate Remote or Hybrid share
# Sum of weight for those Remote or Hybrid / total workers
# -------------------------

# Remote or Hybrid share by survey cycle
srv_results_WFH2OrMoreDays_share <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_total_unweighted = unweighted(n()),
    total_weighted = survey_total(vartype = NULL),
    n_WFH2OrMoreDays_unweighted = unweighted(sum(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week")),
    WFH2OrMoreDays_weighted = survey_total(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week", vartype = NULL),
    WFH2OrMoreDays_share = survey_mean(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week", vartype = c("se", "ci", "cv"))
  )

# Display results
print("\n=== Remote or Hybrid Worker Share (Overall) ===")
print(srv_results_WFH2OrMoreDays_share, n = Inf)

# Remote or Hybrid share by survey cycle and county
srv_results_WFH2OrMoreDays_share_county <- srv_design %>%
  group_by(survey_cycle, home_county_grouped_label) %>%
  summarize(
    n_total_unweighted = unweighted(n()),
    total_weighted = survey_total(vartype = NULL),
    n_WFH2OrMoreDays_unweighted = unweighted(sum(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week")),
    WFH2OrMoreDays_weighted = survey_total(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week", vartype = NULL),
    WFH2OrMoreDays_share = survey_mean(telework_freq_3cat_label2 == "1. Work from home 2 or more days a week", vartype = c("se", "ci", "cv"))
  )

# Display results
print("\n=== Remote or Hybrid Worker Share (By County) ===")
print(srv_results_WFH2OrMoreDays_share_county, n = Inf)

# Save Remote or Hybrid share results to CSV
output_file <- glue("{working_dir}/WFH2OrMoreDays_share_{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")
write_csv(srv_results_WFH2OrMoreDays_share, output_file)

output_file <- glue("{working_dir}/WFH2OrMoreDays_share_ByCounty_{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")
write_csv(srv_results_WFH2OrMoreDays_share_county, output_file)

# TODO: reformat to add CV acceptance etc.

sink()
