# This script generate a person file with demographic and stratification variables

# Load required libraries
library(readr)
library(dplyr)
library(glue)
library(stringr) # so I can use str_sub()
library(tidyr)   # so I can use replace_na

# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023"

# Start a log file
log_file <- glue("{working_dir}/LoadPersonFile_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for loading person file with demographic and stratification variables: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

# Read 2023 household data
# Reading from the weighted database
background_dataset_2023_dir <- "X:/survey_repos/ProjRoot_Mon-Thu20251011/WgtRoot_Mon-Thu20251011/output/full_weighted_dataset"
hh2023_file <- "hh.csv"
hh2023_path <- file.path(background_dataset_2023_dir, hh2023_file)
hh2023_df <- read_csv(hh2023_path)

hh2023_df <- hh2023_df %>%
  select(hh_id, sample_segment, home_lon, home_lat, income_broad) %>%
  mutate(survey_cycle = 2023) %>%
  rename(stratification_var = sample_segment)

# --- person2023 ---
person2023_file <- "person.csv"
person2023_path <- file.path(background_dataset_2023_dir, person2023_file)
person2023_df <- read_csv(person2023_path)

person2023_df <- person2023_df %>%
  select(hh_id, person_id, person_weight_rmove_only, age, employment, telework_freq, work_lat, work_lon) %>%
  mutate(survey_cycle = 2023)

  # Read 2019 data
background_dataset_2019_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations/Final Updated Dataset as of 10-18-2021"

# --- hh2019 ---
hh2019_file <- "hh.tsv"
hh2019_path <- file.path(background_dataset_2019_dir, hh2019_file)
hh2019_df <- read_tsv(hh2019_path)

hh2019_df <- hh2019_df %>%
  select(hh_id, sample_stratum, reported_home_lat, reported_home_lon, income_aggregate) %>%
  mutate(survey_cycle = 2019) %>%
  rename(home_lat = reported_home_lat,
         home_lon = reported_home_lon,
         stratification_var = sample_stratum)

# --- person2019 ---
person2019_file <- "person.tsv"
person2019_path <- file.path(background_dataset_2019_dir, person2019_file)
person2019_df <- read_tsv(person2019_path)

# which is the right weight variables in the 2019 dataset
# it seems it should be wt_sphone_wkday
# because Wkday weights are zeroed where travel_date_dow > 4
# based on this: https://github.com/BayAreaMetro/tnc_california_studies/blob/6486dffc1ca9c42e9ec682d054ce56ccff9bf370/8.1_PopSim_weighting/02_join_weights.R#L229
person2019_df <- person2019_df %>%
  select(hh_id, person_id, wt_sphone_wkday, age, employment, telework_freq, work_lat, work_lon) %>%
  mutate(survey_cycle = 2019) %>%
  rename(person_weight_rmove_only = wt_sphone_wkday)

# Union the two cycles
hh_2019_2023_df <- bind_rows(hh2019_df, hh2023_df)
person_2019_2023_df <- bind_rows(person2019_df, person2023_df)

# Join to person_2019_2023_df
person_2019_2023_df <- person_2019_2023_df %>%
  left_join(hh_2019_2023_df, by = c("hh_id", "survey_cycle"))


# ------------------
# Add labels
# ------------------
person_2019_2023_df <- person_2019_2023_df %>%
  mutate(
    employment_label = case_when(
      employment == 1 ~ "1. Employed full-time (paid)",
      employment == 2 ~ "2. Employed part-time (paid)",
      employment == 3 ~ "3. Self-employed",
      employment == 5 ~ "4. Not employed and not looking for work",
      employment == 6 ~ "5. Unemployed and looking for work",
      employment == 7 ~ "6. Unpaid volunteer or intern",
      employment == 8 ~ "7. Employed, but not currently working",
      employment == 995 ~ "Missing Response",
      TRUE ~ "Other"
    )
  )

person_2019_2023_df <- person_2019_2023_df %>%
  mutate(income2023_label = case_when(
    survey_cycle == 2023 & income_broad == 1   ~ "Under $25,000",
    survey_cycle == 2023 & income_broad == 2   ~ "$25,000-$49,999",
    survey_cycle == 2023 & income_broad == 3   ~ "$50,000-$74,999",
    survey_cycle == 2023 & income_broad == 4   ~ "$75,000-$99,999",
    survey_cycle == 2023 & income_broad == 5   ~ "$100,000-$199,999",
    survey_cycle == 2023 & income_broad == 6   ~ "$200,000 or more",
    survey_cycle == 2023 & income_broad == 995 ~ "Missing Response",
    survey_cycle == 2023 & income_broad == 999 ~ "Prefer not to answer",
    TRUE ~ NA_character_  # For other survey cycles or other values
  ))

person_2019_2023_df <- person_2019_2023_df %>%
  mutate(income2019_label = case_when(
    survey_cycle == 2019 & income_aggregate == 1   ~ "Under $25,000",
    survey_cycle == 2019 & income_aggregate == 2   ~ "$25,000-$49,999",
    survey_cycle == 2019 & income_aggregate == 3   ~ "$50,000-$74,999",
    survey_cycle == 2019 & income_aggregate == 4   ~ "$75,000-$99,999",
    survey_cycle == 2019 & income_aggregate == 5   ~ "$100,000-$249,999", #not the same band as BATS2023
    survey_cycle == 2019 & income_aggregate == 6   ~ "$250,000 or more",  #not the same band as BATS2023
    survey_cycle == 2019 & income_aggregate == 999 ~ "Prefer not to answer",
    TRUE ~ NA_character_  # For other survey cycles or other values
  ))

person_2019_2023_df <- person_2019_2023_df %>%
  mutate(income_label = case_when(
    survey_cycle == 2019 & income_aggregate == 1   ~ "1. Under $25,000",
    survey_cycle == 2019 & income_aggregate == 2   ~ "2. $25,000-$49,999",
    survey_cycle == 2019 & income_aggregate == 3   ~ "3. $50,000-$74,999",
    survey_cycle == 2019 & income_aggregate == 4   ~ "4. $75,000-$99,999",
    survey_cycle == 2019 & income_aggregate == 5   ~ "5. $100,000 or more ",
    survey_cycle == 2019 & income_aggregate == 6   ~ "5. $100,000 or more ",
    survey_cycle == 2019 & income_aggregate == 999 ~ "Missing",
    survey_cycle == 2023 & income_broad == 1       ~ "1. Under $25,000",
    survey_cycle == 2023 & income_broad == 2       ~ "2. $25,000-$49,999",
    survey_cycle == 2023 & income_broad == 3       ~ "3. $50,000-$74,999",
    survey_cycle == 2023 & income_broad == 4       ~ "4. $75,000-$99,999",
    survey_cycle == 2023 & income_broad == 5       ~ "5. $100,000 or more ",
    survey_cycle == 2023 & income_broad == 6       ~ "5. $100,000 or more ",
    survey_cycle == 2023 & income_broad == 995     ~ "Missing",
    survey_cycle == 2023 & income_broad == 999     ~ "Missing",
    TRUE ~ NA_character_  # For other survey cycles or other values
  ))