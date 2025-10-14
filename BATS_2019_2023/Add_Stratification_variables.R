
# Load required libraries
library(readr)
library(dplyr)
library(glue)
library(stringr) # so I can use str_sub()


# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023"

# Read the linked trip data with distance appended (this is the output from BATS_2019_2023_geocode_OD_to_TAZ.R)
LinkedTrips_2019_2023_df <- read.csv(glue("{working_dir}/LinkedTrips_2019_2023_withDist.csv"))

# Display basic information about the dataset
cat("Dataset loaded.\n")
cat("Dimensions:", nrow(LinkedTrips_2019_2023_df), "rows x", ncol(LinkedTrips_2019_2023_df), "columns\n")

# Number of rows by survey_cycle
cat("\n\nNumber of rows by survey_cycle:\n")
table(LinkedTrips_2019_2023_df$survey_cycle)

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
hh2019_df <- read_table(hh2019_path)

hh2019_df <- hh2019_df %>%
  select(hh_id, sample_stratum) %>%
  mutate(survey_cycle = 2019) %>%
  rename(hhno = hh_id,
         stratification_var = sample_stratum)

# --- person2019 ---
person2019_file <- "person.tsv"
person2019_path <- file.path(background_dataset_2019_dir, person2019_file)
person2019_df <- read_table(person2019_path)

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

# Write LinkedTrips_2019_2023_df to csv for subsequent processes
output_trips_csv <- glue("{working_dir}/LinkedTrips_2019_2023_withDist_withStrata.csv")
write.csv(LinkedTrips_2019_2023_df, file = output_trips_csv, row.names = FALSE)
print(glue("Wrote {nrow(LinkedTrips_2019_2023_df)} rows to {output_trips_csv}"))

