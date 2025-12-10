
# Load required libraries
library(readr)
library(dplyr)
library(glue)
library(stringr) # so I can use str_sub()


# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis"

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
  select(hh_id, sample_segment, income_detailed) %>%
  mutate(survey_cycle = 2023) %>%
  rename(hhno = hh_id,
         stratification_var = sample_segment)

# --- person2023 ---
person2023_file <- "person.csv"
person2023_path <- file.path(background_dataset_2023_dir, person2023_file)
person2023_df <- read_csv(person2023_path)

person2023_df <- person2023_df %>%
  select(
    hh_id, person_id, age, employment, telework_freq,
    ethnicity_1, ethnicity_2, ethnicity_3, ethnicity_4,
    ethnicity_997, ethnicity_999, ethnicity_other,
    race_1, race_2, race_3, race_4, race_5, race_997, race_999
  ) %>%
  mutate(survey_cycle = 2023) %>%
  rename(hhno = hh_id) %>%
  mutate(pno = as.numeric(str_sub(person_id, -2, -1)))  

# --- day2023 ---
day2023_file <- "day.csv"
day2023_path <- file.path(background_dataset_2023_dir, day2023_file)
day2023_df <- read_csv(day2023_path)

day2023_df <- day2023_df %>%
  select(hh_id, person_id, travel_dow, telecommute_time) %>%
  mutate(survey_cycle = 2023) %>%
  rename(hhno = hh_id, day=travel_dow) %>%
  mutate(pno = as.numeric(str_sub(person_id, -2, -1))) %>%
  select(-person_id)  


# Read 2019 data
background_dataset_2019_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations/Final Updated Dataset as of 10-18-2021"

# --- hh2019 ---
hh2019_file <- "hh.tsv"
hh2019_path <- file.path(background_dataset_2019_dir, hh2019_file)
hh2019_df <- read_tsv(hh2019_path)

hh2019_df <- hh2019_df %>%
  select(hh_id, sample_stratum, income_detailed) %>%
  mutate(survey_cycle = 2019) %>%
  rename(hhno = hh_id,
         stratification_var = sample_stratum)

# --- person2019 ---
person2019_file <- "person.tsv"
person2019_path <- file.path(background_dataset_2019_dir, person2019_file)
person2019_df <- read_tsv(person2019_path)

person2019_df <- person2019_df %>%
  select(hh_id, person_id, age, employment, telework_freq, raceeth_new_imputed) %>%
  mutate(survey_cycle = 2019) %>%
  rename(hhno = hh_id) %>%
  mutate(pno = as.numeric(str_sub(person_id, -2, -1)))  

# --- day2019 ---
day2019_file <- "day.tsv"
day2019_path <- file.path(background_dataset_2019_dir, day2019_file)
day2019_df <- read_tsv(day2019_path)

day2019_df <- day2019_df %>%
  select(hh_id, person_id, travel_date_dow, telework_time) %>%
  mutate(survey_cycle = 2019) %>%
  rename(hhno = hh_id, day=travel_date_dow) %>%
  rename(telecommute_time=telework_time) %>%
  mutate(pno = as.numeric(str_sub(person_id, -2, -1))) %>%
  select(-person_id)   

# Union the two cycles
hh_2019_2023_df <- bind_rows(hh2019_df, hh2023_df)
person_2019_2023_df <- bind_rows(person2019_df, person2023_df)
day_2019_2023_df <- bind_rows(day2019_df, day2023_df)

# Join to LinkedTrips_2019_2023_df
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  left_join(hh_2019_2023_df, by = c("hhno", "survey_cycle"))

LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  left_join(person_2019_2023_df, by = c("hhno", "pno", "survey_cycle"))

LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  left_join(day_2019_2023_df, by = c("hhno", "pno", "day", "survey_cycle"))  

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
# drop TNC, School Bus and Other
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(mode5cat_label = case_when(
    mode == 1 ~ "4. Walk",
    mode == 2 ~ "5. Bike",
    mode == 3 ~ "1. Drive Alone",
    mode == 4 ~ "2. Carpool",
    mode == 5 ~ "2. Carpool",
    mode == 6 ~ "3. Transit",
    mode == 7 ~ "3. Transit",
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


# income_detailed
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(
    income_detailed_label = case_when(
      income_detailed == 1  ~ "1. Less than $15,000",
      income_detailed == 2  ~ "2. $15,000-$24,999",
      income_detailed == 3  ~ "3. $25,000-$34,999",
      income_detailed == 4  ~ "4. $35,000-$49,999",
      income_detailed == 5  ~ "5. $50,000-$74,999",
      income_detailed == 6  ~ "6. $75,000-$99,999",
      income_detailed == 7  ~ "7. $100,000-$149,999",
      income_detailed == 8  ~ "8. $150,000-$199,999",
      income_detailed == 9  ~ "9. $200,000-$249,999",
      income_detailed == 10 ~ "10.$250,000 or more",
      TRUE ~ NA_character_
    )
  )


# income detailed (and then grouped)
# grouping informed by the fact that median household income in 2023 is $128K in the Bay Area
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(
    income_detailed_grouped = case_when(
      income_detailed == 1  ~ "1. Less than $50,000",
      income_detailed == 2  ~ "1. Less than $50,000",
      income_detailed == 3  ~ "1. Less than $50,000",
      income_detailed == 4  ~ "1. Less than $50,000",
      income_detailed == 5  ~ "2. $50,000-$99,999",
      income_detailed == 6  ~ "2. $50,000-$99,999",
      income_detailed == 7  ~ "3. $100,000-$199,999",
      income_detailed == 8  ~ "3. $100,000-$199,999",
      income_detailed == 9  ~ "4. $200,000 or more",
      income_detailed == 10 ~ "4. $200,000 or more",
      TRUE ~ NA_character_
    )
  )

# race and ethnicity
# for 2023
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(
    race_recode_2023 = case_when(
      ethnicity_1==1 & race_5==1 & race_1==0 & race_2==0 & 
        race_3==0 & race_4==0 & race_997==0                                              ~ "4. White (Non-Hispanic)",
      ethnicity_1==1 & race_1==1 & race_5==0 & race_2==0 & 
        race_3==0 & race_4==0 & race_997==0                                              ~ "2. Black (Non-Hispanic)",
      ethnicity_1==1 & (race_3==1 | race_4==1) & race_5==0 & race_1==0 & 
        race_2==0 & race_997==0                                                          ~ "3. Asian/Pacific Islander (Non-Hispanic)",
      ethnicity_1==1                                                                     ~ "5. Other (Non-Hispanic)",
      ethnicity_2==1 | ethnicity_3==1 | ethnicity_4==1 | ethnicity_997==1                ~ "1. Hispanic (All Races)",
      ethnicity_999==1 | ethnicity_other==1                                              ~ "5. Other (Non-Hispanic)",
      ethnicity_1==995 & ethnicity_2==995 & ethnicity_3==995 & ethnicity_4==995 &
        ethnicity_997==995 & ethnicity_999==995 & race_1==995 & race_2==995 &
        race_3==995 & race_4==995 & race_5==995 & race_997==995 & race_999==995          ~ NA_character_,
      TRUE                                                                               ~ NA_character_
    )
  )

# race and ethnicity
# for 2019
# In the BATS 2019 dataset, there are several race and ethnicity variables (raceeth_imputed, raceeth_new, raceeth_new_imputed)
# raceeth_new_imputed appears to be the most finalized version, as it was the field used in the BATS 2019 freeway usage dashboard.
# https://github.com/BayAreaMetro/Travel-Diary-Surveys/blob/eabb4a2006121101245b3e43ade9388cbf38f438/TNC%202018-2019%20Survey/NextGen%20Freeway%20Work/TNC%20Survey%20Facility%20Margin%20of%20Error%20Calculations_All%20Facilities.r#L148

LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(
    race_eth_label_2019 = case_when(
      raceeth_new_imputed == 1 ~ "1. Hispanic (All Races)",
      raceeth_new_imputed == 2 ~ "2. Black (Non-Hispanic)",
      raceeth_new_imputed == 3 ~ "3. Asian/Pacific Islander (Non-Hispanic)",
      raceeth_new_imputed == 4 ~ "4. White (Non-Hispanic)",
      raceeth_new_imputed == 5 ~ "5. Other (Non-Hispanic)",
      TRUE                     ~ NA_character_
    )
  )

# Combine 2019 and 2023 race/ethnicity into one variable
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(
    race_eth = coalesce(race_recode_2023, race_eth_label_2019)
  )

# Write LinkedTrips_2019_2023_df to csv for subsequent processes
output_trips_csv <- glue("{working_dir}/LinkedTrips_2019_2023_withDist_withStrata.csv")
write.csv(LinkedTrips_2019_2023_df, file = output_trips_csv, row.names = FALSE)
print(glue("Wrote {nrow(LinkedTrips_2019_2023_df)} rows to {output_trips_csv}"))

