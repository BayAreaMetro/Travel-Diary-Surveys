
# This script generate a person day file with demographic and stratification variables

# Load required libraries
library(readr)
library(dplyr)
library(glue)
library(stringr) # so I can use str_sub()
library(tidyr)   # so I can use replace_na

# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023"

# Start a log file
log_file <- glue("{working_dir}/GeneratePersonDayFile_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for creating a person day file with demographic and stratification variables: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

#-----------------------------------------
# Read the person day file
# This is the output from trip linking and tour building process
# It has fewer raw records than the unweighted dataset 
# For example, 76,977 raw records in the processed dataset in 2023 vs 91,581 in the unweighted and pre-processed dataset i.e. a "data loss" of about 16%
#-----------------------------------------

# Read 2023 processed PersonDay file
# Suppress progress bar for cleaner log output
ProcessedPersonDays2023_df <- read_csv("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/TripLinking_20251012/03b-assign_day/wt-wkday_4day/personday.csv",
                                progress = FALSE) %>% 
  mutate(survey_cycle = 2023)  %>%
  select(survey_cycle, hhno, pno, day, pdexpfac)

print(glue("ProcessedPersonDays2023_df:"))
print(glue("  Columns: {ncol(ProcessedPersonDays2023_df)}"))
print(glue("  Observations: {nrow(ProcessedPersonDays2023_df)}"))
print(glue("  Sum of pdexpfac: {sum(ProcessedPersonDays2023_df$pdexpfac, na.rm = TRUE)}"))
cat("\n")

# Read 2019 processed PersonDay file
# Note that the 2019 file is space delimited
ProcessedPersonDays2019_df <- read.table("E:/Box/Modeling and Surveys/Surveys/Travel Diary Survey/MPO Partner Household Travel Survey (Includes 2018_2019 TNC Survey)/Bay Area Travel Study 2018-2019/Data from 2018_2019 Survey/BATS2018_2019_SFCTA_processed/20200228/survey2018_pdayx.dat",
                                  header = TRUE,
                                  sep = "") %>% 
  mutate(survey_cycle = 2019)  %>%
  select(survey_cycle, hhno, pno, day, pdexpfac)

print(glue("ProcessedPersonDays2019_df:"))
print(glue("  Columns: {ncol(ProcessedPersonDays2019_df)}"))
print(glue("  Observations: {nrow(ProcessedPersonDays2019_df)}"))
print(glue("  Sum of pdexpfac: {sum(ProcessedPersonDays2019_df$pdexpfac, na.rm = TRUE)}"))
cat("\n")


# Union the two cycles
# -------------------------
ProcessedPersonDays_2019_2023_df <- bind_rows(ProcessedPersonDays2019_df, ProcessedPersonDays2023_df)


#-----------------------------------------
# Bring in background information e.g. employment, telework_time
# Note that variable naming is not always consistent across year 
# e.g., travel_dow in 2023 vs travel_date_dow in 2019, telecommute time in 2023 vs telework_time in 2019
#-----------------------------------------


# Read 2023 household data
# Referencing the unweighted database, since this is the most upstreamed dataset
# See weighting config in the repo: https://github.com/BayAreaMetro/mtc_hts_weighting_scripts/blob/c93f914167af534b7340bdad5f4dff5895d18598/configs/project_settings.yaml#L10
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
  mutate(pno = as.numeric(str_sub(person_id, -2, -1))) %>%
  select(-person_id) 

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
  mutate(pno = as.numeric(str_sub(person_id, -2, -1)))  %>%
  select(-person_id) 

# --- day2019 ---
day2019_file <- "day.tsv"
day2019_path <- file.path(background_dataset_2019_dir, day2019_file)
day2019_df <- read_table(day2019_path)

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

# Join to ProcessedPersonDays_2019_2023_df
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  left_join(hh_2019_2023_df, by = c("hhno", "survey_cycle"))

ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  left_join(person_2019_2023_df, by = c("hhno", "pno", "survey_cycle"))

ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  left_join(day_2019_2023_df, by = c("hhno", "pno", "day", "survey_cycle"))  


#-----------------------------------------
# Bring in trip information
# for info e.g., commuted_on_travel_day
#-----------------------------------------

# Read the linked trip data with distance appended (this is the output from BATS_2019_2023_geocode_OD_to_TAZ.R)
LinkedTrips_2019_2023_df <- read.csv(glue("{working_dir}/LinkedTrips_2019_2023_withDist_withStrata.csv"))

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

# Collapse LinkedTrips_2019_2023_df so it becomes a person day file
# Group by hhno, pno, day and indicate if the person commuted on that day

# delete
#PersonDay_2019_2023_commutedflag_df <- LinkedTrips_2019_2023_df %>%
#  group_by(hhno, pno, day, survey_cycle) %>%
#  summarise(
#    commuted_on_travel_day    = as.integer(any(dpurp_label == "WORK", na.rm = TRUE)),
#  )

# Group by hhno, pno, day and sum distances
PersonDayFromLinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  group_by(hhno, pno, day, survey_cycle) %>%
  summarise(
    personDay_in_LinkedTripFile   = 1, # for tracking if the PersonDay file includes PersonDay with no travel 
    commuted_on_travel_day        = as.integer(any(dpurp_label == "WORK", na.rm = TRUE)),

    personDay_dist_in_miles       = sum(crow_fly_miles_cap200, na.rm = TRUE),
    personDay_dist_non_work_miles = sum(crow_fly_miles_cap200[dpurp_label %in% c("PERS_BUS", "SHOP", "MEAL", "SOCREC")], na.rm = TRUE),
    num_trips                     = n(),
    num_disc_trips = sum(dpurp_label %in% c("SHOP", "SOCREC"), na.rm = TRUE),

    # Number of trips by purpose
    num_trips_HOME                = sum(dpurp_label == "HOME", na.rm = TRUE),
    num_trips_WORK                = sum(dpurp_label == "WORK", na.rm = TRUE),
    num_trips_SCHOOL              = sum(dpurp_label == "SCHOOL", na.rm = TRUE),
    num_trips_ESCORT              = sum(dpurp_label == "ESCORT", na.rm = TRUE),
    num_trips_PERS_BUS            = sum(dpurp_label == "PERS_BUS", na.rm = TRUE),
    num_trips_SHOP                = sum(dpurp_label == "SHOP", na.rm = TRUE),
    num_trips_MEAL                = sum(dpurp_label == "MEAL", na.rm = TRUE),
    num_trips_SOCREC              = sum(dpurp_label == "SOCREC", na.rm = TRUE),
    num_trips_OTHER               = sum(dpurp_label == "OTHER", na.rm = TRUE),

    .groups = "drop"
  )


# Join PersonDayFromLinkedTrips_2019_2023_df  to ProcessedPersonDays_2019_2023_df, keeping all records from ProcessedPersonDays_2019_2023_df
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  left_join(
    PersonDayFromLinkedTrips_2019_2023_df,
    by = c("hhno", "pno", "day", "survey_cycle")
  )

# fill NA with 0 as those are person-days with no travel
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  mutate(
    commuted_on_travel_day = replace_na(commuted_on_travel_day, 0),
    personDay_dist_in_miles = replace_na(personDay_dist_in_miles, 0),
    personDay_dist_non_work_miles = replace_na(personDay_dist_non_work_miles, 0),
    num_trips = replace_na(num_trips, 0),
    num_disc_trips = replace_na(num_disc_trips, 0),
    num_trips_HOME = replace_na(num_trips_HOME, 0),
    num_trips_WORK = replace_na(num_trips_WORK, 0),
    num_trips_SCHOOL = replace_na(num_trips_SCHOOL, 0),
    num_trips_ESCORT = replace_na(num_trips_ESCORT, 0),
    num_trips_PERS_BUS = replace_na(num_trips_PERS_BUS, 0),
    num_trips_SHOP = replace_na(num_trips_SHOP, 0),
    num_trips_MEAL = replace_na(num_trips_MEAL, 0),
    num_trips_SOCREC = replace_na(num_trips_SOCREC, 0),
    num_trips_OTHER = replace_na(num_trips_OTHER, 0)
  )

# Write ProcessedPersonDays_2019_2023_df  to csv for subsequent processes
output_trips_csv <- glue("{working_dir}/ProcessedPersonDays_2019_2023.csv")
write.csv(ProcessedPersonDays_2019_2023_df, file = output_trips_csv, row.names = FALSE)
print(glue("Wrote {nrow(ProcessedPersonDays_2019_2023_df)} rows to {output_trips_csv}"))

sink()