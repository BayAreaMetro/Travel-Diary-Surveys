# This script generate a person day file with demographic and stratification variables

# Load required libraries
library(readr)
library(dplyr)
library(glue)
library(tidyr)   # so I can use replace_na

# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis"

# Start a log file
log_file <- glue("{working_dir}/GeneratePersonDayFile_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for creating a person day file with demographic and stratification variables: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

#-----------------------------------------
# Read the person day file
# From the weighted dataset
#-----------------------------------------

# Read 2023 Person-Day file
# Suppress progress bar for cleaner log output
PersonDays2023_df <- read_csv("X:/survey_repos/ProjRoot_Mon-Thu20251201/WgtRoot_Mon-Thu20251201_nocommutemode/output/full_weighted_dataset/day.csv",
                                progress = FALSE) %>% 
  mutate(survey_cycle = 2023)  %>%
  select(survey_cycle, hh_id, person_id, day_id, day_num, travel_dow, telecommute_time, is_complete, day_weight_rmove_only)

# align variable names across years
PersonDays2023_df <- PersonDays2023_df %>%
  rename(day_weight = day_weight_rmove_only)

print(glue("PersonDays2023_df:"))
print(glue("  Columns: {ncol(PersonDays2023_df)}"))
print(glue("  Observations: {nrow(PersonDays2023_df)}"))
print(glue("  Sum of day_weight: {sum(PersonDays2023_df$day_weight, na.rm = TRUE)}"))
cat("\n")

# Read 2019 Person-Day file
# Note that the 2019 file is space delimited
PersonDays2019_df <- read_tsv("M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations/Final Updated Dataset as of 10-18-2021/day.tsv",
                                progress = FALSE) %>% 
  mutate(survey_cycle = 2019)  %>%
  select(survey_cycle, hh_id, person_id, day_num, travel_date_dow, telework_time, day_complete, daywt_sphone_wkday)

# Align variable names across years
PersonDays2019_df <- PersonDays2019_df %>%
  rename(day_weight = daywt_sphone_wkday) %>%
  rename(travel_dow = travel_date_dow) %>%
  rename(telecommute_time = telework_time) 

print(glue("PersonDays2019_df:"))
print(glue("  Columns: {ncol(PersonDays2019_df)}"))
print(glue("  Observations: {nrow(PersonDays2019_df)}"))
print(glue("  Sum of day_weight: {sum(PersonDays2019_df$day_weight, na.rm = TRUE)}"))
cat("\n")


# Union the two cycles
# -------------------------
PersonDays_2019_2023_df <- bind_rows(PersonDays2019_df, PersonDays2023_df)


#-----------------------------------------
# Bring in background information e.g. employment
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
  select(hh_id, sample_segment, home_lat, home_lon, home_county, income_detailed) %>%
  mutate(survey_cycle = 2023) %>%
  mutate(home_county = as.character(home_county)) %>%
  rename(stratification_var = sample_segment)

# --- person2023 ---
person2023_file <- "person.csv"
person2023_path <- file.path(background_dataset_2023_dir, person2023_file)
person2023_df <- read_csv(person2023_path)

person2023_df <- person2023_df %>%
  select(
    hh_id, person_id, age, employment, work_lat, work_lon, work_county, job_type, telework_freq,
    ethnicity_1, ethnicity_2, ethnicity_3, ethnicity_4,
    ethnicity_997, ethnicity_999, ethnicity_other,
    race_1, race_2, race_3, race_4, race_5, race_997, race_999
  ) %>%
  mutate(survey_cycle = 2023)


# Read 2019 data
background_dataset_2019_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations/Final Updated Dataset as of 10-18-2021"

# --- hh2019 ---
hh2019_file <- "hh.tsv"
hh2019_path <- file.path(background_dataset_2019_dir, hh2019_file)
hh2019_df <- read_tsv(hh2019_path)

hh2019_df <- hh2019_df %>%
  select(hh_id, sample_stratum, reported_home_lat, reported_home_lon, home_county_fips, income_detailed) %>%
  mutate(survey_cycle = 2019) %>%
  rename(stratification_var = sample_stratum,
         home_lat = reported_home_lat,
         home_lon = reported_home_lon) %>%
  mutate(home_county_fips = as.character(home_county_fips)) # note that the 2023 dataset uses all five digits but the 2019 dataset uses only the last three digits 001, 003

# --- person2019 ---
person2019_file <- "person.tsv"
person2019_path <- file.path(background_dataset_2019_dir, person2019_file)
person2019_df <- read_tsv(person2019_path)

person2019_df <- person2019_df %>%
  select(hh_id, person_id, age, employment, work_lat, work_lon, job_type, telework_freq, raceeth_new_imputed) %>%
  mutate(survey_cycle = 2019) 



#-----------------------------------------
# Handle home_county code inconsistencies
#-----------------------------------------

hh2023_df <- hh2023_df %>%
  mutate(home_county_label = case_when(
    home_county == "06001" ~ "Alameda County",
    home_county == "06013" ~ "Contra Costa County",
    home_county == "06041" ~ "Marin County",
    home_county == "06055" ~ "Napa County",
    home_county == "06075" ~ "San Francisco County",
    home_county == "06081" ~ "San Mateo County",
    home_county == "06085" ~ "Santa Clara County",
    home_county == "06095" ~ "Solano County",
    home_county == "06097" ~ "Sonoma County",
    TRUE ~ NA_character_  
  ))


hh2019_df <- hh2019_df %>%
  mutate(home_county_label = case_when(
    home_county_fips == "1" ~ "Alameda County",
    home_county_fips == "13" ~ "Contra Costa County",
    home_county_fips == "41" ~ "Marin County",
    home_county_fips == "55" ~ "Napa County",
    home_county_fips == "75" ~ "San Francisco County",
    home_county_fips == "81" ~ "San Mateo County",
    home_county_fips == "85" ~ "Santa Clara County",
    home_county_fips == "95" ~ "Solano County",
    home_county_fips == "97" ~ "Sonoma County",
    TRUE ~ NA_character_  
  ))



#-----------------------------------------
# Union the two cycles
hh_2019_2023_df <- bind_rows(hh2019_df, hh2023_df)
person_2019_2023_df <- bind_rows(person2019_df, person2023_df)

# Join to PersonDays_2019_2023_df
PersonDays_2019_2023_df <- PersonDays_2019_2023_df %>%
  left_join(hh_2019_2023_df, by = c("hh_id", "survey_cycle"))

PersonDays_2019_2023_df <- PersonDays_2019_2023_df %>%
  left_join(person_2019_2023_df, by = c("hh_id", "person_id", "survey_cycle"))


#-----------------------------------------
# Bring in trip information
# for info e.g., commuted_on_travel_day
#-----------------------------------------

# --- trip2023 ---
trip2023_file <- "trip.csv"
trip2023_path <- file.path(background_dataset_2023_dir, trip2023_file)
trip2023_df <- read_csv(trip2023_path)

trip2023_df <- trip2023_df %>%
  select(hh_id, person_id, day_num, trip_id, d_purpose_category) %>%
  mutate(survey_cycle = 2023)

# Align variable names across years
trip2023_df <- trip2023_df %>%
  rename(dpurp = d_purpose_category) 

# --- trip2019 ---
trip2019_file <- "trip.tsv"
trip2019_path <- file.path(background_dataset_2019_dir, trip2019_file)
trip2019_df <- read_tsv(trip2019_path)

trip2019_df <- trip2019_df %>%
  select(hh_id, person_id, day_num, trip_id, d_purpose_category_imputed) %>%
  mutate(survey_cycle = 2019)

# Align variable names across years
trip2019_df <- trip2019_df %>%
  rename(dpurp = d_purpose_category_imputed) 

#-----------------------------------------
# Union the two cycles
trip_2019_2023_df <- bind_rows(trip2019_df, trip2023_df)

# Label the dpurp variable according to the codebooks
trip_2019_2023_df <- trip_2019_2023_df %>%
  mutate(dpurp_temp_label = case_when(
    survey_cycle == 2023 & dpurp == -1   ~ "Not imputable",
    survey_cycle == 2023 & dpurp == 1    ~ "Home",
    survey_cycle == 2023 & dpurp == 2    ~ "Work",
    survey_cycle == 2023 & dpurp == 3    ~ "Work related",
    survey_cycle == 2023 & dpurp == 4    ~ "School",
    survey_cycle == 2023 & dpurp == 5    ~ "School related",
    survey_cycle == 2023 & dpurp == 6    ~ "Escort",
    survey_cycle == 2023 & dpurp == 7    ~ "Shop",
    survey_cycle == 2023 & dpurp == 8    ~ "Meal",
    survey_cycle == 2023 & dpurp == 9    ~ "Social/recreation",
    survey_cycle == 2023 & dpurp == 10   ~ "Errand",
    survey_cycle == 2023 & dpurp == 11   ~ "Change mode",
    survey_cycle == 2023 & dpurp == 12   ~ "Overnight",
    survey_cycle == 2023 & dpurp == 13   ~ "Other",
    survey_cycle == 2019 & dpurp == -1   ~ "Missing: non-imputable",
    survey_cycle == 2019 & dpurp == 1    ~ "Home",
    survey_cycle == 2019 & dpurp == 2    ~ "Work",
    survey_cycle == 2019 & dpurp == 3    ~ "Work-related",
    survey_cycle == 2019 & dpurp == 4    ~ "School",
    survey_cycle == 2019 & dpurp == 5    ~ "Escort",
    survey_cycle == 2019 & dpurp == 6    ~ "Shop",
    survey_cycle == 2019 & dpurp == 7    ~ "Meal",
    survey_cycle == 2019 & dpurp == 8    ~ "Social/recreation",
    survey_cycle == 2019 & dpurp == 9    ~ "Errand/appointment",
    survey_cycle == 2019 & dpurp == 10   ~ "Change mode",
    survey_cycle == 2019 & dpurp == 11   ~ "Spent the night at non-home location",
    survey_cycle == 2019 & dpurp == 12   ~ "Other/Missing",
    survey_cycle == 2019 & dpurp == 14   ~ "School-related",
    TRUE ~ NA_character_
  ))

# Collapse the trip file so it becomes a person-day file
# Group by hh_id, person_id, and day_num
PersonDayFromTrips_2019_2023_df <- trip_2019_2023_df %>%
  group_by(survey_cycle, hh_id, person_id, day_num) %>%
  summarise(
    
    commuted_on_travel_day = as.integer(any(dpurp_temp_label == "Work", na.rm = TRUE)),

    # Number of trips by purpose (but do this for select purposes for now)
    num_trips_WORK    = sum(dpurp_temp_label == "Work", na.rm = TRUE),
    num_trips_SHOP    = sum(dpurp_temp_label == "Shop", na.rm = TRUE),
    num_trips_MEAL    = sum(dpurp_temp_label == "Meal", na.rm = TRUE),
    num_trips_SOCREC  = sum(dpurp_temp_label == "Social/recreation", na.rm = TRUE),

    .groups = "drop"
  )

# Join PersonDayFromTrips_2019_2023_df  to PersonDays_2019_2023_df, keeping all records from PersonDays_2019_2023_df
PersonDays_2019_2023_df <- PersonDays_2019_2023_df %>%
  left_join(
    PersonDayFromTrips_2019_2023_df,
    by = c("survey_cycle", "hh_id", "person_id", "day_num")
  )

# fill NA with 0 as those are person-days with no travel
PersonDays_2019_2023_df <- PersonDays_2019_2023_df %>%
  mutate(
    commuted_on_travel_day = replace_na(commuted_on_travel_day, 0),
    num_trips_WORK = replace_na(num_trips_WORK, 0),
    num_trips_SHOP = replace_na(num_trips_SHOP, 0),
    num_trips_MEAL = replace_na(num_trips_MEAL, 0),
    num_trips_SOCREC = replace_na(num_trips_SOCREC, 0)
  )

# ---------------------------
# Add filters that apply to all analysis
# ---------------------------

# Initial count
initial_summary <- PersonDays_2019_2023_df %>%
  group_by(survey_cycle) %>%
  summarise(
    count = n(),
    sum_weight = sum(day_weight, na.rm = TRUE)
  )
print(initial_summary)

# Store initial counts for reference
n_initial <- nrow(PersonDays_2019_2023_df)
n_initial_2019 <- nrow(PersonDays_2019_2023_df %>% filter(survey_cycle == 2019))
n_initial_2023 <- nrow(PersonDays_2019_2023_df %>% filter(survey_cycle == 2023))

# Filter to adults (18+)
PersonDays_2019_2023_df <- PersonDays_2019_2023_df %>%
  filter(age >= 4)

cat("\nAfter age filter (18+):\n")
PersonDays_2019_2023_df %>%
  group_by(survey_cycle) %>%
  summarise(
    count = n(),
    sum_weight = sum(day_weight, na.rm = TRUE)
  ) %>%
  print()


# Filter to workers
PersonDays_2019_2023_df <- PersonDays_2019_2023_df %>%
  filter(employment %in% c(1, 2, 3) )

cat("\nAfter worker filter (includes only full-time, part-time and self-employment):\n")
PersonDays_2019_2023_df %>%
  group_by(survey_cycle) %>%
  summarise(
    count = n(),
    sum_weight = sum(day_weight, na.rm = TRUE)
  ) %>%
  print()

# Filter to day weight > 0
PersonDays_2019_2023_df <- PersonDays_2019_2023_df %>%
  filter(day_weight > 0)

cat("\nAfter weight filter (day_weight > 0):\n")
PersonDays_2019_2023_df %>%
  group_by(survey_cycle) %>%
  summarise(
    count = n(),
    sum_weight = sum(day_weight, na.rm = TRUE)
  ) %>%
  print()

# --------------------------------------
# side investigation: Many records with no work location? Is it just because of questionnaire logic?
# --------------------------------------
# Create the has_work_location variable
PersonDays_2019_2023_df <- PersonDays_2019_2023_df %>%
  mutate(has_work_location = !is.na(work_lat) & !is.na(work_lon),
         has_work_location = factor(has_work_location,
                                   levels = c(TRUE, FALSE),
                                   labels = c("has work location", "no work location")))

# Label job_type
PersonDays_2019_2023_df <- PersonDays_2019_2023_df %>%
  mutate(job_type_label = case_when(
    survey_cycle == 2023 & job_type == 1 ~ "1. Go to one work location ONLY",
    survey_cycle == 2023 & job_type == 2 ~ "2. Work location regularly varies",
    survey_cycle == 2023 & job_type == 3 ~ "3. Work at home ONLY",
    survey_cycle == 2023 & job_type == 4 ~ "4. Drive/travel for work",
    survey_cycle == 2023 & job_type == 5 ~ "5. Work remotely some days and travel to a work location some days",
    survey_cycle == 2023 & job_type == 995 ~ "Missing Response",
    survey_cycle == 2019 & job_type == 1 ~ "1. Go to one work location ONLY",
    survey_cycle == 2019 & job_type == 2 ~ "2. Work location regularly varies",
    survey_cycle == 2019 & job_type == 3 ~ "3. Work at home ONLY",
    survey_cycle == 2019 & job_type == 4 ~ "4. Drive/travel for work",
    TRUE ~ as.character(job_type)
  ))

# chcek crosstab
xtabs(~ job_type_label + has_work_location, 
      data = PersonDays_2019_2023_df %>% filter(survey_cycle == 2019))

xtabs(~ job_type_label + has_work_location, 
      data = PersonDays_2019_2023_df %>% filter(survey_cycle == 2023))

# --------------------------------------

write_csv(PersonDays_2019_2023_df, file.path(working_dir, "PersonDays_2019_2023_workers.csv"))

sink()
