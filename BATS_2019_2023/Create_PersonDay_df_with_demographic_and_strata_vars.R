
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
  select(hh_id, sample_segment, home_county, income_detailed) %>%
  mutate(survey_cycle = 2023) %>%
  mutate(home_county = as.character(home_county)) %>%
  rename(hhno = hh_id, # TODO: switch back to using RSG's convention rather than the trip linking and tour buiding convention
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
hh2019_df <- read_tsv(hh2019_path)

hh2019_df <- hh2019_df %>%
  select(hh_id, sample_stratum, home_county_fips, income_detailed) %>%
  mutate(survey_cycle = 2019) %>%
  rename(hhno = hh_id,
         stratification_var = sample_stratum) %>%
  mutate(home_county_fips = as.character(home_county_fips)) # note that the 2023 dataset uses all five digits but the 2019 dataset uses only the last three digits 001, 003

# --- person2019 ---
person2019_file <- "person.tsv"
person2019_path <- file.path(background_dataset_2019_dir, person2019_file)
person2019_df <- read_tsv(person2019_path)

person2019_df <- person2019_df %>%
  select(hh_id, person_id, age, employment, telework_freq, raceeth_new_imputed) %>%
  mutate(survey_cycle = 2019) %>%
  rename(hhno = hh_id) %>%
  mutate(pno = as.numeric(str_sub(person_id, -2, -1)))  %>%
  select(-person_id) 

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

# Label the opurp variable
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(opurp_label = case_when(
    opurp == 0 ~ "HOME",
    opurp == 1 ~ "WORK",
    opurp == 2 ~ "SCHOOL",
    opurp == 3 ~ "ESCORT",
    opurp == 4 ~ "PERS_BUS",
    opurp == 5 ~ "SHOP",
    opurp == 6 ~ "MEAL",
    opurp == 7 ~ "SOCREC",
    opurp == 10 ~ "CHANGE_MODE",
    opurp == 11 ~ "OTHER",
    opurp == -1 ~ "MISSING",
    TRUE ~ NA_character_
  ))


# Collapse LinkedTrips_2019_2023_df so it becomes a person day file
# Group by hhno, pno, day and indicate if the person commuted on that day

# -------------------------
# Use a bounding box to clean the distance data
# -------------------------
#The northernmost point of Yuba County: 39.639458, -121.009478
#The southernmost point of Monterey County: 35.795190, -121.347789
#The easternmost point of El Dorado County: 38.870630, -119.877219
#The westernmost point of Sonoma County: 38.768395, -123.533743

# Create indicator variables in the data frame
LinkedTrips_2019_2023_df$OxInMegaRegion <- ifelse(
  LinkedTrips_2019_2023_df$oxco_copy >= -123.533743 & 
  LinkedTrips_2019_2023_df$oxco_copy <= -119.877219, 1, 0)

LinkedTrips_2019_2023_df$DxInMegaRegion <- ifelse(
  LinkedTrips_2019_2023_df$dxco_copy >= -123.533743 & 
  LinkedTrips_2019_2023_df$dxco_copy <= -119.877219, 1, 0)

LinkedTrips_2019_2023_df$OyInMegaRegion <- ifelse(
  LinkedTrips_2019_2023_df$oyco_copy >= 35.795190 & 
  LinkedTrips_2019_2023_df$oyco_copy <= 39.639458, 1, 0)

LinkedTrips_2019_2023_df$DyInMegaRegion <- ifelse(
  LinkedTrips_2019_2023_df$dyco_copy >= 35.795190 & 
  LinkedTrips_2019_2023_df$dyco_copy <= 39.639458, 1, 0)

LinkedTrips_2019_2023_df$OD_In_MegaRegion <- ifelse(
  LinkedTrips_2019_2023_df$OxInMegaRegion == 1 & 
  LinkedTrips_2019_2023_df$OyInMegaRegion == 1 & 
  LinkedTrips_2019_2023_df$DxInMegaRegion == 1 & 
  LinkedTrips_2019_2023_df$DyInMegaRegion == 1, 1, 0)

# -------------------------

# Group by hhno, pno, day and sum distances
PersonDayFromLinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  group_by(hhno, pno, day, survey_cycle) %>%
  summarise(
    PersonDay_In_MegaRegion       = ifelse(all(OD_In_MegaRegion == 1), 1, 0),
 
    personDay_in_LinkedTripFile   = 1, # for tracking if the PersonDay file includes PersonDay with no travel 
    commuted_on_travel_day        = as.integer(any(dpurp_label == "WORK", na.rm = TRUE)),

    personDay_dist_in_miles       = sum(crow_fly_miles_cap200, na.rm = TRUE),
    personDay_dist_PbShMeSo_miles = sum(crow_fly_miles_cap200[dpurp_label %in% c("PERS_BUS", "SHOP", "MEAL", "SOCREC")], na.rm = TRUE),
    num_trips                     = n(),
    num_PbShMeSo_trips = sum(dpurp_label %in% c("PERS_BUS", "SHOP", "MEAL", "SOCREC"), na.rm = TRUE),
    num_ShMeSo_trips = sum(dpurp_label %in% c("SHOP", "MEAL", "SOCREC"), na.rm = TRUE),

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
   
    # Home->Work trip mode
    HtoW_trip_mode                 = ifelse(any(opurp_label == "HOME" & dpurp_label == "WORK"), 
                                           first(mode[opurp_label == "HOME" & dpurp_label == "WORK"]), 
                                           NA_integer_),

    WtoH_trip_mode                 = ifelse(any(opurp_label == "WORK" & dpurp_label == "HOME"), 
                                           last(mode[opurp_label == "WORK" & dpurp_label == "HOME"]), 
                                           NA_integer_),

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
    personDay_dist_PbShMeSo_miles = replace_na(personDay_dist_PbShMeSo_miles, 0),
    num_trips = replace_na(num_trips, 0),
    num_PbShMeSo_trips = replace_na(num_PbShMeSo_trips, 0),
    num_ShMeSo_trips = replace_na(num_ShMeSo_trips, 0),
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

# -------------------------------------
# drop if PersonDay_In_MegaRegion=0
# --------------------------------------

# Store numbers before filtering
original_rows <- nrow(ProcessedPersonDays_2019_2023_df)
original_pdexpfac <- sum(ProcessedPersonDays_2019_2023_df$pdexpfac, na.rm = TRUE)

# Filter to drop only rows where PersonDay_In_MegaRegion = 0 (keeps 1 and NA)
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  filter(PersonDay_In_MegaRegion != 0 | is.na(PersonDay_In_MegaRegion))

# Calculate numbers after filtering
new_rows <- nrow(ProcessedPersonDays_2019_2023_df)
new_pdexpfac <- sum(ProcessedPersonDays_2019_2023_df$pdexpfac, na.rm = TRUE)

# Report results
cat("Rows dropped:", original_rows - new_rows, "\n")
cat("Original rows:", original_rows, "\n")
cat("Remaining rows:", new_rows, "\n\n")
cat("Original pdexpfac:", format(original_pdexpfac, big.mark = ","), "\n")
cat("Remaining pdexpfac:", format(new_pdexpfac, big.mark = ","), "\n")
cat("pdexpfac dropped:", format(original_pdexpfac - new_pdexpfac, big.mark = ","), "\n")


# ------------------
# Add labels
# ------------------
# employment status
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
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

# income_detailed
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
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
      income_detailed == 999 ~ "999. Prefer not to answer",
      TRUE ~ "Other"
    )
  )


# income detailed (and then grouped)
# grouping informed by the fact that median household income in 2023 is $128K in the Bay Area
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
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
      income_detailed == 999 ~ "999. Prefer not to answer",
      TRUE ~ "Other"
    )
  )

# The variable income_broad is not as helpful in the comparison between 2019 and 2023 as top categories were banded differently.
# If we use income_broad, the top category would have to be "$100,000 or more"
# This is why income_detailed is used here.
# The trade-off is that income_broad had more complete records than income_detailed 
# In 2023, the variable income_broad
#    survey_cycle == 2023 & income_broad == 5   ~ "$100,000-$199,999",
#    survey_cycle == 2023 & income_broad == 6   ~ "$200,000 or more",
# In 2019, the variable income_aggregate
#     survey_cycle == 2019 & income_aggregate == 5   ~ "$100,000-$249,999", #not the same band as BATS2023
#     survey_cycle == 2019 & income_aggregate == 6   ~ "$250,000 or more",  #not the same band as BATS2023

# race and ethnicity
# for 2023
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
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

ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
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
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  mutate(
    race_eth = coalesce(race_recode_2023, race_eth_label_2019)
  )

# Write ProcessedPersonDays_2019_2023_df  to csv for subsequent processes
output_trips_csv <- glue("{working_dir}/ProcessedPersonDays_2019_2023.csv")
write.csv(ProcessedPersonDays_2019_2023_df, file = output_trips_csv, row.names = FALSE)
print(glue("Wrote {nrow(ProcessedPersonDays_2019_2023_df)} rows to {output_trips_csv}"))

sink()