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
  select(hh_id, sample_segment, home_lon, home_lat, income_broad, income_detailed, home_county) %>%
  mutate(survey_cycle = 2023) %>%
  mutate(home_county = as.character(home_county)) %>%
  rename(stratification_var = sample_segment)

# --- person2023 ---
person2023_file <- "person.csv"
person2023_path <- file.path(background_dataset_2023_dir, person2023_file)
person2023_df <- read_csv(person2023_path)

person2023_df <- person2023_df %>%
  select(hh_id, person_id, person_weight_rmove_only, age, gender, employment, telework_freq, job_type, work_mode, education, industry, occupation, work_lat, work_lon) %>%
  rename(telework_freq2023=telework_freq) %>% # the 2023 coding has more categories than the 2019 coding
  mutate(survey_cycle = 2023)

  # Read 2019 data
background_dataset_2019_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations/Final Updated Dataset as of 10-18-2021"

# --- hh2019 ---
hh2019_file <- "hh.tsv"
hh2019_path <- file.path(background_dataset_2019_dir, hh2019_file)
hh2019_df <- read_tsv(hh2019_path)

hh2019_df <- hh2019_df %>%
  select(hh_id, sample_stratum, reported_home_lat, reported_home_lon, income_aggregate, income_detailed, home_county_fips) %>%
  mutate(survey_cycle = 2019) %>%
  mutate(home_county_fips = as.character(home_county_fips)) %>% # note that the 2023 dataset uses all five digits but the 2019 dataset uses only the last three digits 001, 003
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
  select(hh_id, person_id, wt_sphone_wkday, age, gender, employment, telework_freq, job_type, education, work_lat, work_lon) %>%
  mutate(survey_cycle = 2019) %>%
  rename(telework_freq2019=telework_freq) %>%
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
    gender_label = case_when(
      gender == 1 ~ "1. Female",
      gender == 2 ~ "2. Male",
      gender == 4 ~ "4. Non-binary",
      TRUE ~ NA_character_ 
    )
  )

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

# income_detailed
person_2019_2023_df <- person_2019_2023_df %>%
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
person_2019_2023_df <- person_2019_2023_df %>%
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

  # label telework_freq based on codebook
  person_2019_2023_df <- person_2019_2023_df %>%
  mutate(telework_freq_temp_label = case_when(
    survey_cycle == 2019 & telework_freq2019 == 1   ~ "1. 6-7 days a week",
    survey_cycle == 2019 & telework_freq2019 == 2   ~ "2. 5 days a week",
    survey_cycle == 2019 & telework_freq2019 == 3   ~ "3. 4 days a week",
    survey_cycle == 2019 & telework_freq2019 == 4   ~ "4. 2-3 days a week", # ths category is different across cycles
    survey_cycle == 2019 & telework_freq2019 == 5   ~ "5. 1 day a week",
    survey_cycle == 2019 & telework_freq2019 == 6   ~ "6. 1-3 days a month",
    survey_cycle == 2019 & telework_freq2019 == 7   ~ "7. Less than monthly",
    survey_cycle == 2019 & telework_freq2019 == 8   ~ "8. Never",
    survey_cycle == 2023 & telework_freq2023 == 1       ~ "1. 6-7 days a week",
    survey_cycle == 2023 & telework_freq2023 == 2       ~ "2. 5 days a week",
    survey_cycle == 2023 & telework_freq2023 == 3       ~ "3. 4 days a week",
    survey_cycle == 2023 & telework_freq2023 == 4       ~ "4. 3 days a week",
    survey_cycle == 2023 & telework_freq2023 == 5       ~ "5. 2 days a week ",
    survey_cycle == 2023 & telework_freq2023 == 6       ~ "6. 1 day a week ",
    survey_cycle == 2023 & telework_freq2023 == 7       ~ "7. 1-3 days a month",
    survey_cycle == 2023 & telework_freq2023 == 8       ~ "8. Less than monthly",
    survey_cycle == 2023 & telework_freq2023 == 995     ~ "Missing",
    survey_cycle == 2023 & telework_freq2023 == 996     ~ "Never",
    TRUE ~ NA_character_  # For other survey cycles or other values
  ))

# use telework_freq in conjunction with the job_type variable
 person_2019_2023_df <- person_2019_2023_df %>%
    mutate(telework_jobtype3_label = case_when(
    job_type == 3                                   ~ "1. 5+ days a week", # Work ONLY from home or remotely (telework, self-employed)
    survey_cycle == 2019 & telework_freq2019 == 1   ~ "1. 5+ days a week",
    survey_cycle == 2019 & telework_freq2019 == 2   ~ "1. 5+ days a week",
    survey_cycle == 2019 & telework_freq2019 == 3   ~ "2. 4 days a week",
    survey_cycle == 2019 & telework_freq2019 == 4   ~ "3. 2-3 days a week", # ths category is different across cycles
    survey_cycle == 2019 & telework_freq2019 == 5   ~ "4. 1 day a week",
    survey_cycle == 2019 & telework_freq2019 == 6   ~ "5. Less than weekly",
    survey_cycle == 2019 & telework_freq2019 == 7   ~ "5. Less than weekly",
    survey_cycle == 2019 & telework_freq2019 == 8   ~ "5. Less than weekly",
    survey_cycle == 2023 & telework_freq2023 == 1       ~ "1. 5+ days a week",
    survey_cycle == 2023 & telework_freq2023 == 2       ~ "1. 5+ days a week",
    survey_cycle == 2023 & telework_freq2023 == 3       ~ "2. 4 days a week",
    survey_cycle == 2023 & telework_freq2023 == 4       ~ "3. 2-3 days a week",
    survey_cycle == 2023 & telework_freq2023 == 5       ~ "3. 2-3 days a week",
    survey_cycle == 2023 & telework_freq2023 == 6       ~ "4. 1 day a week",
    survey_cycle == 2023 & telework_freq2023 == 7       ~ "5. Less than weekly",
    survey_cycle == 2023 & telework_freq2023 == 8       ~ "5. Less than weekly",
    survey_cycle == 2023 & telework_freq2023 == 996     ~ "5. Less than weekly",
    TRUE ~ NA_character_  # For other survey cycles or other values
  ))

 person_2019_2023_df <- person_2019_2023_df %>%
    mutate(telework_freq_3cat_label = case_when(
    job_type == 3                                   ~ "1. Fully remote",
    survey_cycle == 2019 & telework_freq2019 == 1   ~ "1. Fully remote",
    survey_cycle == 2019 & telework_freq2019 == 2   ~ "1. Fully remote",
    survey_cycle == 2019 & telework_freq2019 == 3   ~ "2. Hybrid",
    survey_cycle == 2019 & telework_freq2019 == 4   ~ "2. Hybrid",
    survey_cycle == 2019 & telework_freq2019 == 5   ~ "2. Hybrid",
    survey_cycle == 2019 & telework_freq2019 == 6   ~ "3. Fully on-site",
    survey_cycle == 2019 & telework_freq2019 == 7   ~ "3. Fully on-site",
    survey_cycle == 2019 & telework_freq2019 == 8   ~ "3. Fully on-site",
    survey_cycle == 2023 & telework_freq2023 == 1       ~ "1. Fully remote",
    survey_cycle == 2023 & telework_freq2023 == 2       ~ "1. Fully remote",
    survey_cycle == 2023 & telework_freq2023 == 3       ~ "2. Hybrid",
    survey_cycle == 2023 & telework_freq2023 == 4       ~ "2. Hybrid",
    survey_cycle == 2023 & telework_freq2023 == 5       ~ "2. Hybrid",
    survey_cycle == 2023 & telework_freq2023 == 6       ~ "2. Hybrid",
    survey_cycle == 2023 & telework_freq2023 == 7       ~ "3. Fully on-site",
    survey_cycle == 2023 & telework_freq2023 == 8       ~ "3. Fully on-site",
    survey_cycle == 2023 & telework_freq2023 == 996     ~ "3. Fully on-site",
    TRUE ~ NA_character_  # For other survey cycles or other values
  ))


 person_2019_2023_df <- person_2019_2023_df %>%
    mutate(telework_freq_3cat_label2 = case_when(
    job_type == 3                                   ~ "1. Work from home 2 or more days a week",
    survey_cycle == 2019 & telework_freq2019 == 1   ~ "1. Work from home 2 or more days a week",
    survey_cycle == 2019 & telework_freq2019 == 2   ~ "1. Work from home 2 or more days a week",
    survey_cycle == 2019 & telework_freq2019 == 3   ~ "1. Work from home 2 or more days a week",
    survey_cycle == 2019 & telework_freq2019 == 4   ~ "1. Work from home 2 or more days a week",
    survey_cycle == 2019 & telework_freq2019 == 5   ~ "2. 1 day a week",
    survey_cycle == 2019 & telework_freq2019 == 6   ~ "3. Work from home infrequently (less than 1 day a week) or fully on-site",
    survey_cycle == 2019 & telework_freq2019 == 7   ~ "3. Work from home infrequently (less than 1 day a week) or fully on-site",
    survey_cycle == 2019 & telework_freq2019 == 8   ~ "3. Work from home infrequently (less than 1 day a week) or fully on-site",
    survey_cycle == 2023 & telework_freq2023 == 1       ~ "1. Work from home 2 or more days a week",
    survey_cycle == 2023 & telework_freq2023 == 2       ~ "1. Work from home 2 or more days a week",
    survey_cycle == 2023 & telework_freq2023 == 3       ~ "1. Work from home 2 or more days a week",
    survey_cycle == 2023 & telework_freq2023 == 4       ~ "1. Work from home 2 or more days a week",
    survey_cycle == 2023 & telework_freq2023 == 5       ~ "1. Work from home 2 or more days a week",
    survey_cycle == 2023 & telework_freq2023 == 6       ~ "2. 1 day a week",
    survey_cycle == 2023 & telework_freq2023 == 7       ~ "3. Work from home infrequently (less than 1 day a week) or fully on-site",
    survey_cycle == 2023 & telework_freq2023 == 8       ~ "3. Work from home infrequently (less than 1 day a week) or fully on-site",
    survey_cycle == 2023 & telework_freq2023 == 996     ~ "3. Work from home infrequently (less than 1 day a week) or fully on-site",
    TRUE ~ NA_character_  # For other survey cycles or other values
  ))


# ----------------------------------------
# Education
#-----------------------------------------

person_2019_2023_df <- person_2019_2023_df %>%
  mutate(education_label = case_when(
    education == 1 ~ "1. Less than high school",
    education == 2 ~ "2. High school graduate/GED",
    education == 3 ~ "3. Some college",
    education == 4 ~ "4. Vocational/technical training",
    education == 5 ~ "5. Associate degree",
    education == 6 ~ "6. Bachelor's degree",
    education == 7 ~ "7. Graduate/post-graduate degree",
    #education == 995 ~ "Missing Response",
    #education == 999 ~ "Prefer not to answer",
    TRUE ~ NA_character_
  ))

person_2019_2023_df <- person_2019_2023_df %>%
  mutate(education_grouped_label = case_when(
    education == 1 ~ "1. High school or less",
    education == 2 ~ "1. High school or less",
    education == 3 ~ "2. Some college or associate degree",
    education == 4 ~ "2. Some college or associate degree",
    education == 5 ~ "2. Some college or associate degree",
    education == 6 ~ "3. Bachelor's degree or higher",
    education == 7 ~ "3. Bachelor's degree or higher",
    #education == 995 ~ "Missing Response",
    #education == 999 ~ "Prefer not to answer",
    TRUE ~ NA_character_
  ))



# ----------------------------------------
# Industry and occupation (only in 2023)
#-----------------------------------------

person_2019_2023_df <- person_2019_2023_df %>%
  mutate(industry_label = case_when(
    industry == 1 ~ "1. Agriculture, Forestry, Fishing, and Hunting",
    industry == 2 ~ "2. Mining, Quarrying, and Oil and Gas Extraction",
    industry == 3 ~ "3. Utilities",
    industry == 4 ~ "4. Construction",
    industry == 5 ~ "5. Manufacturing",
    industry == 6 ~ "6. Wholesale Trade",
    industry == 7 ~ "7. Retail Trade",
    industry == 8 ~ "8. Transportation and Warehousing",
    industry == 9 ~ "9. Information",
    industry == 10 ~ "10. Finance and Insurance",
    industry == 11 ~ "11. Real Estate and Rental and Leasing",
    industry == 12 ~ "12. Professional, Scientific, and Technical Services",
    industry == 13 ~ "13. Management of Companies and Enterprises",
    industry == 14 ~ "14. Administrative and Support and Waste Management and Remediation Services",
    industry == 15 ~ "15. Educational Services",
    industry == 16 ~ "16. Health Care and Social Assistance",
    industry == 17 ~ "17. Arts, Entertainment, and Recreation",
    industry == 18 ~ "18. Accommodation and Food Services",
    industry == 19 ~ "19. Other Services (except Public Administration)",
    industry == 20 ~ "20. Public Administration",
    #industry == 995 ~ "Missing Response",
    #industry == 997 ~ "Other, please specify",
    TRUE ~ NA_character_
  ))

person_2019_2023_df <- person_2019_2023_df %>%
  mutate(occupation_label = case_when(
    occupation == 1 ~ "1. Management",
    occupation == 2 ~ "2. Business and Financial Operations",
    occupation == 3 ~ "3. Computer and Mathematical",
    occupation == 4 ~ "4. Architecture and Engineering",
    occupation == 5 ~ "5. Life, Physical, and Social Science",
    occupation == 6 ~ "6. Community and Social Service",
    occupation == 7 ~ "7. Legal",
    occupation == 8 ~ "8. Educational Instruction and Library",
    occupation == 9 ~ "9. Arts, Design, Entertainment, Sports, and Media",
    occupation == 10 ~ "10. Healthcare Practitioners and Technical",
    occupation == 11 ~ "11. Healthcare Support",
    occupation == 12 ~ "12. Protective Service",
    occupation == 13 ~ "13. Food Preparation and Serving Related",
    occupation == 14 ~ "14. Building and Grounds Cleaning and Maintenance",
    occupation == 15 ~ "15. Personal Care and Service",
    occupation == 16 ~ "16. Sales and Related",
    occupation == 17 ~ "17. Office and Administrative Support",
    occupation == 18 ~ "18. Farming, Fishing, and Forestry",
    occupation == 19 ~ "19. Construction and Extraction",
    occupation == 20 ~ "20. Installation, Maintenance, and Repair",
    occupation == 21 ~ "21. Production",
    occupation == 22 ~ "22. Transportation and Material Moving",
    occupation == 23 ~ "23. Military Specific",
    #occupation == 995 ~ "Missing Response",
    #occupation == 997 ~ "Other, please specify",
    TRUE ~ NA_character_
  ))

#-----------------------------------------
# Handle home_county code inconsistencies
#-----------------------------------------

person_2019_2023_df <- person_2019_2023_df %>%
  mutate(home_county_label = case_when(
    # 2023
    survey_cycle == 2023 & home_county == "06001" ~ "Alameda County",
    survey_cycle == 2023 & home_county == "06013" ~ "Contra Costa County",
    survey_cycle == 2023 & home_county == "06041" ~ "Marin County",
    survey_cycle == 2023 & home_county == "06055" ~ "Napa County",
    survey_cycle == 2023 & home_county == "06075" ~ "San Francisco County",
    survey_cycle == 2023 & home_county == "06081" ~ "San Mateo County",
    survey_cycle == 2023 & home_county == "06085" ~ "Santa Clara County",
    survey_cycle == 2023 & home_county == "06095" ~ "Solano County",
    survey_cycle == 2023 & home_county == "06097" ~ "Sonoma County",
    # 2019
    survey_cycle == 2019 & home_county_fips == "1" ~ "Alameda County",
    survey_cycle == 2019 & home_county_fips == "13" ~ "Contra Costa County",
    survey_cycle == 2019 & home_county_fips == "41" ~ "Marin County",
    survey_cycle == 2019 & home_county_fips == "55" ~ "Napa County",
    survey_cycle == 2019 & home_county_fips == "75" ~ "San Francisco County",
    survey_cycle == 2019 & home_county_fips == "81" ~ "San Mateo County",
    survey_cycle == 2019 & home_county_fips == "85" ~ "Santa Clara County",
    survey_cycle == 2019 & home_county_fips == "95" ~ "Solano County",
    survey_cycle == 2019 & home_county_fips == "97" ~ "Sonoma County",
    TRUE ~ NA_character_  
  ))

person_2019_2023_df <- person_2019_2023_df %>%
  mutate(home_county_grouped_label = case_when(
    # 2023
    survey_cycle == 2023 & home_county == "06001" ~ "Alameda",
    survey_cycle == 2023 & home_county == "06013" ~ "Contra Costa",
    survey_cycle == 2023 & home_county == "06041" ~ "Marin, Napa, Sonoma, Solano",
    survey_cycle == 2023 & home_county == "06055" ~ "Marin, Napa, Sonoma, Solano",
    survey_cycle == 2023 & home_county == "06075" ~ "San Francisco",
    survey_cycle == 2023 & home_county == "06081" ~ "San Mateo",
    survey_cycle == 2023 & home_county == "06085" ~ "Santa Clara",
    survey_cycle == 2023 & home_county == "06095" ~ "Marin, Napa, Sonoma, Solano",
    survey_cycle == 2023 & home_county == "06097" ~ "Marin, Napa, Sonoma, Solano",
    # 2019
    survey_cycle == 2019 & home_county_fips == "1" ~ "Alameda",
    survey_cycle == 2019 & home_county_fips == "13" ~ "Contra Costa",
    survey_cycle == 2019 & home_county_fips == "41" ~ "Marin, Napa, Sonoma, Solano",
    survey_cycle == 2019 & home_county_fips == "55" ~ "Marin, Napa, Sonoma, Solano",
    survey_cycle == 2019 & home_county_fips == "75" ~ "San Francisco",
    survey_cycle == 2019 & home_county_fips == "81" ~ "San Mateo",
    survey_cycle == 2019 & home_county_fips == "85" ~ "Santa Clara",
    survey_cycle == 2019 & home_county_fips == "95" ~ "Marin, Napa, Sonoma, Solano",
    survey_cycle == 2019 & home_county_fips == "97" ~ "Marin, Napa, Sonoma, Solano",
    TRUE ~ NA_character_  
  ))


# Write person_2019_2023_df  to csv for subsequent processes
output_persons_csv <- glue("{working_dir}/person_2019_2023.csv")
write.csv(person_2019_2023_df, file = output_persons_csv, row.names = FALSE)
print(glue("Wrote {nrow(person_2019_2023_df)} rows to {output_persons_csv}"))