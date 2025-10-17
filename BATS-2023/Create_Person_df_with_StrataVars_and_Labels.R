# --------------------------------------
# Created a person level data frame
# --------------------------------------

library(readr)
library(dplyr)
library(glue)



# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023"

# Start a log file
log_file <- glue("{working_dir}/GeneratePersonFile_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for creating a person file with demographic and stratification variables: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line


weighted_dataset_dir <- "X:/survey_repos/ProjRoot_Mon-Thu20251011/WgtRoot_Mon-Thu20251011/output/full_weighted_dataset"


# read the household file because it has the stratification variables
hh_file <- "hh.csv"
hh_path <- file.path(weighted_dataset_dir, hh_file)
hh_df <- read_csv(hh_path)

hh_segment_df <- hh_df %>%
  select(hh_id, sample_segment, income_broad)


# read the person file
person_file <- "person.csv"
person_path <- file.path(weighted_dataset_dir, person_file)
person_df <- read_csv(person_path)

# Join the hh_df to person_df file
person_df <- person_df %>%
  left_join(hh_segment_df, by = "hh_id")


# ----
# labeling
# ----

# label pre_covid_mode
person_df <- person_df %>%
  mutate(pre_covid_mode_label = case_when(
    pre_covid_mode == 1 ~ "Walk (or jog/wheelchair)",
    pre_covid_mode == 100 ~ "Household vehicle (or motorcycle)",
    pre_covid_mode == 101 ~ "Other vehicle (e.g., friend's car, rental, carshare, work car)",
    pre_covid_mode == 102 ~ "Bus, shuttle, or vanpool",
    pre_covid_mode == 103 ~ "Bicycle",
    pre_covid_mode == 104 ~ "Other",
    pre_covid_mode == 105 ~ "Rail (e.g., train, light rail, trolley, BART, MUNI Metro)",
    pre_covid_mode == 106 ~ "Uber/Lyft, taxi, car service",
    pre_covid_mode == 107 ~ "Micromobility (e.g., scooter, moped, skateboard)",
    pre_covid_mode == 27 ~ "Medical transportation service",
    pre_covid_mode == 995 ~ "Missing Response",
    TRUE ~ NA_character_  # For any other values not listed
  ))

# group pre_covid_mode_grouped
person_df <- person_df %>%
  mutate(pre_covid_mode_grouped = case_when(
    pre_covid_mode == 1   ~ "Active Transportation",
    pre_covid_mode == 100 ~ "Drive",
    pre_covid_mode == 101 ~ "Drive",
    pre_covid_mode == 102 ~ "Transit",
    pre_covid_mode == 103 ~ "Active Transportation",
    pre_covid_mode == 104 ~ "Other",
    pre_covid_mode == 105 ~ "Transit",
    pre_covid_mode == 106 ~ "Drive",
    pre_covid_mode == 107 ~ "Active Transportation",
    pre_covid_mode == 27  ~ "Other",
    pre_covid_mode == 995 ~ "Missing Response",
    TRUE ~ NA_character_
  ))


# label work_mode
person_df <- person_df %>%
  mutate(work_mode_label = case_when(
    work_mode == 1 ~ "Walk (or jog/wheelchair)",
    work_mode == 100 ~ "Household vehicle (or motorcycle)",
    work_mode == 101 ~ "Other vehicle (e.g., friend's car, rental, carshare, work car)",
    work_mode == 102 ~ "Bus, shuttle, or vanpool",
    work_mode == 103 ~ "Bicycle",
    work_mode == 104 ~ "Other",
    work_mode == 105 ~ "Rail (e.g., train, light rail, trolley, BART, MUNI Metro)",
    work_mode == 106 ~ "Uber/Lyft, taxi, car service",
    work_mode == 107 ~ "Micromobility (e.g., scooter, moped, skateboard)",
    work_mode == 27 ~ "Medical transportation service",
    work_mode == 995 ~ "Missing Response",
    TRUE ~ NA_character_  # For any other values not listed
  ))

# group work_mode_grouped
person_df <- person_df %>%
  mutate(work_mode_grouped = case_when(
    work_mode == 1   ~ "Active Transportation",
    work_mode == 100 ~ "Drive",
    work_mode == 101 ~ "Drive",
    work_mode == 102 ~ "Transit",
    work_mode == 103 ~ "Active Transportation",
    work_mode == 104 ~ "Other",
    work_mode == 105 ~ "Transit",
    work_mode == 106 ~ "Drive",
    work_mode == 107 ~ "Active Transportation",
    work_mode == 27  ~ "Other",
    work_mode == 995 ~ "Missing Response",
    TRUE ~ NA_character_
  ))

# label for income_broad_label
person_df <- person_df %>%
  mutate(income_broad_label = case_when(
    income_broad == 1 ~ "1. Under $25,000",
    income_broad == 2 ~ "2. $25,000-$49,999",
    income_broad == 3 ~ "3. $50,000-$74,999",
    income_broad == 4 ~ "4. $75,000-$99,999",
    income_broad == 5 ~ "5. $100,000-$199,999",
    income_broad == 6 ~ "6. $200,000 or more",
    income_broad == 995 ~ "8. Missing Response",
    income_broad == 999 ~ "7. Prefer not to answer",
    TRUE ~ NA_character_  
  ))
