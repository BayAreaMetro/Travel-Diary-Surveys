library(dplyr)
library(readr)
library(glue)
library(tidyr)

# -----------
# Read input
# -----------
# 2023
weighted_2023dataset_dir <- "X:/survey_repos/ProjRoot_Mon-Thu20251011/WgtRoot_Mon-Thu20251011/output/full_weighted_dataset"

day2023_file <- "day.csv"
hh2023_file <- "hh.csv"

day2023_path <- file.path(weighted_2023dataset_dir, day2023_file)
hh2023_path <- file.path(weighted_2023dataset_dir, hh2023_file)

day2023_df <- read_csv(day2023_path) %>%
 mutate(survey_cycle=2023)
hh2023_df <- read_csv(hh2023_path) %>%
 mutate(survey_cycle=2023) %>%
 mutate(last_travel_date = as.Date(last_travel_date)) %>% # resolve data type issue
 rename(stratification_var = sample_segment) 


# The 2023 dataset contains blank weight values; replace them with 0
hh2023_df <- hh2023_df %>%
    mutate(hh_weight_rmove_only = replace_na(hh_weight_rmove_only, 0))

# 2019
weighted_2019dataset_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations/Final Updated Dataset as of 10-18-2021"

day2019_file <- "day.tsv"
hh2019_file <- "hh.tsv"

day2019_path <- file.path(weighted_2019dataset_dir, day2019_file)
hh2019_path <- file.path(weighted_2019dataset_dir, hh2019_file)

day2019_df <- read_tsv(day2019_path)  %>%
 mutate(survey_cycle=2019)
hh2019_df <- read_table(hh2019_path) %>%
 mutate(survey_cycle=2019) %>%
 mutate(last_travel_date = as.Date(last_travel_date)) %>% # resolve data type issue
 rename(stratification_var = sample_stratum)


# Union the two cycles
hh_2019_2023_df <- bind_rows(hh2019_df, hh2023_df)
day_2019_2023_df <- bind_rows(day2019_df, day2023_df)

# -----------
# Create unified weight variable
# -----------
hh_2019_2023_df <- hh_2019_2023_df %>%
  mutate(
    hh_weight = case_when(
      survey_cycle == 2023 ~ hh_weight_rmove_only,
      survey_cycle == 2019 ~ wt_sphone_wkday,
      TRUE ~ NA_real_
    )
  )

# Check for any missing weights
table(is.na(hh_2019_2023_df$hh_weight), hh_delivery_df$survey_cycle, useNA = "ifany")


# -----------
# Create unified income variable
# -----------
hh_2019_2023_df <- hh_2019_2023_df %>%
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

hh_2019_2023_df <- hh_2019_2023_df %>%
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

hh_2019_2023_df <- hh_2019_2023_df %>%
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
    survey_cycle == 2023 & income_broad == 3       ~ "3. $50,000-$74,2999",
    survey_cycle == 2023 & income_broad == 4       ~ "4. $75,000-$99,999",
    survey_cycle == 2023 & income_broad == 5       ~ "5. $100,000 or more",
    survey_cycle == 2023 & income_broad == 6       ~ "5. $100,000 or more",
    survey_cycle == 2023 & income_broad == 995     ~ "Missing",
    survey_cycle == 2023 & income_broad == 999     ~ "Missing",
    TRUE ~ NA_character_  # For other survey cycles or other values
  ))

hh_2019_2023_df <- hh_2019_2023_df %>%
  mutate(income_label_3cat = case_when(
    survey_cycle == 2019 & income_aggregate == 1   ~ "1. Under $50,000",
    survey_cycle == 2019 & income_aggregate == 2   ~ "1. Under $50,000",
    survey_cycle == 2019 & income_aggregate == 3   ~ "2. $50,000-$99,999",
    survey_cycle == 2019 & income_aggregate == 4   ~ "2. $50,000-$99,999",
    survey_cycle == 2019 & income_aggregate == 5   ~ "3. $100,000 or more ",
    survey_cycle == 2019 & income_aggregate == 6   ~ "3. $100,000 or more ",
    survey_cycle == 2019 & income_aggregate == 999 ~ "Missing",
    survey_cycle == 2023 & income_broad == 1       ~ "1. Under $50,000",
    survey_cycle == 2023 & income_broad == 2       ~ "1. Under $50,000",
    survey_cycle == 2023 & income_broad == 3       ~ "2. $50,000-$99,999",
    survey_cycle == 2023 & income_broad == 4       ~ "2. $50,000-$99,999",
    survey_cycle == 2023 & income_broad == 5       ~ "3. $100,000 or more",
    survey_cycle == 2023 & income_broad == 6       ~ "3. $100,000 or more",
    survey_cycle == 2023 & income_broad == 995     ~ "Missing",
    survey_cycle == 2023 & income_broad == 999     ~ "Missing",
    TRUE ~ NA_character_  # For other survey cycles or other values
  ))



# -----------
# if the deliveries were received on a travel day...
# -----------
# BATS 2023:
#delivery_2: Take-out/prepared food delivered to home 
#delivery_3: Someone came to do work at home (e.g., babysitter, housecleaning, lawn) [not included in delivery_cols below]
#delivery_4: Groceries delivered to home
#delivery_5: Received packages at home (e.g., USPS, FedEx, UPS)
#delivery_6: Received personal packages at work
#delivery_7: Received packages at another location (e.g., Amazon Locker, package pick-up point)
#delivery_8: Other items delivered to home (e.g., appliance)
#delivery_9: Other items delivered to work
#delivery_996: None of the above

# BATS 2019: 
# main difference seems to be that groceries and food are grouped
#delivery_home:   Received packages AT HOME
#delivery_work:   Received personal packages AT WORK
#delivery_locker: Received packages AT OFFSITE LOCKER (e.g., Amazon Locker)
#delivery_food:   Food was delivered to home (e.g., take-out, groceries)
#delivery_other:  Other item delivered to home (e.g., appliance)
#service_work:    Someone came to home to do work (e.g., landscaping, plumber, housecleaning) 
#delivery_none:   None of the above

 
#  filters rows where at least one delivery column equals 1
#delivery_rows_df <- day_2019_2023_df %>%
#  filter(
#    (survey_cycle == 2023 & if_any(all_of(c('delivery_2', 'delivery_4', 'delivery_5', 
#                                              'delivery_6', 'delivery_7', 'delivery_8', 'delivery_9')), 
#                                    ~ . == 1)) |
#    (survey_cycle == 2019 & if_any(all_of(c('delivery_home', 'delivery_work', 'delivery_locker', 
#                                              'delivery_food', 'delivery_other')), 
#                                    ~ . == 1))
#  )

# packages at home
delivery_rows_df <- day_2019_2023_df %>%
  filter(
    (survey_cycle == 2023 & if_any(all_of(c('delivery_5', 'delivery_9')), 
                                    ~ . == 1)) |
    (survey_cycle == 2019 & if_any(all_of(c('delivery_home', 'delivery_other')), 
                                    ~ . == 1))
  )

# food or groceries
delivery_rows_df <- day_2019_2023_df %>%
  filter(
    (survey_cycle == 2023 & if_any(all_of(c('delivery_2', 'delivery_4')), 
                                    ~ . == 1)) |
    (survey_cycle == 2019 & if_any(all_of(c('delivery_food')), 
                                    ~ . == 1))
  )


# households that had a delivery
delivery_households_df <- delivery_rows_df %>%
  select(hh_id) %>%
  distinct() %>%
  mutate(had_delivery = 1)

# Merge with household data
hh_delivery_df <- hh_2019_2023_df %>%
  left_join(delivery_households_df, by = "hh_id")

# Check merge results (equivalent to indicator=True)
table(is.na(hh_delivery_df$had_delivery))

# Replace NA with 0 for had_delivery
hh_delivery_df <- hh_delivery_df %>%
  mutate(had_delivery = replace_na(had_delivery, 0))

# -----------
# Analysis: % of households that had a delivery
# -----------

# Unweighted counts and percentages
delivery_summary_unweighted <- hh_delivery_df %>%
  group_by(survey_cycle) %>%
  summarise(
    total_hh = n(),
    hh_with_delivery = sum(had_delivery),
    hh_no_delivery = sum(had_delivery == 0),
    pct_with_delivery = round(100 * mean(had_delivery), 1)
  )

print("Unweighted Delivery Rates by Survey Cycle:")
print(delivery_summary_unweighted)

# Overall unweighted
overall_unweighted <- hh_delivery_df %>%
  summarise(
    total_hh = n(),
    hh_with_delivery = sum(had_delivery),
    pct_with_delivery = round(100 * mean(had_delivery), 1)
  )

print("Overall Unweighted Delivery Rate:")
print(overall_unweighted)


# Weighted percentages using unified hh_weight variable
delivery_summary_weighted <- hh_delivery_df %>%
  group_by(survey_cycle) %>%
  summarise(
    weighted_total_hh = sum(hh_weight),
    weighted_hh_with_delivery = sum(had_delivery * hh_weight),
    pct_with_delivery_weighted = round(100 * sum(had_delivery * hh_weight) / sum(hh_weight), 1)
  )

print("Weighted Delivery Rates by Survey Cycle:")
print(delivery_summary_weighted)

# Overall weighted
overall_weighted <- hh_delivery_df %>%
  summarise(
    weighted_total_hh = sum(hh_weight),
    weighted_hh_with_delivery = sum(had_delivery * hh_weight),
    pct_with_delivery_weighted = round(100 * sum(had_delivery * hh_weight) / sum(hh_weight), 1)
  )

print("Overall Weighted Delivery Rate:")
print(overall_weighted)


# Combined summary table
combined_summary <- bind_rows(
  delivery_summary_unweighted %>% 
    mutate(type = "Unweighted") %>%
    select(survey_cycle, type, total_hh, hh_with_delivery, pct_with_delivery),
  delivery_summary_weighted %>% 
    mutate(type = "Weighted") %>%
    rename(total_hh = weighted_total_hh, hh_with_delivery = weighted_hh_with_delivery) %>%
    select(survey_cycle, type, total_hh, hh_with_delivery, pct_with_delivery = pct_with_delivery_weighted)
)

print("Combined Summary:")
print(combined_summary)


# -----------
# Survey-weighted analysis with SE, CI, and CV using srvyr
# -----------
library(srvyr)

# Create survey design object
hh_svy <- hh_delivery_df %>%
  as_survey_design(
    ids = hh_id,  
    weights = hh_weight,
    strata =  stratification_var
  )

# Analysis by survey cycle
delivery_svy_by_cycle <- hh_svy %>%
  group_by(survey_cycle) %>%
  summarise(
    pct_with_delivery = survey_mean(had_delivery, vartype = c("se", "ci", "cv")) * 100
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

print("Survey-weighted Delivery Rates by Survey Cycle (with SE, CI, CV):")
print(delivery_svy_by_cycle)

# -----------
# Survey-weighted analysis by survey cycle and income
# -----------

# Analysis by survey cycle and income_label_3cat
delivery_svy_by_cycle_income <- hh_svy %>%
  group_by(survey_cycle, income_label_3cat) %>%
  summarise(
    pct_with_delivery = survey_mean(had_delivery, vartype = c("se", "ci", "cv")) * 100,
    n = unweighted(n())  # Include unweighted sample size for reference
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

print("Survey-weighted Delivery Rates by Survey Cycle and Income (with SE, CI, CV):")
print(delivery_svy_by_cycle_income)

# Create a cleaner table view
delivery_svy_by_cycle_income_clean <- delivery_svy_by_cycle_income %>%
  arrange(survey_cycle, income_label_3cat) %>%
  select(survey_cycle, income_label_3cat, n, pct_with_delivery, 
         pct_with_delivery_se, pct_with_delivery_low, pct_with_delivery_upp, 
         pct_with_delivery_cv)

print("Formatted Results:")
print(delivery_svy_by_cycle_income_clean)