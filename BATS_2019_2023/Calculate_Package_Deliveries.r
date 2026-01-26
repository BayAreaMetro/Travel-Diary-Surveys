library(dplyr)
library(readr)
library(glue)
library(tidyr)

# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis"

# Set confidence level
CONF_LEVEL <- 0.90

# -----------
# Read input
# -----------
# 2023
weighted_2023dataset_dir <- "X:/survey_repos/ProjRoot_Mon-Thu20251201/WgtRoot_Mon-Thu20251201_nocommutemode/output/full_weighted_dataset"

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



# Union the two cycles
hh_2019_2023_df <- bind_rows(hh2019_df, hh2023_df)
day_2019_2023_df <- bind_rows(day2019_df, day2023_df)


#-----------------------------------------
# Group the counties two ways
#-----------------------------------------
hh_2019_2023_df <- hh_2019_2023_df %>%
  mutate(home_county_label_grouped = case_when(
    home_county_label == "Alameda County"       ~ "Alameda",
    home_county_label == "Contra Costa County"  ~ "Contra Costa",
    home_county_label == "Marin County"         ~ "Marin, Sonoma, Napa, Solano",
    home_county_label == "Napa County"          ~ "Marin, Sonoma, Napa, Solano",
    home_county_label == "San Francisco County" ~ "San Francisco",
    home_county_label == "San Mateo County"     ~ "San Mateo",
    home_county_label == "Santa Clara County"   ~ "Santa Clara",
    home_county_label == "Solano County"        ~ "Marin, Sonoma, Napa, Solano",
    home_county_label == "Sonoma County"        ~ "Marin, Sonoma, Napa, Solano",
    TRUE ~ NA_character_  
  ))

hh_2019_2023_df <- hh_2019_2023_df %>%
  mutate(home_county_label_grouped2 = case_when(
    home_county_label == "Alameda County"       ~ "Alameda",
    home_county_label == "Contra Costa County"  ~ "Contra Costa",
    home_county_label == "Marin County"         ~ "Marin & Sonoma",
    home_county_label == "Napa County"          ~ "Napa & Solano",
    home_county_label == "San Francisco County" ~ "San Francisco",
    home_county_label == "San Mateo County"     ~ "San Mateo",
    home_county_label == "Santa Clara County"   ~ "Santa Clara",
    home_county_label == "Solano County"        ~ "Napa & Solano",
    home_county_label == "Sonoma County"        ~ "Marin & Sonoma",
    TRUE ~ NA_character_  
  ))


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
table(is.na(hh_2019_2023_df$hh_weight), hh_2019_2023_df$survey_cycle, useNA = "ifany")


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
    survey_cycle == 2019 & income_aggregate == 5   ~ "5. $100,000 or more",
    survey_cycle == 2019 & income_aggregate == 6   ~ "5. $100,000 or more",
    survey_cycle == 2019 & income_aggregate == 999 ~ "Missing",
    survey_cycle == 2023 & income_broad == 1       ~ "1. Under $25,000",
    survey_cycle == 2023 & income_broad == 2       ~ "2. $25,000-$49,999",
    survey_cycle == 2023 & income_broad == 3       ~ "3. $50,000-$74,999",
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
    survey_cycle == 2019 & income_aggregate == 5   ~ "3. $100,000 or more",
    survey_cycle == 2019 & income_aggregate == 6   ~ "3. $100,000 or more",
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

# income_detailed is consistent across both cycles
hh_2019_2023_df <- hh_2019_2023_df %>%
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
hh_2019_2023_df <- hh_2019_2023_df %>%
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

 


# packages at home
#delivery_rows_df <- day_2019_2023_df %>%
#  filter(
#    (survey_cycle == 2023 & if_any(all_of(c('delivery_5', 'delivery_8')), 
#                                    ~ . == 1)) |
#    (survey_cycle == 2019 & if_any(all_of(c('delivery_home', 'delivery_other')), 
#                                    ~ . == 1))
# )

# food or groceries
#delivery_rows_df <- day_2019_2023_df %>%
#  filter(
#    (survey_cycle == 2023 & if_any(all_of(c('delivery_2', 'delivery_4')), 
#                                    ~ . == 1)) |
#    (survey_cycle == 2019 & if_any(all_of(c('delivery_food')), 
#                                    ~ . == 1))
#  )

# Create flags for both delivery types
# Packages at home
delivery_packages_df <- day_2019_2023_df %>%
  filter(
    (survey_cycle == 2023 & if_any(all_of(c('delivery_5', 'delivery_8')), ~ . == 1)) |
    (survey_cycle == 2019 & if_any(all_of(c('delivery_home', 'delivery_other')), ~ . == 1))
  ) %>%
  select(hh_id) %>%
  distinct() %>%
  mutate(had_package_delivery = 1)

# Food or groceries
delivery_food_df <- day_2019_2023_df %>%
  filter(
    (survey_cycle == 2023 & if_any(all_of(c('delivery_2', 'delivery_4')), ~ . == 1)) |
    (survey_cycle == 2019 & if_any(all_of(c('delivery_food')), ~ . == 1))
  ) %>%
  select(hh_id) %>%
  distinct() %>%
  mutate(had_food_delivery = 1)

# Merge both with household data
hh_delivery_df <- hh_2019_2023_df %>%
  left_join(delivery_packages_df, by = "hh_id") %>%
  left_join(delivery_food_df, by = "hh_id") %>%
  mutate(
    had_package_delivery = replace_na(had_package_delivery, 0),
    had_food_delivery = replace_na(had_food_delivery, 0)
  )


# -----------
# Survey-weighted analysis with SE, CI, and CV using srvyr
# -----------
library(srvyr)

# Create survey design object
hh_svy <- hh_delivery_df %>%
  as_survey_design(
    ids = hh_id,  
    weights = hh_weight,
    strata = stratification_var
  )

# Function to analyze delivery type
analyze_delivery_type <- function(hh_svy, delivery_var, delivery_label) {
  # By cycle
  by_cycle <- hh_svy %>%
    group_by(survey_cycle) %>%
    summarise(
      pct_with_delivery = survey_mean(!!sym(delivery_var), vartype = c("se", "ci", "cv"), level = CONF_LEVEL),
      n_weighted = survey_total(vartype = NULL),
      n = unweighted(n())
    ) %>%
    mutate(income_detailed_grouped = "All Income Levels",
           home_county_label_grouped = "Bay Area",
           home_county_label_grouped2 = "Bay Area",
           summary_level = "survey_cycle")
  
  # By cycle and income
  by_income <- hh_svy %>%
    group_by(survey_cycle, income_detailed_grouped) %>%
    summarise(
      pct_with_delivery = survey_mean(!!sym(delivery_var), vartype = c("se", "ci", "cv"), level = CONF_LEVEL),
      n_weighted = survey_total(vartype = NULL),
      n = unweighted(n())
    ) %>%
    mutate(home_county_label_grouped = "Bay Area",
           home_county_label_grouped2 = "Bay Area",
           summary_level = "survey_cycle,income_detailed_grouped")

  # By cycle and county grouping 1
  by_county1 <- hh_svy %>%
    group_by(survey_cycle, home_county_label_grouped) %>%
    summarise(
      pct_with_delivery = survey_mean(!!sym(delivery_var), vartype = c("se", "ci", "cv"), level = CONF_LEVEL),
      n_weighted = survey_total(vartype = NULL),
      n = unweighted(n())
    ) %>%
    mutate(income_detailed_grouped = "All Income Levels",
           summary_level = "survey_cycle,home_county_label_grouped")
  
  # By cycle and county grouping 2
  by_county2 <- hh_svy %>%
    group_by(survey_cycle, home_county_label_grouped2) %>%
    summarise(
      pct_with_delivery = survey_mean(!!sym(delivery_var), vartype = c("se", "ci", "cv"), level = CONF_LEVEL),
      n_weighted = survey_total(vartype = NULL),
      n = unweighted(n())
    ) %>%
    mutate(income_detailed_grouped = "All Income Levels",
           summary_level = "survey_cycle,home_county_label_grouped2")
  
  # By cycle, county grouping 1, and income
  by_county1_income <- hh_svy %>%
    group_by(survey_cycle, home_county_label_grouped, income_detailed_grouped) %>%
    summarise(
      pct_with_delivery = survey_mean(!!sym(delivery_var), vartype = c("se", "ci", "cv"), level = CONF_LEVEL),
      n_weighted = survey_total(vartype = NULL),
      n = unweighted(n())
    ) %>%
    mutate(summary_level = "survey_cycle,home_county_label_grouped,income_detailed_grouped")
  
  # By cycle, county grouping 2, and income
  by_county2_income <- hh_svy %>%
    group_by(survey_cycle, home_county_label_grouped2, income_detailed_grouped) %>%
    summarise(
      pct_with_delivery = survey_mean(!!sym(delivery_var), vartype = c("se", "ci", "cv"), level = CONF_LEVEL),
      n_weighted = survey_total(vartype = NULL),
      n = unweighted(n())
    ) %>%
    mutate(summary_level = "survey_cycle,home_county_label_grouped2,income_detailed_grouped")
  
  
  # Combine
  bind_rows(by_cycle, by_income, by_county1, by_county2, by_county1_income, by_county2_income) %>%
    arrange(survey_cycle, home_county_label_grouped, home_county_label_grouped2, income_detailed_grouped) %>%
    select(survey_cycle, home_county_label_grouped, home_county_label_grouped2, income_detailed_grouped, summary_level, everything()) %>%
    mutate(delivery_type = delivery_label)
}

# Run for both types
packages_summary <- analyze_delivery_type(hh_svy, "had_package_delivery", "Packages at Home")
food_summary <- analyze_delivery_type(hh_svy, "had_food_delivery", "Food or Groceries")

# Combine both
delivery_summary <- bind_rows(packages_summary, food_summary) %>%
  select(delivery_type, everything())

# Add formatted count string and reliability flags
delivery_summary <- delivery_summary %>%
  mutate(
    total_unweighted_str = paste0("N=", prettyNum(n, big.mark = ",", scientific = FALSE)),
    confidence_level = CONF_LEVEL,
    cv_flag = pct_with_delivery_cv > 0.30,
    sample_size_flag = n < 30,
    ci_width_flag = (pct_with_delivery_upp - pct_with_delivery_low) > 0.40,
    extreme_values_flag = pct_with_delivery_low < 0 | pct_with_delivery_upp > 1,
    suppress = cv_flag | sample_size_flag | ci_width_flag | extreme_values_flag,
    estimate_reliability = case_when(
      cv_flag ~ "Poor (High CV >30%)",
      sample_size_flag ~ "Poor (Small sample n<30)",
      ci_width_flag ~ "Poor (Wide CI >40pp)",
      extreme_values_flag ~ "Poor (Invalid range)",
      TRUE ~ "Acceptable"
    )
  )

# Generate timestamp
timestamp <- format(Sys.time(), '%Y%m%d_%H%M%S')

# Write CSV output
output_csv <- glue("{working_dir}/summarize_Deliveries_{timestamp}.csv")
write.csv(delivery_summary, file = output_csv, row.names = FALSE)
print(glue("Wrote {nrow(delivery_summary)} rows to {output_csv}"))

# Save RData output
output_file <- glue("{working_dir}/summarize_Deliveries_{timestamp}.Rdata")
save(delivery_summary, file = output_file)
print(glue("Wrote {nrow(delivery_summary)} rows to {output_file}"))