library(dplyr)
library(readr)
library(glue)
library(tidyr)


# Read input

# Set dataset version
survey_name <- "BATS2019"  # Can be "BATS2019" or "BATS2023"

# Set directory based on survey name
if (survey_name == "BATS2023") {

  weighted_dataset_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset/WeightedDataset_02212025"

  day_file <- "day.csv"
  hh_file <- "hh.csv"

  day_path <- file.path(weighted_dataset_dir, day_file)
  hh_path <- file.path(weighted_dataset_dir, hh_file)

  day_df <- read_csv(day_path)
  hh_df <- read_csv(hh_path)

} else if (survey_name == "BATS2019") {

  weighted_dataset_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations/Final Updated Dataset as of 10-18-2021"

  day_file <- "day.tsv"
  hh_file <- "hh.tsv"

  day_path <- file.path(weighted_dataset_dir, day_file)
  hh_path <- file.path(weighted_dataset_dir, hh_file)

  day_df <- read_table(day_path)
  hh_df <- read_table(hh_path)

} else {
  stop("survey_name must be either 'BATS2019' or 'BATS2023'")
}


# Filter if the deliveries were received on a travel day

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

if (survey_name == "BATS2023") {

   delivery_cols <- c('delivery_2', 'delivery_4', 'delivery_5', 'delivery_6', 'delivery_7', 'delivery_8', 'delivery_9')

} else if (survey_name == "BATS2019") {

  delivery_cols <- c('delivery_home', 'delivery_work', 'delivery_locker', 'delivery_food', 'delivery_other')

}
 
delivery_rows_df <- day_df %>%
  filter(if_any(all_of(delivery_cols), ~ . == 1))

delivery_households_df <- delivery_rows_df %>%
  select(hh_id) %>%
  distinct() %>%
  mutate(had_delivery = 1)

# Merge with household data
hh_merged_df <- hh_df %>%
  left_join(delivery_households_df, by = "hh_id")

# Check merge results (equivalent to indicator=True)
table(is.na(hh_merged_df$had_delivery))

# Replace NA with 0 for had_delivery
hh_merged_df <- hh_merged_df %>%
  mutate(had_delivery = replace_na(had_delivery, 0))

# Calculate weighted statistics
if (survey_name == "BATS2023") {
  
  num_hh_delivery <- sum(hh_merged_df$had_delivery * hh_merged_df$hh_weight_rmove_only)
  num_hh <- sum(hh_merged_df$hh_weight_rmove_only)

} else if (survey_name == "BATS2019") {
  
  num_hh_delivery <- sum(hh_merged_df$had_delivery * hh_merged_df$wt_sphone_wkday)
  num_hh <- sum(hh_merged_df$wt_sphone_wkday)

}

print(glue("Number of households had a delivery (weighted): {num_hh_delivery}"))
print(glue("Number of households in hh_df (weighted): {num_hh}"))


Percent_Had_Delivery <- num_hh_delivery / num_hh
print(glue("Percent of households that had a delivery: {Percent_Had_Delivery}"))

