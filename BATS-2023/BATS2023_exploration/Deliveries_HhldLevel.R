library(dplyr)
library(readr)
library(glue)
library(tidyr)
library(sf)

# -----------
# Read input
# -----------

# Set dataset version
survey_cycle <- "BATS2023"  # Can be "BATS2019" or "BATS2023"

# Set directory based on survey name
if (survey_cycle == "BATS2023") {

  weighted_dataset_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset/WeightedDataset_02212025"

  day_file <- "day.csv"
  hh_file <- "hh.csv"

  day_path <- file.path(weighted_dataset_dir, day_file)
  hh_path <- file.path(weighted_dataset_dir, hh_file)

  day_df <- read_csv(day_path)
  hh_df <- read_csv(hh_path)

  # The 2023 dataset contains blank weight values; replace them with 0
  hh_df <- hh_df %>%
      mutate(hh_weight_rmove_only = replace_na(hh_weight_rmove_only, 0))

} else if (survey_cycle == "BATS2019") {

  weighted_dataset_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations/Final Updated Dataset as of 10-18-2021"

  day_file <- "day.tsv"
  hh_file <- "hh.tsv"

  day_path <- file.path(weighted_dataset_dir, day_file)
  hh_path <- file.path(weighted_dataset_dir, hh_file)

  day_df <- read_table(day_path)
  hh_df <- read_table(hh_path)

} else {
  stop("survey_cycle must be either 'BATS2019' or 'BATS2023'")
}


# -----------
# add geographical information
# -----------
if (survey_cycle == "BATS2023") {

# Bring in TAZ shapefile as a sf (simple features) object, a spatial data frame with a special geometry column
taz_sf    <- st_read("M:/Data/GIS layers/Travel_Analysis_Zones_(TAZ1454)/Travel Analysis Zones.shp") %>% 
  select(TAZ1454)

# Converts the hh file into an simple features (sf) spatial object, using home_lon and home_lat as coordinates, assuming they are in WGS84 (EPSG:4326, typical GPS lat/lon).
# Reprojects those points into the same coordinate reference system as the taz file so spatial join works properly
hh_sf <- hh_df %>% 
  st_as_sf(., coords = c("home_lon", "home_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(taz_sf))

# Perform spatial join
hh_taz_df <- st_join(hh_sf,taz_sf, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  relocate(home_taz=TAZ1454,.before=home_county)

# check how many households didn’t match to any TAZ
# for BATS2023, it's only 28 households, which is 0.34%
summary_hh_noTAZ <- hh_taz_df %>%
  summarise(
    total_households = n(),
    missing_home_taz = sum(is.na(home_taz)),
    pct_missing = round(100 * missing_home_taz / total_households, 2)
  )

print(" ")
print("-------------------------------------------------")
print("check how many households didn’t match to any TAZ")
print("-------------------------------------------------")
print(summary_hh_noTAZ)
print("-------------------------------------------------")
print(" ")

# TODO: review the non-matched hh on a map
# Drop households not matched to any TAZ
hh_taz_df <- hh_taz_df %>%
  filter(!is.na(home_taz))

# Confirm the number of remaining records
hh_taz_df %>%
  summarise(
    remaining_households = n()
  )

# Bring in a tazdata file for Area Type information
# Area Type in TM1: 0=regional core, 1=central business district, 2=urban business, 3=urban, 4=suburban, 5=rural
tazData_df <- read_csv("M:/Application/Model One/RTP2025/IncrementalProgress/2023_TM160_IPA_57/INPUT/landuse/tazData.csv")

# Join the Area Type variable to hh_taz_df
hh_taz_df <- hh_taz_df %>%
  left_join(
    tazData_df %>% select(ZONE, AREATYPE),
    by = c("home_taz" = "ZONE")
  )

# add labels
hh_taz_df <- hh_taz_df %>%
  mutate(AREATYPE_3cat_label = recode(
    AREATYPE,
    `0` = "Regional Core and CBD",
    `1` = "Regional Core and CBD",
    `2` = "Urban",
    `3` = "Urban",
    `4` = "Suburban and Rural",
    `5` = "Suburban and Rural"
  ))

hh_taz_df <- hh_taz_df %>%
  relocate(AREATYPE, AREATYPE_3cat_label, .after = home_taz)

}

# -----------
# Filter if the deliveries were received on a travel day
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

if (survey_cycle == "BATS2023") {

   delivery_cols <- c('delivery_2', 'delivery_4', 'delivery_5', 'delivery_6', 'delivery_7', 'delivery_8', 'delivery_9')

} else if (survey_cycle == "BATS2019") {

  delivery_cols <- c('delivery_home', 'delivery_work', 'delivery_locker', 'delivery_food', 'delivery_other')

}
 
delivery_rows_df <- day_df %>%
  filter(if_any(all_of(delivery_cols), ~ . == 1))

delivery_households_df <- delivery_rows_df %>%
  select(hh_id) %>%
  distinct() %>%
  mutate(had_delivery = 1)

# Merge with household data
hh_delivery_df <- hh_taz_df %>%
  left_join(delivery_households_df, by = "hh_id")

# Check merge results (equivalent to indicator=True)
table(is.na(hh_delivery_df$had_delivery))

# Replace NA with 0 for had_delivery
hh_delivery_df <- hh_delivery_df %>%
  mutate(had_delivery = replace_na(had_delivery, 0))


# ------
# Calculate high-level weighted statistics
# -----
# Note that the weight variable name differ by survey cycle
# Define the weight variable here
if (survey_cycle == "BATS2023") {
   weight_var <- hh_delivery_df$hh_weight_rmove_only
} else if (survey_cycle == "BATS2019") {
   weight_var <- hh_delivery_df$wt_sphone_wkday
}

num_hh_delivery   <- sum(hh_delivery_df$had_delivery * weight_var)
num_hh            <- sum(weight_var)

#also want to know the number of unweighted households
num_hh_unweighted <- hh_delivery_df %>%
  filter(hh_weight_rmove_only > 0) %>%
  nrow()

print(glue("Number of households had a delivery (weighted): {num_hh_delivery}"))
print(glue("Number of households (weighted): {num_hh}"))


Percent_Had_Delivery <- num_hh_delivery / num_hh
print(glue("Percent of households that had a delivery: {Percent_Had_Delivery}"))
print(glue("Number of households (unweighted): {num_hh_unweighted}"))

# ------
# Summarise by area type (3 categories)
# -----

summary_by_3areatype <- hh_delivery_df %>%
  group_by(AREATYPE_3cat_label) %>%
  summarise(
    num_hh_delivery = sum(had_delivery * hh_weight_rmove_only),
    num_hh = sum(hh_weight_rmove_only),
    Percent_Had_Delivery = num_hh_delivery / num_hh
  )

summary_by_3areatype 
