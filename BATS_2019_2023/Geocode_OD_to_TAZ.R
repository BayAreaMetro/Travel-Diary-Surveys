# Load required libraries
library(readr)
library(dplyr)
library(glue)
library(sf)      # simple features, for spatial joins

# -------------------------
# Read BATS data
# -------------------------

# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023"

# Start a log file
log_file <- glue("{working_dir}/multi-cycle_BATS_processing_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for creating a multi-cycle BATS dataset: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

# Read 2023 linked trip file
# Suppress progress bar for cleaner log output
LinkedTrips2023_df <- read_csv("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/TripLinking_20251012/03b-assign_day/wt-wkday_4day/trip.csv",
                                progress = FALSE) %>% 
  mutate(survey_cycle = 2023)

print(glue("LinkedTrips2023_df:"))
print(glue("  Observations: {nrow(LinkedTrips2023_df)}"))
print(glue("  Columns: {ncol(LinkedTrips2023_df)}"))
cat("\n")

# Read 2019 linked trip file
# Note that the 2019 file is space delimited
LinkedTrips2019_df <- read.table("E:/Box/Modeling and Surveys/Surveys/Travel Diary Survey/MPO Partner Household Travel Survey (Includes 2018_2019 TNC Survey)/Bay Area Travel Study 2018-2019/Data from 2018_2019 Survey/BATS2018_2019_SFCTA_processed/20200228/survey2018_tripx.dat",
                                  header = TRUE,
                                  sep = "") %>% 
  mutate(survey_cycle = 2019)


print(glue("LinkedTrips2019_df:"))
print(glue("  Observations: {nrow(LinkedTrips2019_df)}"))
print(glue("  Columns: {ncol(LinkedTrips2019_df)}"))
cat("\n")

# Union the two dataframes
LinkedTrips_2019_2023_df <- bind_rows(LinkedTrips2023_df, LinkedTrips2019_df)

# Check the combined data frame
print(glue("LinkedTrips_2019_2023_df:"))
print(glue("  Observations: {nrow(LinkedTrips_2019_2023_df)}"))
print(glue("  Columns: {ncol(LinkedTrips_2019_2023_df)}"))
cat("\n")

print(glue("Observations by survey cycle:"))
print(table(LinkedTrips_2019_2023_df$survey_cycle))
cat("\n")

# Check column differences
cols_2023_only <- setdiff(names(LinkedTrips2023_df), names(LinkedTrips2019_df))
cols_2019_only <- setdiff(names(LinkedTrips2019_df), names(LinkedTrips2023_df))
cols_common <- intersect(names(LinkedTrips2023_df), names(LinkedTrips2019_df))

print(glue("\nColumn comparison:"))
print(glue("  Columns in 2023 only ({length(cols_2023_only)}): {paste(cols_2023_only, collapse=', ')}"))
print(glue("  Columns in 2019 only ({length(cols_2019_only)}): {paste(cols_2019_only, collapse=', ')}"))
print(glue("  Columns in both ({length(cols_common)}): {paste(cols_common, collapse=', ')}"))
cat("\n")

# Check if trexpfac and pdexpfac are identical
are_identical <- identical(LinkedTrips2019_df$trexpfac, LinkedTrips2019_df$pdexpfac)
print(glue("Are trexpfac and pdexpfac identical? {are_identical}"))

# If not identical, show differences
if (!are_identical) {
  num_differences <- sum(LinkedTrips2019_df$trexpfac != LinkedTrips2019_df$pdexpfac, na.rm = TRUE)
  print(glue("  Number of differing values: {num_differences}"))
}

# Remove the columns that are in 2019 only, as they appear to come from sfcta's model skims travtime, travcost, travdist
# And pdexpfac is identical to trexpfac. It doesn't add value and can be dropped
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  select(-travtime, -travcost, -travdist, -pdexpfac)

# Note that MTC didn't process the 2019 data ourselves, so variables like otaz and dtaz in this file are not from MTC's model
# Delete otaz and dtaz to minimize confusion
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  select(-otaz, -dtaz)

# -----------
# add TM1 TAZ
# -----------
sf_use_s2(FALSE)  # Disable spherical geometry (otherwise it thinks that the taz file has invalid geometry)

# Bring in TAZ shapefile as a sf (simple features) object, a spatial data frame with a special geometry column
taz_sf    <- st_read("X:/travel-model-one-master/utilities/geographies/bayarea_rtaz1454_rev1_WGS84.shp") %>% 
  select(TAZ1454)
#taz_sf    <- st_read("M:/Data/GIS layers/Travel_Analysis_Zones_(TAZ1454)/Travel Analysis Zones.shp") %>% 
#  select(TAZ1454)

# make copies of the columns with coordinates (to be used for the spatial join verification later)
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(
    oxco_copy = oxco,
    oyco_copy = oyco,
    dxco_copy = dxco,
    dyco_copy = dyco
  )

# origins
# Converts the linked trip file into an simple features (sf) spatial object, using oxco (dxco) and oyco (dyco) as coordinates, assuming they are in WGS84 (EPSG:4326, typical GPS lat/lon).
# Reprojects those points into the same coordinate reference system as the taz file so spatial join works properly
LinkedTrips_o2019_2023_sf <- LinkedTrips_2019_2023_df %>%
  st_as_sf(., coords = c("oxco", "oyco"), crs = 4326) %>% 
  st_transform(., crs=st_crs(taz_sf))

# Perform spatial join
LinkedTrips_2019_2023_df <- st_join(LinkedTrips_o2019_2023_sf,taz_sf, join=st_intersects,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  rename(oTAZ1454 = TAZ1454)

# check how many trips didn’t match to any TAZ
summary_trip_o_noTAZ <- LinkedTrips_2019_2023_df %>%
  summarise(
    total_trips= n(),
    missing_o_taz1454 = sum(is.na(oTAZ1454)),
    pct_missing = round(100 * missing_o_taz1454 / total_trips, 2)
  )


cat("\n")
print("check how many trip did not match to any origin TAZ")
print(summary_trip_o_noTAZ)
cat("\n")

# For unmatched origins, buffer by 10 meters and retry
# unmatched_o <- is.na(LinkedTrips_2019_2023_df$oTAZ1454)
# if(sum(unmatched_o) > 0) {
#   print(glue("Attempting to match {sum(unmatched_o)} unmatched origins with 10m buffer..."))
  
#   unmatched_sf <- LinkedTrips_o2019_2023_sf[unmatched_o, ] %>%
#     st_buffer(., dist = 10)  # 10 meter buffer (in projected CRS units)
  
#   matched <- st_join(unmatched_sf, taz_sf, join=st_intersects, left=TRUE, largest=TRUE) %>%
#     as.data.frame() %>% select(-geometry)
  
#   LinkedTrips_2019_2023_df$oTAZ1454[unmatched_o] <- matched$TAZ1454
  
#   print(glue("  Matched {sum(!is.na(matched$TAZ1454))} additional origins"))
# }

# check how many trips didn’t match to any TAZ
summary_trip_o_noTAZ <- LinkedTrips_2019_2023_df %>%
  summarise(
    total_trips= n(),
    missing_o_taz1454 = sum(is.na(oTAZ1454)),
    pct_missing = round(100 * missing_o_taz1454 / total_trips, 2)
  )

cat("\n")
print("check how many trip did not match to any origin TAZ")
print(summary_trip_o_noTAZ)
cat("\n")

# destinations
LinkedTrips_d2019_2023_sf <- LinkedTrips_2019_2023_df %>%
  st_as_sf(., coords = c("dxco", "dyco"), crs = 4326) %>% 
  st_transform(., crs=st_crs(taz_sf))

# Perform spatial join
LinkedTrips_2019_2023_df <- st_join(LinkedTrips_d2019_2023_sf,taz_sf, join=st_intersects,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  rename(dTAZ1454 = TAZ1454)


# check how many trips didn’t match to any TAZ
summary_trip_d_noTAZ <- LinkedTrips_2019_2023_df %>%
  summarise(
    total_trips= n(),
    missing_d_taz1454 = sum(is.na(dTAZ1454)),
    pct_missing = round(100 * missing_d_taz1454 / total_trips, 2)
  )

cat("\n")
print("check how many trip did not match to any destination TAZ")
print(summary_trip_d_noTAZ)
cat("\n")

# For unmatched destination, buffer by 10 meters and retry
# unmatched_d <- is.na(LinkedTrips_2019_2023_df$dTAZ1454)
# if(sum(unmatched_d) > 0) {
#   print(glue("Attempting to match {sum(unmatched_d)} unmatched destinations with 10m buffer..."))
  
#   unmatched_sf <- LinkedTrips_d2019_2023_sf[unmatched_d, ] %>%
#     st_buffer(., dist = 10)  # 10 meter buffer (in projected CRS units)
  
#   matched <- st_join(unmatched_sf, taz_sf, join=st_intersects, left=TRUE, largest=TRUE) %>%
#     as.data.frame() %>% select(-geometry)
  
#   LinkedTrips_2019_2023_df$dTAZ1454[unmatched_d] <- matched$TAZ1454
  
#   print(glue("  Matched {sum(!is.na(matched$TAZ1454))} additional destinations"))
# }


# check how many trips didn’t match to any TAZ
summary_trip_d_noTAZ <- LinkedTrips_2019_2023_df %>%
  summarise(
    total_trips= n(),
    missing_d_taz1454 = sum(is.na(dTAZ1454)),
    pct_missing = round(100 * missing_d_taz1454 / total_trips, 2)
  )

cat("\n")
print("check how many trip did not match to any destination TAZ")
print(summary_trip_d_noTAZ)
cat("\n")


# -----------
# Add the distance skims
# -----------
# Read the skims
distance_skim_df <- read.csv("\\\\model3-b\\Model3B-Share\\Projects\\2023_TM161_IPA_35\\database\\DistanceSkimsDatabaseAM.csv") %>%
  select(orig, dest, daToll) %>%
  rename(skim_dist_in_miles = daToll)

# Left join LinkedTrips_2019_2023_df to distance_skim_df
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  left_join(distance_skim_df, by = c("oTAZ1454" = "orig", "dTAZ1454" = "dest"))


# check how many trips didn't match to any distance
summary_trip_noDist <- LinkedTrips_2019_2023_df %>%
  summarise(
    total_trips = n(),
    missing_dist = sum(is.na(skim_dist_in_miles)),
    pct_missing = round(100 * missing_dist / total_trips, 2)
  )

cat("\n")
print("check how many trips did not match to any distance")
print(summary_trip_noDist)
cat("\n")



# -----------
# Calculate crow fly distance using Haversine formula
# -----------
print("Calculating crow fly (straight line) distances using Haversine formula...")

# Haversine formula function
# Inputs: lon1, lat1, lon2, lat2 in decimal degrees
# Output: distance in miles
haversine_distance <- function(lon1, lat1, lon2, lat2) {
  # Earth's radius in miles
  R <- 3959
  
  # Convert degrees to radians
  lon1_rad <- lon1 * pi / 180
  lat1_rad <- lat1 * pi / 180
  lon2_rad <- lon2 * pi / 180
  lat2_rad <- lat2 * pi / 180
  
  # Haversine formula
  dlon <- lon2_rad - lon1_rad
  dlat <- lat2_rad - lat1_rad
  
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  distance <- R * c
  return(distance)
}

# Calculate crow fly distance for all trips
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(
    crow_fly_miles = haversine_distance(oxco_copy, oyco_copy, dxco_copy, dyco_copy)
  )

# Summary statistics
summary_crow_fly <- LinkedTrips_2019_2023_df %>%
  summarise(
    total_trips = n(),
    with_crow_fly = sum(!is.na(crow_fly_miles)),
    missing_crow_fly = sum(is.na(crow_fly_miles)),
    mean_distance = round(mean(crow_fly_miles, na.rm = TRUE), 2),
    median_distance = round(median(crow_fly_miles, na.rm = TRUE), 2),
    max_distance = round(max(crow_fly_miles, na.rm = TRUE), 2)
  )
  
cat("\n")
print("Crow fly distance summary:")
print(summary_crow_fly)
cat("\n")

# add a new var to cap crow fly miles to 200 miles
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df %>%
  mutate(
    crow_fly_miles_cap200 = pmin(crow_fly_miles, 200)
  )

# Write LinkedTrips_2019_2023_df to csv for subsequent processes
output_trips_csv <- glue("{working_dir}/LinkedTrips_2019_2023_withDist.csv")
write.csv(LinkedTrips_2019_2023_df, file = output_trips_csv, row.names = FALSE)
print(glue("Wrote {nrow(LinkedTrips_2019_2023_df)} rows to {output_trips_csv}"))

sink() # to close the log file connection

