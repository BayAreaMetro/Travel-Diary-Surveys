# TNC Survey matched to BART TODs.r
# Remove point level data and match to BART TOD geographies

# Set output directory

Output        <- "M:/Data/Requests/Seung-Yen Hong"

# Bring in libraries

library(tigris)
suppressMessages(library(tidyverse))
library(sf)

# Set up working directory

temp                  <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations"
file_location         <- file.path(temp,"Final Updated Dataset as of 10-18-2021/RSG_HTS_Oct2021_bayarea")
day_location          <- file.path(file_location,"day.tsv")
hh_location           <- file.path(file_location,"hh.tsv")
person_location       <- file.path(file_location,"person.tsv")
trip_location         <- file.path(file_location,"trip.tsv")
trip_linked_location  <- file.path(file_location,"trip_linked.tsv")
vehicle_location      <- file.path(file_location,"vehicle.tsv")


# Bring in datasets

#day            <- read_tsv(day_location,col_names=TRUE)
household      <- read_tsv(hh_location,col_names=TRUE)
#person         <- read_tsv(person_location,col_names=TRUE)
#trip           <- read_tsv(trip_location,col_names=TRUE)
#linked_trip    <- read_tsv(trip_linked_location,col_names=TRUE)
#vehicle        <- read_tsv(vehicle_location,col_names=TRUE)


# Bring in TOD shapefile
# Convert projection to NAD83 / UTM zone 10N (ESPG 26910)

shapefile_path <- file.path(Output,"TOD_Parcels_Merged.shp")
TOD_shape <- st_read(dsn = shapefile_path) %>% 
  st_transform(.,crs = 26910) %>% 
  select(TOD_Station=Station,TOD_Name=Name,TOD_County=County,geometry)

# Day file has no location information

day_out <- day

# Household file needs geocoding

household_places <- household %>% 
  filter(!(is.na(reported_home_lat)),!(is.na(reported_home_lon))) %>% 
  st_as_sf(., coords = c("reported_home_lon", "reported_home_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(TOD_shape)) 

household_out <- st_join(household_places,TOD_shape, join=st_within) %>%
  as.data.frame(.) %>% select(-geometry,-sample_home_lat,-sample_home_lon,-home_bg_geoid,-home_taz,-home_puma) %>% 
  relocate(c(TOD_Station,TOD_Name,TOD_County),.before=home_county_fips)

print (sum(is.na(household_out["TOD_Station"])))

