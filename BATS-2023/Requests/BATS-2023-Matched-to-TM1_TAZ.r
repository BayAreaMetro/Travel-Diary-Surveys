# BATS-2023-Matched-to-TM1_TAZ.r
# Remove point level data and match lat/lon values to TM1 (1454) TAZ geometry
# Delete block group and PUMA geographic detail
# Join with trip trace facility match file
# Create zero values for all trips that have no trip trace facilities 
# Output versions of files with geographic aggregation for sharing (with NDA)

# Set output directory

output        <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset Aggregated to TAZ 1454"

# Bring in libraries and set options to remove scientific notation

suppressMessages(library(tidyverse))
library(sf)
options(scipen = 999)

# Set up inputs directories

# Data file locations

file_location         <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset/WeightedDataset_08092024"
day_location          <- file.path(file_location,"day.csv")
hh_location           <- file.path(file_location,"hh.csv")
person_location       <- file.path(file_location,"person.csv")
trip_location         <- file.path(file_location,"trip.csv")
vehicle_location      <- file.path(file_location,"vehicle.csv")

# Facility conflation file location

USERPROFILE    <- gsub("////","/", Sys.getenv("USERPROFILE"))
BOX_dir1       <- file.path(USERPROFILE, "Box", "Modeling and Surveys","Surveys","Travel Diary Survey")
Box_dir2       <- file.path(BOX_dir1,"Biennial Travel Diary Survey","Data","2023")
conflation_loc <- file.path(Box_dir2,"Survey Conflation")

# Bring in datasets

day            <- read.csv(day_location)
household      <- read.csv(hh_location)
person         <- read.csv(person_location)
trip           <- read.csv(trip_location)
vehicle        <- read.csv(vehicle_location)

# Bring in TAZ shapefile

taz_shp    <- st_read("M:/Data/GIS layers/Travel_Analysis_Zones_(TAZ1454)/Travel Analysis Zones.shp") %>% 
  select(TAZ1454)

# Bring in facility conflation flag file

facility_flag <- read.csv(file = file.path(conflation_loc,"BATS 2023 Facility Use Booleans.csv"))

# Bring in data files and do the necessary steps to convert inputs to outputs (some files require no modifications)
# Where geocoding is necessary, assigned CRS=4326, World Geodetic System, then convert to 26910, spatially match
# Append TAZ locations, relocate variables to be near similar geolocation variables

# Day file has no location information, so no modifications are necessary

day_out <- day

# Household file needs geocoding of home location and to omit block group and PUMA variables

household_places <- household %>% 
  filter(!(is.na(home_lat)),!(is.na(home_lon))) %>% 
  st_as_sf(., coords = c("home_lon", "home_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(taz_shp))

household_out <- st_join(household_places,taz_shp, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry,-home_bg_2010,-home_bg_2020,
                              -home_puma_2012,-home_puma_2022,-sample_home_lat,-sample_home_lon,
                              -sample_home_bg) %>% 
  relocate(home_taz=TAZ1454,.before=home_county)

# Person file needs geocoding for school, work, and second home locations; omit block group and PUMA variables

person_school <- person %>% 
  filter(!(is.na(school_lat)),!(is.na(school_lon))) %>% 
  st_as_sf(., coords = c("school_lon", "school_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(taz_shp))

person_work <- person %>% 
  filter(!(is.na(work_lat)),!(is.na(work_lon))) %>% 
  st_as_sf(., coords = c("work_lon", "work_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(taz_shp))

person_second_home <- person %>% 
  filter(!(is.na(second_home_lat)),!(is.na(second_home_lon))) %>% 
  st_as_sf(., coords = c("second_home_lon", "second_home_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(taz_shp))

temp_person_school <- st_join(person_school,taz_shp, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,hh_id,school_taz=TAZ1454)

temp_person_work <- st_join(person_work,taz_shp, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,hh_id,work_taz=TAZ1454)

temp_person_second_home <- st_join(person_second_home,taz_shp, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,hh_id,second_home_taz=TAZ1454)

person_out <- person %>% 
  left_join(.,temp_person_school,by=c("person_id","hh_id")) %>% 
  left_join(.,temp_person_work,by=c("person_id","hh_id")) %>% 
  left_join(.,temp_person_second_home,by=c("person_id","hh_id")) %>% 
  relocate(school_taz,.before = school_county) %>% 
  relocate(work_taz,.before = work_county) %>% 
  relocate(second_home_taz,.before = second_home_county) %>% 
  select(-school_bg_2010,-school_bg_2020,-school_puma_2012,-school_puma_2022,
         -work_bg_2010,-work_bg_2020,-work_puma_2012,-work_puma_2022,
         -second_home_bg_2010,-second_home_bg_2020,-second_home_puma_2012,-second_home_puma_2022,
         -school_lat,-school_lon,-work_lat,-work_lon,-second_home_lat,-second_home_lon)

# Trip file needs origin/destination recoded, omission of block group and PUMA variables
# Join trip trace facility file, recode 0s for NA values creating 0/1 binary if facility fields used with rMove conflation

trip_origin <- trip %>% 
  filter(!(is.na(o_lat)),!(is.na(o_lon))) %>% 
  st_as_sf(., coords = c("o_lon", "o_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(taz_shp))

trip_destination <- trip %>% 
  filter(!(is.na(d_lat)),!(is.na(d_lon))) %>% 
  st_as_sf(., coords = c("d_lon", "d_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(taz_shp))

temp_trip_origin <- st_join(trip_origin,taz_shp, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,day_num, hh_id,trip_id,trip_num,o_taz=TAZ1454)

temp_trip_destination <- st_join(trip_destination,taz_shp, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,day_num, hh_id,trip_id,trip_num,d_taz=TAZ1454)

trip_out <- trip %>% 
  left_join(.,temp_trip_origin,by=c("person_id","day_num","hh_id","trip_id","trip_num")) %>% 
  left_join(.,temp_trip_destination,by=c("person_id","day_num","hh_id","trip_id","trip_num")) %>% 
  relocate(o_taz,.before = o_county) %>% 
  relocate(d_taz,.before = d_county) %>% 
  select(-o_lat,-o_lon,-d_lat,-d_lon,-o_bg_2010,-o_bg_2020,-o_puma_2012,-o_puma_2022,
         -d_bg_2010,-d_bg_2020,-d_puma_2012,-d_puma_2022) %>% 
  left_join(.,facility_flag,by="trip_id") %>% 
  mutate_at(vars(95:116), ~ ifelse(is.na(.), 0, .))

# Nothing recoded for vehicle file

vehicle_out <- vehicle 

# Output recoded files

write.csv(day_out,file.path(output,"BATS_2023_Day_TAZ1454.csv"),row.names = FALSE)
write.csv(household_out,file.path(output,"BATS_2019_Household_TAZ1454.csv"),row.names = FALSE)
write.csv(person_out,file.path(output,"BATS_2019_Person_TAZ1454.csv"),row.names = FALSE)
write.csv(trip_out,file.path(output,"BATS_2019_Trip_TAZ1454.csv"),row.names = FALSE)
write.csv(vehicle_out,file.path(output,"BATS_2019_Vehicle_TAZ1454.csv"),row.names = FALSE)

# Print to screen all variables with taz, tract, block group, lat/lon, PUMA to verify only relevant TAZ variables retained

print(grep("taz|tract|bg|_lat|_lon|puma",names(c(day_out,household_out,person_out,trip_out,vehicle_out)),value = T))

