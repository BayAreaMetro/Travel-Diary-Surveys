# BATS-2023-Matched-to-TM1_TAZ.r
# Remove point level data and match TM1 (1454) TAZs
# Join with trip trace facility match file

# Set output directory

Output        <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset Aggregated to TAZ 1454"

# Bring in libraries and set options to remove scientific notation

suppressMessages(library(tidyverse))
library(sf)
options(scipen = 999)

# Set up inputs directories

file_location         <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset/WeightedDataset_08092024"
day_location          <- file.path(file_location,"day.csv")
hh_location           <- file.path(file_location,"hh.csv")
person_location       <- file.path(file_location,"person.csv")
trip_location         <- file.path(file_location,"trip.csv")
vehicle_location      <- file.path(file_location,"vehicle.csv")

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

# Bring in shapefile

taz_shp    <- st_read("M:/Data/GIS layers/Travel_Analysis_Zones_(TAZ1454)/Travel Analysis Zones.shp") %>% 
  select(TAZ1454)

# Bring in facility flag for later merging

facility_flag <- read.csv(file = file.path(conflation_loc,"BATS 2023 Facility Use Booleans.csv"))

# Where geocoding is necessary, assigned CRS=4326, World Geodetic System, then convert to 26910, spatially match
# Append TAZ locations, relocate variables to be near similar geolocation variables

# Day file has no location information

day_out <- day

# Household file needs geocoding

household_places <- household %>% 
  filter(!(is.na(home_lat)),!(is.na(home_lon))) %>% 
  st_as_sf(., coords = c("home_lon", "home_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(taz_shp))

household_out <- st_join(household_places,taz_shp, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry,-home_bg_2010,-home_bg_2020,
                              -home_puma_2012,-home_puma_2022,-sample_home_lat,-sample_home_lon,
                              -sample_home_bg) %>% 
relocate(home_taz=TAZ1454,.before=home_county)

# Person file needs geocoding for school and work locations

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

# Trip file needs origin/destination recoded

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
  select(person_id,day_num, hh_id,person_num,trip_id,trip_num,linked_trip_id,leg_num,origin_tract_geoid=GEOID)

temp_trip_destination <- st_join(trip_destination,taz_shp, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,day_num, hh_id,person_num,trip_id,trip_num,linked_trip_id,leg_num,destination_tract_geoid=GEOID)

trip_out <- trip %>% 
  left_join(.,temp_trip_origin,by=c("person_id","day_num","hh_id","person_num","trip_id","trip_num",
                                    "linked_trip_id","leg_num")) %>% 
  left_join(.,temp_trip_destination,by=c("person_id","day_num","hh_id","person_num","trip_id","trip_num",
                                         "linked_trip_id","leg_num")) %>% 
  relocate(origin_tract_geoid,.before = o_county_fips) %>% 
  relocate(destination_tract_geoid,.before = d_county_fips) %>% 
  select(-o_bg_geo_id,-d_bg_geo_id,-o_lat,-o_lon,-d_lat,-d_lon,-o_taz,-d_taz)

# Nothing recoded for vehicle file

vehicle_out <- vehicle 

# Output recoded files

write.csv(day_out,file.path(Output,"BATS_2019_Day.csv"),row.names = FALSE)
write.csv(household_out,file.path(Output,"BATS_2019_Household.csv"),row.names = FALSE)
write.csv(person_out,file.path(Output,"BATS_2019_Person.csv"),row.names = FALSE)
write.csv(trip_out,file.path(Output,"BATS_2019_Trip.csv"),row.names = FALSE)
write.csv(vehicle_out,file.path(Output,"BATS_2019_Vehicle.csv"),row.names = FALSE)

