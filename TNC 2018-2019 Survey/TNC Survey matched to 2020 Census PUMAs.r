# TNC Survey matched to 2020 Census PUMAs.r
# Remove point level data and match 2020 Census geographies
# Code first to tract and then recode to PUMA (easier based on lack of PUMA shapefile)

# Set output directory

Output        <- "M:/Data/HomeInterview/TNC Survey/Data/Task 8 Data Refinement Version/PUMA Matched Version for NDA-Free Sharing/Bay Area Travel Survey 2019 Data"

# Bring in libraries

library(tigris)
suppressMessages(library(tidyverse))
library(sf)

# Set up working directory

temp                  <- "M:/Data/HomeInterview/TNC Survey/Data/Task 8 Data Refinement Version"
file_location         <- file.path(temp,"Final Updated Dataset as of 10-18-2021","RSG_HTS_Oct2021_bayarea")
day_location          <- file.path(file_location,"day.tsv")
hh_location           <- file.path(file_location,"hh.tsv")
person_location       <- file.path(file_location,"person.tsv")
trip_location         <- file.path(file_location,"trip.tsv")
trip_linked_location  <- file.path(file_location,"trip_linked.tsv")
vehicle_location      <- file.path(file_location,"vehicle.tsv")


# Bring in datasets

day            <- read_tsv(day_location,col_names=TRUE)
household      <- read_tsv(hh_location,col_names=TRUE)
person         <- read_tsv(person_location,col_names=TRUE)
trip           <- read_tsv(trip_location,col_names=TRUE)
linked_trip    <- read_tsv(trip_linked_location,col_names=TRUE)
vehicle        <- read_tsv(vehicle_location,col_names=TRUE)


# Fetch all CA 2020 tracts
# Convert projection to NAD83 / UTM zone 10N (ESPG 26910)

ca_tracts <- tracts(
  state = "CA",
  year = 2020
) %>% 
  st_transform(.,crs = 26910) %>% 
  select(GEOID,geometry)

# tract to PUMA equivalency file, create merging variable, only CA
# https://www.census.gov/programs-surveys/geography/guidance/geo-areas/pumas.html

tract_PUMA <- read.csv("M:/Data/Census/2020/PUMA 2020/PUMA 2020 Tract PUMA Equivalency.csv",header = T, 
                  colClasses = c(STATEFP="character",COUNTYFP="character",TRACTCE="character",
                                 PUMA5CE="character")) %>% 
  mutate(GEOID=paste0(STATEFP,COUNTYFP,TRACTCE)) %>% 
  filter(STATEFP=="06")

ca_pumas <- left_join(ca_tracts,tract_PUMA, by="GEOID") %>% select(-GEOID,-STATEFP,-COUNTYFP,-TRACTCE)

# Where geocoding is necessary, assigned CRS=4326, World Geodetic System, then convert to 26910, spatially match
# Append census tract locations, relocate variables to be near similar geolocation variables

# Day file has no location information

day_out <- day

# Household file needs geocoding

household_places <- household %>% 
  filter(!(is.na(reported_home_lat)),!(is.na(reported_home_lon))) %>% 
  st_as_sf(., coords = c("reported_home_lon", "reported_home_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(ca_pumas))

household_out <- st_join(household_places,ca_pumas, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry,-sample_home_lat,-sample_home_lon,-home_bg_geoid,-home_taz,-home_puma) %>% 
  relocate(home_PUMA_geoid=PUMA5CE,.before=home_county_fips)

# Person file needs geocoding for school and work locations

person_school <- person %>% 
  filter(!(is.na(school_lat)),!(is.na(school_lon))) %>% 
  st_as_sf(., coords = c("school_lon", "school_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(ca_pumas))

person_work <- person %>% 
  filter(!(is.na(work_lat)),!(is.na(work_lon))) %>% 
  st_as_sf(., coords = c("work_lon", "work_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(ca_pumas))

temp_person_school <- st_join(person_school,ca_pumas, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,hh_id,person_num,school_puma_geoid=PUMA5CE)

temp_person_work <- st_join(person_work,ca_pumas, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,hh_id,person_num,work_puma_geoid=PUMA5CE)

person_out <- person %>% 
  left_join(.,temp_person_school,by=c("person_id","hh_id","person_num")) %>% 
  left_join(.,temp_person_work,by=c("person_id","hh_id","person_num")) %>% 
  relocate(school_puma_geoid,.before = school_county_fips) %>% 
  relocate(work_puma_geoid,.before = work_county_fips) %>% 
  select(-school_bg_geo_id,-work_bg_geo_id,-school_lat,-school_lon,-work_lat,-work_lon,-school_taz,-work_taz)

# Trip file needs origin/destination recoded

trip_origin <- trip %>% 
  filter(!(is.na(o_lat)),!(is.na(o_lon))) %>% 
  st_as_sf(., coords = c("o_lon", "o_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(ca_pumas))

trip_destination <- trip %>% 
  filter(!(is.na(d_lat)),!(is.na(d_lon))) %>% 
  st_as_sf(., coords = c("d_lon", "d_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(ca_pumas))

temp_trip_origin <- st_join(trip_origin,ca_pumas, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,day_num, hh_id,person_num,trip_id,trip_num,linked_trip_id,leg_num,origin_puma_geoid=PUMA5CE)

temp_trip_destination <- st_join(trip_destination,ca_pumas, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,day_num, hh_id,person_num,trip_id,trip_num,linked_trip_id,leg_num,destination_puma_geoid=PUMA5CE)

trip_out <- trip %>% 
  left_join(.,temp_trip_origin,by=c("person_id","day_num","hh_id","person_num","trip_id","trip_num",
                                    "linked_trip_id","leg_num")) %>% 
  left_join(.,temp_trip_destination,by=c("person_id","day_num","hh_id","person_num","trip_id","trip_num",
                                         "linked_trip_id","leg_num")) %>% 
  relocate(origin_puma_geoid,.before = o_county_fips) %>% 
  relocate(destination_puma_geoid,.before = d_county_fips) %>% 
  select(-o_bg_geo_id,-d_bg_geo_id,-o_lat,-o_lon,-d_lat,-d_lon,-o_taz,-d_taz)

# Linked trip file needs origin/destination recoded

linked_trip_origin <- linked_trip %>% 
  filter(!(is.na(o_lat)),!(is.na(o_lon))) %>% 
  st_as_sf(., coords = c("o_lon", "o_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(ca_pumas))

linked_trip_destination <- linked_trip %>% 
  filter(!(is.na(d_lat)),!(is.na(d_lon))) %>% 
  st_as_sf(., coords = c("d_lon", "d_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(ca_pumas))

temp_linked_trip_origin <- st_join(linked_trip_origin,ca_pumas, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,day_num, hh_id,person_num,linked_trip_id,origin_puma_geoid=PUMA5CE)

temp_linked_trip_destination <- st_join(linked_trip_destination,ca_pumas, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,day_num, hh_id,person_num,linked_trip_id,destination_puma_geoid=PUMA5CE)

linked_trip_out <- linked_trip %>% 
  left_join(.,temp_linked_trip_origin,by=c("person_id","day_num","hh_id","person_num",
                                    "linked_trip_id")) %>% 
  left_join(.,temp_linked_trip_destination,by=c("person_id","day_num","hh_id","person_num",
                                         "linked_trip_id")) %>% 
  relocate(origin_puma_geoid,.before = o_county_fips) %>% 
  relocate(destination_puma_geoid,.before = d_county_fips) %>% 
  select(-o_bg_geo_id,-d_bg_geo_id,-o_lat,-o_lon,-d_lat,-d_lon,-o_taz,-d_taz)

# Nothing recoded for vehicle file

vehicle_out <- vehicle 

# Output recoded files

write.csv(day_out,file.path(Output,"BATS_2019_Day.csv"),row.names = FALSE)
write.csv(household_out,file.path(Output,"BATS_2019_Household.csv"),row.names = FALSE)
write.csv(person_out,file.path(Output,"BATS_2019_Person.csv"),row.names = FALSE)
write.csv(trip_out,file.path(Output,"BATS_2019_Trip.csv"),row.names = FALSE)
write.csv(linked_trip_out,file.path(Output,"BATS_2019_Linked_Trip.csv"),row.names = FALSE)
write.csv(vehicle_out,file.path(Output,"BATS_2019_Vehicle.csv"),row.names = FALSE)
