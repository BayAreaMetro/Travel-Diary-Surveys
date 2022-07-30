# TNC Survey matched to 2020 Census Tracts.r
# Remove point level data and match 2020 Census geographies

# Bring in libraries

library(tigris)
suppressMessages(library(tidyverse))
library(sf)

# Set up working directory

temp                  <- "M:/Data/HomeInterview/TNC Survey/Data/Task 8 Data Refinement Version"
file_location         <- file.path(temp,"Final Updated Dataset as of 10-18-2021","RSG_HTS_Oct2021_bayarea")
day_location          <- file.path(file_location,"day.tsv")
hh_location           <- file.path(file_location,"hh.tsv")
location_location     <- file.path(file_location,"location.tsv")
person_location       <- file.path(file_location,"person.tsv")
trip_location         <- file.path(file_location,"trip.tsv")
trip_linked_location  <- file.path(file_location,"trip_linked.tsv")
trip_w_other_location <- file.path(file_location,"trip_with_purpose_other.tsv")
vehicle_location      <- file.path(file_location,"vehicle.tsv")

# Set up vector of counties for Bay Area and beyond

megaregion <- c("Alameda","Contra Costa","Marin","Napa","San Francisco","San Mateo","Santa Clara","Solano","Sonoma",
                "Santa Cruz","San Benito","Monterey","San Joaquin","Stanislaus","Merced","Yuba","Placer","El Dorado",
                "Sutter","Yolo","Sacramento","Lake","Mendocino")

# Bring in datasets

day            <- read_tsv(day_location,col_names=TRUE)
household      <- read_tsv(hh_location,col_names=TRUE)
location       <- read_tsv(location_location,col_names=TRUE)
person         <- read_tsv(person_location,col_names=TRUE)
trip           <- read_tsv(trip_location,col_names=TRUE)
linked_trip    <- read_tsv(trip_linked_location,col_names=TRUE)
trip_other     <- read_tsv(trip_w_other_location,col_names=TRUE)
vehicle        <- read_tsv(vehicle_location,col_names=TRUE)


# Make tracts call for 23-county region (Link 21+Lake and Mendocino counties), convert projection to 26910

bay_tracts <- tracts(
  state = "CA",
  county = megaregion,
  year = 2020
) %>% 
  st_transform(.,crs = 26910) %>% 
  select(GEOID,geometry)

# Day file has no location information

day_out <- day

rm(day)

# Household file needs geocoding

household_places <- household %>% 
  filter(!(is.na(reported_home_lat)),!(is.na(reported_home_lon))) %>% 
  st_as_sf(., coords = c("reported_home_lon", "reported_home_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(bay_tracts))

household_out <- st_join(household_places,bay_tracts, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry,-sample_home_lat,-sample_home_lon,-home_bg_geoid) %>% 
  relocate(home_tract_geoid=GEOID,.before=home_county_fips)

rm(household)

# Location file needs geocoding

location_places <- location %>% 
  filter(!(is.na(lat)),!(is.na(lon))) %>% 
  st_as_sf(., coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(bay_tracts))

location_out <- st_join(location_places,bay_tracts, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  rename(location_tract_geoid=GEOID)

rm(location)

# Person file needs geocoding for school and work locations

person_school <- person %>% 
  filter(!(is.na(school_lat)),!(is.na(school_lon))) %>% 
  st_as_sf(., coords = c("school_lon", "school_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(bay_tracts))

person_work <- person %>% 
  filter(!(is.na(work_lat)),!(is.na(work_lon))) %>% 
  st_as_sf(., coords = c("work_lon", "work_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(bay_tracts))

temp_person_school <- st_join(person_school,bay_tracts, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,hh_id,person_num,school_tract_geoid=GEOID)

temp_person_work <- st_join(person_work,bay_tracts, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,hh_id,person_num,work_tract_geoid=GEOID)

person_out <- person %>% 
  left_join(.,temp_person_school,by=c("person_id","hh_id","person_num")) %>% 
  left_join(.,temp_person_work,by=c("person_id","hh_id","person_num")) %>% 
  relocate(school_tract_geoid,.before = school_county_fips) %>% 
  relocate(work_tract_geoid,.before = work_county_fips) %>% 
  select(-school_bg_geo_id,-work_bg_geo_id,-school_lat,-school_lon,-work_lat,-work_lon)

rm(person)

# Trip file needs origin/destination recoded

trip_origin <- trip %>% 
  filter(!(is.na(o_lat)),!(is.na(o_lon))) %>% 
  st_as_sf(., coords = c("o_lon", "o_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(bay_tracts))

trip_destination <- trip %>% 
  filter(!(is.na(d_lat)),!(is.na(d_lon))) %>% 
  st_as_sf(., coords = c("d_lon", "d_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(bay_tracts))

temp_trip_origin <- st_join(trip_origin,bay_tracts, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,hh_id,person_num,origin_tract_geoid=GEOID)

temp_trip_destination <- st_join(trip_destination,bay_tracts, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,hh_id,person_num,destination_tract_geoid=GEOID)
------
trip_out <- trip %>% 
  left_join(.,temp_trip_origin,by=c("person_id","hh_id","person_num")) %>% 
  left_join(.,temp_person_work,by=c("person_id","hh_id","person_num")) %>% 
  relocate(school_tract_geoid,.before = school_county_fips) %>% 
  relocate(work_tract_geoid,.before = work_county_fips) %>% 
  select(-school_bg_geo_id,-work_bg_geo_id,-school_lat,-school_lon,-work_lat,-work_lon)

rm(person)
  
