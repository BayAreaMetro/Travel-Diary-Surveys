# TNC Survey matched to 2010 Census Tracts.r
# Remove point level data and match 2010 Census geographies. Retain TAZs for TM1 too.

# Set output directory

Output        <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations/NDA Sharing Versions/Census Tract Data Version 2010"

# Bring in libraries

library(tigris)
suppressMessages(library(tidyverse))
library(sf)

# Set up working directory

temp                  <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations"
file_location         <- file.path(temp,"Final Updated Dataset as of 10-18-2021")
day_location          <- file.path(file_location,"day.tsv")
hh_location           <- file.path(file_location,"hh.tsv")
person_location       <- file.path(file_location,"person.tsv")
trip_location         <- file.path(file_location,"trip.tsv")
trip_linked_location  <- file.path(file_location,"trip_linked.tsv")
vehicle_location      <- file.path(file_location,"vehicle.tsv")

# Set up vector of counties for Bay Area and beyond

megaregion <- c("Alameda","Contra Costa","Marin","Napa","San Francisco","San Mateo","Santa Clara","Solano","Sonoma",
                "Santa Cruz","San Benito","Monterey","San Joaquin","Stanislaus","Merced","Yuba","Placer","El Dorado",
                "Sutter","Yolo","Sacramento","Lake","Mendocino")

# Bring in datasets

day            <- read_tsv(day_location,col_names=TRUE)
household      <- read_tsv(hh_location,col_names=TRUE)
person         <- read_tsv(person_location,col_names=TRUE)
trip           <- read_tsv(trip_location,col_names=TRUE)
linked_trip    <- read_tsv(trip_linked_location,col_names=TRUE)
vehicle        <- read_tsv(vehicle_location,col_names=TRUE)


# Make tracts call for 23-county region (Link 21+Lake and Mendocino counties)
# Convert projection to NAD83 / UTM zone 10N (ESPG 26910)

bay_tracts <- tracts(
  state = "CA",
  county = megaregion,
  year = 2010
) %>% 
  st_transform(.,crs = 26910) %>% 
  select(GEOID10,geometry)

# Where geocoding is necessary, assigned CRS=4326, World Geodetic System, then convert to 26910, spatially match
# Append census tract locations, relocate variables to be near similar geolocation variables

# Day file has no location information

day_out <- day

# Household file needs geocoding

household_places <- household %>% 
  filter(!(is.na(reported_home_lat)),!(is.na(reported_home_lon))) %>% 
  st_as_sf(., coords = c("reported_home_lon", "reported_home_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(bay_tracts))

household_out <- st_join(household_places,bay_tracts, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry,-sample_home_lat,-sample_home_lon,-home_bg_geoid) %>% 
  relocate(home_tract_geoid=GEOID10,.before=home_county_fips)

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
  select(person_id,hh_id,person_num,school_tract_geoid=GEOID10)

temp_person_work <- st_join(person_work,bay_tracts, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,hh_id,person_num,work_tract_geoid=GEOID10)

person_out <- person %>% 
  left_join(.,temp_person_school,by=c("person_id","hh_id","person_num")) %>% 
  left_join(.,temp_person_work,by=c("person_id","hh_id","person_num")) %>% 
  relocate(school_tract_geoid,.before = school_county_fips) %>% 
  relocate(work_tract_geoid,.before = work_county_fips) %>% 
  select(-school_bg_geo_id,-work_bg_geo_id,-school_lat,-school_lon,-work_lat,-work_lon)

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
  select(person_id,day_num, hh_id,person_num,trip_id,trip_num,linked_trip_id,leg_num,origin_tract_geoid=GEOID10)

temp_trip_destination <- st_join(trip_destination,bay_tracts, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,day_num, hh_id,person_num,trip_id,trip_num,linked_trip_id,leg_num,destination_tract_geoid=GEOID10)

trip_out <- trip %>% 
  left_join(.,temp_trip_origin,by=c("person_id","day_num","hh_id","person_num","trip_id","trip_num",
                                    "linked_trip_id","leg_num")) %>% 
  left_join(.,temp_trip_destination,by=c("person_id","day_num","hh_id","person_num","trip_id","trip_num",
                                         "linked_trip_id","leg_num")) %>% 
  relocate(origin_tract_geoid,.before = o_county_fips) %>% 
  relocate(destination_tract_geoid,.before = d_county_fips) %>% 
  select(-o_bg_geo_id,-d_bg_geo_id,-o_lat,-o_lon,-d_lat,-d_lon)

# Linked trip file needs origin/destination recoded

linked_trip_origin <- linked_trip %>% 
  filter(!(is.na(o_lat)),!(is.na(o_lon))) %>% 
  st_as_sf(., coords = c("o_lon", "o_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(bay_tracts))

linked_trip_destination <- linked_trip %>% 
  filter(!(is.na(d_lat)),!(is.na(d_lon))) %>% 
  st_as_sf(., coords = c("d_lon", "d_lat"), crs = 4326) %>% 
  st_transform(., crs=st_crs(bay_tracts))

temp_linked_trip_origin <- st_join(linked_trip_origin,bay_tracts, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,day_num, hh_id,person_num,linked_trip_id,origin_tract_geoid=GEOID10)

temp_linked_trip_destination <- st_join(linked_trip_destination,bay_tracts, join=st_within,left=TRUE)%>%
  as.data.frame(.) %>% select(-geometry) %>% 
  select(person_id,day_num, hh_id,person_num,linked_trip_id,destination_tract_geoid=GEOID10)

linked_trip_out <- linked_trip %>% 
  left_join(.,temp_linked_trip_origin,by=c("person_id","day_num","hh_id","person_num",
                                    "linked_trip_id")) %>% 
  left_join(.,temp_linked_trip_destination,by=c("person_id","day_num","hh_id","person_num",
                                         "linked_trip_id")) %>% 
  relocate(origin_tract_geoid,.before = o_county_fips) %>% 
  relocate(destination_tract_geoid,.before = d_county_fips) %>% 
  select(-o_bg_geo_id,-d_bg_geo_id,-o_lat,-o_lon,-d_lat,-d_lon)

# Nothing recoded for vehicle file

vehicle_out <- vehicle 

# Output recoded files

write.csv(day_out,file.path(Output,"BATS_2019_Day_2010_Tract.csv"),row.names = FALSE)
write.csv(household_out,file.path(Output,"BATS_2019_Household_2010_Tract.csv"),row.names = FALSE)
write.csv(person_out,file.path(Output,"BATS_2019_Person_2010_Tract.csv"),row.names = FALSE)
write.csv(trip_out,file.path(Output,"BATS_2019_Trip_2010_Tract.csv"),row.names = FALSE)
write.csv(linked_trip_out,file.path(Output,"BATS_2019_Linked_Trip_2010_Tract.csv"),row.names = FALSE)
write.csv(vehicle_out,file.path(Output,"BATS_2019_Vehicle_2010_Tract.csv"),row.names = FALSE)

