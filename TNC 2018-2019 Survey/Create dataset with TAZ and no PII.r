# Create dataset with TAZ and no PII.r
# Remove point level data and retain TAZs

# Set output directory

Output        <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations/NDA Sharing Versions/TAZ Data Version"

# Bring in libraries

suppressMessages(library(tidyverse))

# Set up working directory

temp                  <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations"
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


# Output files

# Day file has no location information

day_out <- day

# Household file needs removal of lat/long for home and sample locations, block groups

household_out <- household %>% 
  select(-reported_home_lat,-reported_home_lon,-sample_home_lat,-sample_home_lon,-home_bg_geoid)

# Person file needs removal of school and work discrete and small area data

person_out <- person %>% 
  select(-school_lat,-school_lon, -work_lat,-work_lon,-school_bg_geo_id,-work_bg_geo_id)

# Trip file needs origin/destination discrete locations removed

trip_out <- trip %>% 
  select(-o_bg_geo_id,-d_bg_geo_id,-o_lat,-o_lon,-d_lat,-d_lon)


# Linked trip file needs origin/destination discrete locations removed

linked_trip_out <- linked_trip %>% 
  select(-o_bg_geo_id,-d_bg_geo_id,-o_lat,-o_lon,-d_lat,-d_lon)

# Nothing recoded for vehicle file

vehicle_out <- vehicle 

# Output recoded files

write.csv(day_out,file.path(Output,"BATS_2019_Day.csv"),row.names = FALSE)
write.csv(household_out,file.path(Output,"BATS_2019_Household.csv"),row.names = FALSE)
write.csv(person_out,file.path(Output,"BATS_2019_Person.csv"),row.names = FALSE)
write.csv(trip_out,file.path(Output,"BATS_2019_Trip.csv"),row.names = FALSE)
write.csv(linked_trip_out,file.path(Output,"BATS_2019_Linked_Trip.csv"),row.names = FALSE)
write.csv(vehicle_out,file.path(Output,"BATS_2019_Vehicle.csv"),row.names = FALSE)

