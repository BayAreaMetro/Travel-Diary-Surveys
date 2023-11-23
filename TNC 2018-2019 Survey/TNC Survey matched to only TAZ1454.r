# TNC Survey matched to only TAZ1454.r
# Remove point level data and any small-level census geo, keep TAZs

# Set output directory

USERPROFILE          <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
BOX_TM               <- file.path(USERPROFILE, "Box", "Modeling and Surveys")
Output               <- file.path(BOX_TM,"Share Data/Protected Data/Jason Hawkins")

# Bring in libraries

suppressMessages(library(tidyverse))

# Set up file locations

temp                  <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations"
file_location         <- file.path(temp,"Final Updated Dataset as of 10-18-2021/RSG_HTS_Oct2021_bayarea")
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


# Day file has no location information

day_out <- day

# Household file 

household_out <- household %>%
  select(-grep("_lat|_geo|_lon",names(.)))

# Person file

person_out <- person %>% 
  select(-grep("_lat|_geo|_lon",names(.)))

# Trip file 

trip_out <- trip %>% 
  select(-grep("_lat|_geo|_lon",names(.)))

# Linked trip file needs origin/destination recoded

linked_trip_out <- linked_trip %>% 
  select(-grep("_lat|_geo|_lon",names(.)))

# Nothing recoded for vehicle file

vehicle_out <- vehicle 

# Output recoded files

write.csv(day_out,file.path(Output,"BATS_2019_Day.csv"),row.names = FALSE)
write.csv(household_out,file.path(Output,"BATS_2019_Household.csv"),row.names = FALSE)
write.csv(person_out,file.path(Output,"BATS_2019_Person.csv"),row.names = FALSE)
write.csv(trip_out,file.path(Output,"BATS_2019_Trip.csv"),row.names = FALSE)
write.csv(linked_trip_out,file.path(Output,"BATS_2019_Linked_Trip.csv"),row.names = FALSE)
write.csv(vehicle_out,file.path(Output,"BATS_2019_Vehicle.csv"),row.names = FALSE)

