# TNC Survey Facility Margin of Error Calculations.r
# Summarize TNC survey data for key variables and calculate margins of error

# Bring in libraries

suppressMessages(library(tidyverse))

# Set output directory

USERPROFILE   <- gsub("////","/", Sys.getenv("USERPROFILE"))
Box_TM1       <- file.path(USERPROFILE, "Box", "Modeling and Surveys", "Surveys", "Travel Diary Survey")
Box_TM2       <- file.path(Box_TM1,"MPO Partner Household Travel Survey","TNC Work","SFCTA Map Matching")
Output        <- file.path(Box_TM2,"NextGen Freeway Analysis")

# Bring in TNC survey files
# Commented out files that may be needed for future analyses

temp                  <- "M:/Data/HomeInterview/TNC Survey/Data/Task 8 Data Refinement Version"
file_location         <- file.path(temp,"Final Updated Dataset as of 10-18-2021","RSG_HTS_Oct2021_bayarea")
hh_location           <- file.path(file_location,"hh.tsv")
person_location       <- file.path(file_location,"person.tsv")
trip_linked_location  <- file.path(file_location,"trip_linked.tsv")
#day_location          <- file.path(file_location,"day.tsv")
#location_location     <- file.path(file_location,"location.tsv")
#trip_location         <- file.path(file_location,"trip.tsv")
#trip_w_other_location <- file.path(file_location,"trip_with_purpose_other.tsv")
#vehicle_location      <- file.path(file_location,"vehicle.tsv")

# Bring in datasets
# Coomented out files that may be needed for future analyses

household      <- read_tsv(hh_location,col_names=TRUE)
person         <- read_tsv(person_location,col_names=TRUE)
linked_trip    <- read_tsv(trip_linked_location,col_names=TRUE)
#day            <- read_tsv(day_location,col_names=TRUE)
#location       <- read_tsv(location_location,col_names=TRUE)
#trip           <- read_tsv(trip_location,col_names=TRUE)
#trip_other     <- read_tsv(trip_w_other_location,col_names=TRUE)
#vehicle        <- read_tsv(vehicle_location,col_names=TRUE)

# Join linked trip file

vehicle_out <- vehicle 

# Output recoded files

write.csv(day_out,file.path(Output,"BATS_2019_Day.csv"),row.names = FALSE)
write.csv(household_out,file.path(Output,"BATS_2019_Household.csv"),row.names = FALSE)
write.csv(location_out,file.path(Output,"BATS_2019_Location.csv"),row.names = FALSE)
write.csv(person_out,file.path(Output,"BATS_2019_Person.csv"),row.names = FALSE)
write.csv(trip_out,file.path(Output,"BATS_2019_Trip.csv"),row.names = FALSE)
write.csv(linked_trip_out,file.path(Output,"BATS_2019_Linked_Trip.csv"),row.names = FALSE)
write.csv(trip_other_out,file.path(Output,"BATS_2019_Trip_Purpose_Other.csv"),row.names = FALSE)
write.csv(vehicle_out,file.path(Output,"BATS_2019_Vehicle.csv"),row.names = FALSE)

