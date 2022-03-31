# TNC Data Import and Conversion 022720.R
# Read in raw data and write out R versions
# SI

# Import Libraries

suppressMessages(library(tidyverse))

# Set up working directory

wd <- "M:/Data/HomeInterview/TNC Survey/Data/Final Version/Raw/TSV/"
setwd(wd)

# Bring in data

TNC_Trip 	    <- paste0(wd,"ex_trip.tsv")
TNC_Trip_Other<- paste0(wd,"ex_trip_with_purpose_other.tsv")
TNC_Person 	  <- paste0(wd,"ex_person.tsv")
TNC_Location 	<- paste0(wd,"ex_location.tsv")
TNC_Household	<- paste0(wd,"ex_hh.tsv")
TNC_Vehicle   <- paste0(wd,"ex_vehicle.tsv")
TNC_Day 	    <- paste0(wd,"ex_day.tsv")

trip       <- read_tsv(TNC_Trip,col_names=TRUE)
trip_other <- read_tsv(TNC_Trip_Other,col_names=TRUE)
person     <- read_tsv(TNC_Person,col_names=TRUE) 
location   <- read_tsv(TNC_Location,col_names=TRUE) 
household  <- read_tsv(TNC_Household,col_names=TRUE) 
vehicle    <- read_tsv(TNC_Vehicle,col_names=TRUE) 
day        <- read_tsv(TNC_Day,col_names=TRUE) 

# Save out R versions

save(trip, file = paste0(wd,"../R/trip.rdata"))
save(trip_other, file = paste0(wd,"../R/trip_other.rdata"))
save(person, file = paste0(wd,"../R/person.rdata"))
save(location, file = paste0(wd,"../R/location.rdata"))
save(household, file = paste0(wd,"../R/household.rdata"))
save(vehicle, file = paste0(wd,"../R/vehicle.rdata"))
save(day, file = paste0(wd,"../R/day.rdata"))
