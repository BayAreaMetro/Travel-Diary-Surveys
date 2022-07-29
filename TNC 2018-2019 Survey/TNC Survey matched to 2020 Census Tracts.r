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
  st_transform(.,crs = 26910)

# Day file has no location information
# Household file needs adjustments, but can sub tract for existing block group data

household_out <- houshold %>% 
  mutate(home_tract_geoid = substr(home_bg_geoid,1,9))
