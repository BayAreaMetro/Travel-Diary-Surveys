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

temp                   <- "M:/Data/HomeInterview/TNC Survey/Data/Task 8 Data Refinement Version"
file_location          <- file.path(temp,"Final Updated Dataset as of 10-18-2021","RSG_HTS_Oct2021_bayarea")
person_location        <- file.path(file_location,"person.tsv")
trip_location          <- file.path(file_location,"trip.tsv")
#trip_linked_location  <- file.path(file_location,"trip_linked.tsv")
#hh_location           <- file.path(file_location,"hh.tsv")
#day_location          <- file.path(file_location,"day.tsv")
#location_location     <- file.path(file_location,"location.tsv")
#trip_w_other_location <- file.path(file_location,"trip_with_purpose_other.tsv")
#vehicle_location      <- file.path(file_location,"vehicle.tsv")

# Bring in TNC survey datasets
# Commented out files that may be needed for future analyses

person          <- read_tsv(person_location,col_names=TRUE)
trip            <- read_tsv(trip_location,col_names=TRUE)      
#linked_trip    <- read_tsv(trip_linked_location,col_names=TRUE)
#household      <- read_tsv(hh_location,col_names=TRUE)
#day            <- read_tsv(day_location,col_names=TRUE)
#location       <- read_tsv(location_location,col_names=TRUE)
#trip_other     <- read_tsv(trip_w_other_location,col_names=TRUE)
#vehicle        <- read_tsv(vehicle_location,col_names=TRUE)

# Bring in facility flag file (file that indicates whether a given trip traverses a given freeway)

facility_flag <- read.csv(file = file.path(Output,"TNC Survey Trips Per Facility.csv"))

# Recode linked trip file using imputed HH income and race/ethnicity from person file

person_joiner <- person %>% 
  filter(is_active_participant==1) %>%                   # Only include participants
  mutate(
    income_recoded=case_when(
      income_imputed %in% c(1,2)                         ~ "Under $50,000",
      income_imputed %in% c(3,4)                         ~ "$50,000-$99,999",
      income_imputed==5                                  ~ "$100,000-$149,999",
      income_imputed %in% c(6,7,8)                       ~ "Over $150,000",
      TRUE                                               ~ "Miscoded"
    ),
    race_recoded=case_when(
      raceeth_new_imputed==1                             ~ "Hispanic",
      raceeth_new_imputed==2                             ~ "Black",
      raceeth_new_imputed==3                             ~ "Asian/Pacific Islander",
      raceeth_new_imputed==4                             ~ "White",
      raceeth_new_imputed %in% c(-1,5)                   ~ "Other",
      TRUE                                               ~ "Miscoded"
    )
  )

# Recoded trip purpose on linked trip file

recoded_trip <- trip %>% 
  mutate(
    purpose_recoded=case_when(
      d_purpose_category_imputed==1                      ~ "Home",
      d_purpose_category_imputed %in% c(2,3)             ~ "Work or work-related",
      d_purpose_category_imputed %in% c(4,14)            ~ "School or school-related",
      d_purpose_category_imputed==5                      ~ "Escort",
      d_purpose_category_imputed==6                      ~ "Shop",
      d_purpose_category_imputed==7                      ~ "Meal",
      d_purpose_category_imputed==8                      ~ "Social/recreation",
      d_purpose_category_imputed==9                      ~ "Errand/appointment",
      d_purpose_category_imputed==10                     ~ "Change mode",
      d_purpose_category_imputed==11                     ~ "Spent the night elsewhere",
      d_purpose_category_imputed %in% c(-1,12)           ~ "Other/missing",
      TRUE                                               ~ "Miscoded"
    )
  )

# Join trips file with facility flag file 

working <- left_join(facility_flag,recoded_trip,by=c("hh_id","person_id","trip_id")) %>% 
  left_join(.,person_joiner,by=c("hh_id","person_id"))

# Function to analyze data and calculate standard errors

calculation <- function(facility){
  temp <- working %>% 
    filter(temp[[facility]]==1) 
  return(temp)
}

trial <- calculation(facility="Al_SF_80_PlazaTo101")

# Output recoded files

write.csv(day_out,file.path(Output,"BATS_2019_Day.csv"),row.names = FALSE)
write.csv(household_out,file.path(Output,"BATS_2019_Household.csv"),row.names = FALSE)
write.csv(location_out,file.path(Output,"BATS_2019_Location.csv"),row.names = FALSE)
write.csv(person_out,file.path(Output,"BATS_2019_Person.csv"),row.names = FALSE)
write.csv(trip_out,file.path(Output,"BATS_2019_Trip.csv"),row.names = FALSE)
write.csv(linked_trip_out,file.path(Output,"BATS_2019_Linked_Trip.csv"),row.names = FALSE)
write.csv(trip_other_out,file.path(Output,"BATS_2019_Trip_Purpose_Other.csv"),row.names = FALSE)
write.csv(vehicle_out,file.path(Output,"BATS_2019_Vehicle.csv"),row.names = FALSE)

-------
  
  # Join OSM and TNC Survey Paths Files and Create Facility Flags.R
  # Compile all the freeway files and create flags for links on each facility
  # Get rid of scientific notation, bring in library
  
  options(scipen = 999)

library(tidyverse)

# Input segment directory

dir1        <- "M:/Data/HomeInterview/TNC Survey/SFCTA Map Matching/NextGen Freeway Project"
segment_in  <- file.path(dir1,"TNC_Survey_OSM_Network")

# Bring in TNC Survey paths file

paths_in <- "M:/Data/HomeInterview/TNC Survey/SFCTA Map Matching/TNC_Survey_Paths.RData"
paths <- load(paths_in)


