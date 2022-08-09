# TNC Survey Facility Margin of Error Calculations.r
# Summarize TNC survey data for key variables and calculate margins of error

# Set options to get rid of scientific noation

options(scipen = 999)

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
  ) %>% 
  select(hh_id,person_id,income_recoded,race_recoded)

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
# Filter for facility value==1 (i.e., traverses that facility) 
# Formula for SE of a weighted sample: 
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Standard_error_of_a_proportion_estimation_when_using_weighted_data
# Takes the form: sqrt(p(1-p)summation((weights standardized to 1)^2))
# Do separate summaries for race, trip purpose, and income, then concatenate and do additional calculations

# Start with input dataset based on desired time of day (df_tod), using depart_hour variable

all_day <- working                                 # all times of day
peak <- working %>%                                # morning and evening peak
  filter(depart_hour %in% c(6,7,8,9,15,16,17,18))
am_peak <- working %>%                             # morning peak
  filter(depart_hour %in% c(6,7,8,9))
pm_peak <- working %>%                             # evening peak
  filter(depart_hour %in% c(15,16,17,18))

# Now create function

calculations <- function(df_tod,facility){
  temp_output <- data.frame()
  temp_df <- df_tod %>% 
    filter(.[[facility]]==1) %>% 
    mutate(squared_standard_weights=(daywt_alladult_wkday/sum(daywt_alladult_wkday))^2)
  
# Store value of summed squared standardized weights in a variable for later use  
  error_summation <- sum(temp_df$squared_standard_weights)
  
# Store total trips for calculating shares within each summary
  total_trips <- sum(temp_df$daywt_alladult_wkday)
  
# Summarize data by race/ethnicity

  race <- temp_df %>% 
    group_by(race_recoded) %>% 
    summarize(share_value=sum(daywt_alladult_wkday)/total_trips) %>% 
    mutate(category="ethnicity") %>% 
    rename(metric=race_recoded) %>% 
    ungroup()
  
  purpose <- temp_df %>% 
    group_by(purpose_recoded) %>% 
    summarize(share_value=sum(daywt_alladult_wkday)/total_trips) %>% 
    mutate(category="trip_purpose") %>% 
    rename(metric=purpose_recoded) %>% 
    ungroup()
  
  income <- temp_df %>% 
    group_by(income_recoded) %>% 
    summarize(share_value=sum(daywt_alladult_wkday)/total_trips) %>% 
    mutate(category="income") %>% 
    rename(metric=income_recoded) %>% 
    ungroup()
  
# Calculate standard error, 90 percent confidence interval, lower and upper bound values
  
  temp_output <- bind_rows(temp_output,race,purpose,income) %>% 
    mutate(roadway=facility,
           standard_error=sqrt((share_value*(1-share_value)*error_summation)),
           ci_90=1.645*standard_error,
           lower_bound=if_else(share_value-ci_90>=0,share_value-ci_90,0),
           upper_bound=share_value+ci_90) %>% 
    relocate(roadway,.before = metric) %>% 
    relocate(category,.after = roadway)
  
  return(temp_output)
}

trial <- calculations(all_day,"Al_SF_80_PlazaTo101")
trial2 <- calculations(peak,"Al_SF_80_PlazaTo101")

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

temp_df <- working %>% 
  filter(Al_SF_80_PlazaTo101==1 & daywt_alladult_wkday>0) %>% 
  mutate(squared_standard_weights=(daywt_alladult_wkday/sum(daywt_alladult_wkday))^2)
error_summation <- sum(temp_df$squared_standard_weights)


