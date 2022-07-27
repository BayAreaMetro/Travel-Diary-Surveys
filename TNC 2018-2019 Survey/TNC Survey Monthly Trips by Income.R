# TNC Survey Monthly Trips by Income.R
# Calculate trips by income using weeklong weight and then extrapolating to month

# Import Libraries

suppressMessages(library(tidyverse))
library(spatstat)

# Set up working directory

temp          <- "M:/Data/HomeInterview/TNC Survey/Data/Task 8 Data Refinement Version"
file_location <- file.path(temp,"Final Updated Dataset as of 10-18-2021","RSG_HTS_Oct2021_bayarea")
trip_location <- file.path(file_location,"trip_linked.tsv")
hh_location   <- file.path(file_location,"hh.tsv")
person_location   <- file.path(file_location,"person.tsv")

USERPROFILE   <- gsub("////","/", Sys.getenv("USERPROFILE"))
BOX_TM        <- file.path(USERPROFILE, "Box", "Modeling and Surveys", "Surveys")
Output        <- file.path(BOX_TM,"Travel Diary Survey","Requests","Danielle Dai")

# Bring in data

trip       <- read_tsv(trip_location,col_names=TRUE) 
household  <- read_tsv(hh_location,col_names=TRUE) 
person  <- read_tsv(person_location,col_names=TRUE) 

# Merge household income file, recode income

hh_income <- household %>%
  select(hh_id,income_detailed)%>% 
  mutate(hh_income=case_when(
    income_detailed %in% c(1,2,3,4) ~ "1_Under $50,000",
    income_detailed %in% c(5,6)     ~ "2_$50,000-$99,999",
    income_detailed==7              ~ "3_$100,000-$149,999",
    income_detailed==8              ~ "4_$150,000-$199,999",
    income_detailed %in% c(9,10)    ~ "5_$200,000 or more",
    income_detailed==999            ~ "6_Prefer not to answer",
    TRUE                            ~ "7_Miscoded")) 

working <- left_join(trip,hh_income, by="hh_id") %>% 
  filter(hh_income!="6_Prefer not to answer")

# Number of persons within each income category
# Start with trip list and get a list of unique people

trip_persons <- working %>% 
  group_by(hh_id,person_id) %>% 
  summarize(num_trips=sum(daywt_alladult_7day)) %>% 
  select(-num_trips) %>% 
  ungroup()

person_join <- person %>% 
  select(hh_id,person_id,wt_alladult_7day)

trip_person_joined <- left_join(trip_persons,person_join,by=c("hh_id","person_id")) %>% 
  left_join(.,hh_income,by="hh_id")

person_income_summary <- trip_person_joined %>% 
  group_by(hh_income) %>% 
  summarize(total_persons=sum(wt_alladult_7day))
  
# Total mean person trips per day by income
# Multiply by 365 and divide by 12 to get monthly trips

mean_trips <- working %>%
  group_by(hh_income) %>% 
  summarize(total_trips=sum(daywt_alladult_7day)) %>% 
  left_join(.,person_income_summary,by="hh_income") %>% 
  mutate(trips_per_person_daily=total_trips/total_persons) %>% 
  mutate(mean_monthly_trips=trips_per_person_daily*365/12)


median_trips <- working %>%
  group_by(hh_income) %>% 
  summarize(median_daily_trips=median(daywt_alladult_7day,na.rm = TRUE)) %>% 
  filter(hh_income !="6_Prefer not to answer") %>% 
  mutate(median_monthly_trips=median_daily_trips*365/12)


# Write out TNC-only trip file

write.csv(tnc_only, "TNC_Pilot_Trips.csv", row.names = FALSE, quote = T)
