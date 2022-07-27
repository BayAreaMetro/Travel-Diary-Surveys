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

# Merge household income file, recode income in groups

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

# Join with income and remove missing data

working <- left_join(trip,hh_income, by="hh_id") %>% 
  filter(hh_income!="6_Prefer not to answer")

# Summarize trips by income category and hh/person
# Join person weights
# Daily trips is weighted trips/person weight
# Create daily and monthly trip variables

trip_persons <- working %>% 
  group_by(hh_id,person_id,hh_income) %>% 
  summarize(num_trips=sum(daywt_alladult_7day)) %>% 
  ungroup()

person_join <- person %>% 
  select(hh_id,person_id,wt_alladult_7day)

trip_person_joined <- left_join(trip_persons,person_join,by=c("hh_id","person_id")) %>% 
  mutate(daily_trips=if_else(num_trips==0,0,num_trips/wt_alladult_7day))

final <- trip_person_joined %>% 
  group_by(hh_income) %>% 
  summarize(median_person_daily=weighted.median(daily_trips,wt_alladult_7day),
            median_person_monthly=median_person_daily*365/12)

View(final)
  

