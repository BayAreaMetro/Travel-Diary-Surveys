# TNC_Survey_Summarize_Tours_Per_Worker.r
# Summarize TNC survey data tours by worker by three categories:
# Workers who go to work, workers who telework, and workers who don't work on day

# Set options to get rid of scientific noation

options(scipen = 999)

# Bring in libraries

suppressMessages(library(tidyverse))

# Set output directory

USERPROFILE   <- gsub("////","/", Sys.getenv("USERPROFILE"))
Box_TM1       <- file.path(USERPROFILE, "Box", "Modeling and Surveys", "Surveys", "Travel Diary Survey")
Box_TM2       <- file.path(Box_TM1,"MPO Partner Household Travel Survey","Bay Area Travel Study 2018-2019","SFCTA Map Matching")
Output        <- file.path(Box_TM2,"Facility Summaries")
OSM_Path      <- "M:/Data/HomeInterview/TNC Survey/SFCTA Map Matching"

# TNC survey files locations

temp1                  <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019"
temp2                  <- file.path(temp1,"Data/Final Version No Imputations")
output                 <- file.path(temp1,"MTC Summaries")
file_location          <- file.path(temp2,"R")
person_location        <- file.path(file_location,"person.rdata")
trip_location          <- file.path(file_location,"trip.rdata")
hh_location            <- file.path(file_location,"household.rdata")
day_location           <- file.path(file_location,"day.rdata")

# Bring in TNC survey datasets
# Commenting out some files that might be useful for additional analyses

load(person_location) 
person <- person %>%
  mutate(is_active_participant=as.numeric(is_active_participant)) %>% 
  filter(is_active_participant==1) %>% 
  select(hh_id,person_num,num_jobs,employment,worker,job_type,hours_work, telework_freq,wt_alladult_wkday) 
  
#trip            <- load(trip_location)      
#household       <- load(household_location) 
load(day_location) 
 day <- day %>% 
  select(hh_id,person_num,travel_date_dow, no_travel_telework,telework_time,missing_telework_time, daywt_alladult_wkday)

# Tour file locations from SFCTA 
# x suffix denotes DaySim files (different variable names)

tourx_folder     <- file.path(temp1, "Data", "Final Version No Imputations","SFCTA Tour Files","2_tour_extract", "wt_wkday")
tourx_location   <- file.path(tourx_folder,"survey2018_tourx.rdata")
tripx_location   <- file.path(tourx_folder,"survey2018_tripx.rdata")
personx_location <- file.path(tourx_folder,"survey2018_precx.rdata")
hhx_location     <- file.path(tourx_folder,"survey2018_hrecx.rdata")
dayx_location    <- file.path(tourx_folder,"survey2018_pdayx.rdata")

# Bring in tour files
# Commenting out some files that might be useful for additional analyses

#load(tourx_location)
#load(tripx_location)
#load(personx_location)
#load(hhx_location)
load(dayx_location)

# Join employment and telework variables to the tour day file
# Remove Friday-Sunday records
# Set up rules for workers outside home, teleworkers, and workers not working on the day

joined <- left_join(SFCTA_day,day,by=c("hhno"="hh_id","pno"="person_num","day"="travel_date_dow")) %>% 
  filter(day<5) %>% 
  left_join(.,person,by=c("hhno"="hh_id","pno"="person_num")) %>% 
  mutate(
    work_category=case_when(
      wktours>=1                                                         ~"1_worked outside home",
      wktours==0 & worker==1 & telework_time>0                           ~"2_teleworked",
      wktours==0 & worker==1 & (telework_time==0 |is.na(telework_time))  ~"3_worker but did not work",
      TRUE                                                               ~"4_not a worker"
    )
  )

# Weighted average of tours by worker category

work_cat <- joined %>% 
  group_by(work_category) %>% 
  summarize(weighted_average_home_based_tours=weighted.mean(hbtours,pdexpfac))

total_pop <- joined %>% 
  summarize(work_category="5_total_population",weighted_average_home_based_tours=weighted.mean(hbtours,pdexpfac))

final <- rbind(work_cat,total_pop)

