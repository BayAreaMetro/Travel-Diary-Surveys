# TNC Survey Summaries for Transform.r
# Summarize mean trip length for auto using dataset distances and skim distances
# SUm number of transit boardings per day for transit riders

# Import Libraries

suppressMessages(library(tidyverse))
library(stats)

# Set up working directory

temp          <- "M:/Data/HomeInterview/TNC Survey/Data/Task 8 Data Refinement Version"
file_location <- file.path(temp,"Final Updated Dataset as of 10-18-2021","RSG_HTS_Oct2021_bayarea")
trip_location <- file.path(file_location,"trip_linked.tsv")

skim_location <- "Z:/Projects/2015_TM152_IPA_17/skims/skims_csv/HWYSKMAM.csv"

USERPROFILE   <- gsub("////","/", Sys.getenv("USERPROFILE"))
BOX_TM        <- file.path(USERPROFILE, "Box", "Modeling and Surveys", "Share Data")
Output        <- file.path(BOX_TM,"bespoke","Travel Diary Survey","Transform")


# Bring in data and skim matrix

trip            <- read_tsv(trip_location,col_names=TRUE)
skim            <- read.csv(skim_location) %>% 
  select(orig,dest,skim_distance=DISTDA)

# Join data on origin/destination for skims and summarize weighted mean distance for skim and dataset-provided distances

car <- left_join(trip,skim, by=c("o_taz"="orig","d_taz"="dest")) %>% 
  filter(!is.na(skim_distance)) %>%
  filter(mode_type==3) %>% 
  summarize(weighted_skim_mean=weighted.mean(skim_distance,daywt_alladult_wkday),
                                             weighted_RSG_mean=weighted.mean(distance,daywt_alladult_wkday))
    
# Output summary

write.csv(car,file.path(Output,"2019 Mean Weekday Driving Distances for the Bay Area.csv"), row.names = F)

# Now calculate boardings for transit passengers
# Tally trips within a single trip and then across a full day
# Calculate weighted mean for each day that a transit trip occurs

unlinked_location   <- file.path(file_location,"trip.tsv")
unlinked            <- read_tsv(unlinked_location,col_names=TRUE) 

day_location   <- file.path(file_location,"day.tsv")
day            <- read_tsv(day_location,col_names=TRUE) %>% 
  select(person_id,day_num,daywt_alladult_wkday)

boardings <- unlinked %>% 
  mutate(mode_1_transit=if_else(mode_1 %in% c(28,30,32,39,42,46,55,68,24),1,0),
         mode_2_transit=if_else(mode_2 %in% c(28,30,32,39,42,46,55,68,24),1,0),
         mode_3_transit=if_else(mode_3 %in% c(28,30,32,39,42,46,55,68,24),1,0),
         mode_4_transit=if_else(mode_4 %in% c(28,30,32,39,42,46,55,68,24),1,0),
         trip_tally=mode_1_transit + mode_2_transit + mode_3_transit + mode_4_transit
         ) %>% 
  group_by(person_id,day_num) %>% 
  summarize(daily_tally=sum(trip_tally)) %>% 
  ungroup() %>% 
  filter(daily_tally>0) %>% 
  left_join(.,day,by=c("person_id","day_num")) %>% 
  filter(!is.na(daywt_alladult_wkday>0)) %>% 
  summarize(mean_weekday_boardings=weighted.mean(daily_tally,daywt_alladult_wkday))

# Output summary

write.csv(boardings,file.path(Output,"2019 Mean Weekday Boardings for Transit Riders.csv"), row.names = F)


