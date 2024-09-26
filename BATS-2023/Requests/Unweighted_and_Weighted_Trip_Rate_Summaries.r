# Unweighted_and_Weighted_Trip_Rate_Summaries.r
# Summarize weighted and unweighted trip rates for persons and households

# Set options to get rid of scientific notation

options(scipen = 999)

# Bring in libraries

suppressMessages(library(tidyverse))
library(stats)

# Set file directories for input and output

USERPROFILE    <- gsub("////","/", Sys.getenv("USERPROFILE"))
BOX_dir1       <- file.path(USERPROFILE, "Box", "Modeling and Surveys","Surveys","Travel Diary Survey")
Box_dir2       <- file.path(BOX_dir1,"Biennial Travel Diary Survey","Data","2023")
conflation_loc <- file.path(Box_dir2,"Survey Conflation")
data_loc       <- file.path(Box_dir2,"Full Weighted 2023 Dataset","WeightedDataset_09112024")
output         <- file.path(Box_dir2,"Summaries")

# Bring in BATS 2023 survey files
  
trip           <- read.csv(file=file.path(data_loc,"trip.csv")) 
person         <- read.csv(file=file.path(data_loc,"person.csv")) %>% select(hh_id,person_id,person_weight)
hh             <- read.csv(file=file.path(data_loc,"hh.csv")) %>% select(hh_id,hh_weight)

# Person and household weights filtered to non-zero

person_non <- person %>% 
  filter(person_weight>0)

trips <- trip %>% 
  filter(trip_weight>0)

# Average trips per day summed at the person level (including 0-trip days)
# Join HH ID for hh-level summing too

person_sum <- trips %>% 
  group_by(person_id, day_num) %>% 
  summarize(unweighted_trips=n(),weighted_trips=sum(trip_weight)) %>% 
  ungroup() %>% 
  group_by(person_id) %>% 
  summarize(days=n(),unweighted_trips=sum(unweighted_trips),weighted_trips=sum(weighted_trips)) %>% 
  mutate(unweighted_trips_per_day=unweighted_trips/days) %>% 
  left_join(person_non,.,by="person_id") %>% 
  left_join(.,hh,by="hh_id") %>% 
  mutate_at(vars("unweighted_trips":"unweighted_trips_per_day"),~if_else(is.na(.),0,.))

# Calculate average unweighted and weighed trip rates per person and per household

person_calcs <- person_sum %>% 
  summarize(unweighted_mean=mean(unweighted_trips_per_day),weighted_mean=weighted.mean(unweighted_trips_per_day,person_weight))

hh_calcs <- person_sum %>% 
  group_by(hh_id) %>% 
  summarize(unweighted_trips_per_day=sum(unweighted_trips_per_day),weighted_trips_per_day=sum(weighted_trips)) %>% 
  ungroup() %>% 
  left_join(.,hh, by="hh_id") %>% 
  summarize(unweighted_mean=mean(unweighted_trips_per_day),weighted_mean=weighted.mean(unweighted_trips_per_day,hh_weight))

print(person_calcs)
print(hh_calcs)
  







