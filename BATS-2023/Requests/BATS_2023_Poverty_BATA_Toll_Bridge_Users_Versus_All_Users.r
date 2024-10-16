# BATS_2023_Poverty_BATA_Toll_Bridge_Users_Versus_All_Users.r
# Summarize modes used by BATA bridge users in poverty vs. all people

# Set options to get rid of scientific notation

options(scipen = 999)

# Bring in libraries

suppressMessages(library(tidyverse))

# Set file directories for input and output

USERPROFILE    <- gsub("////","/", Sys.getenv("USERPROFILE"))
BOX_dir1       <- file.path(USERPROFILE, "Box", "Modeling and Surveys","Surveys","Travel Diary Survey")
Box_dir2       <- file.path(BOX_dir1,"Biennial Travel Diary Survey","Data","2023")
conflation_loc <- file.path(Box_dir2,"Survey Conflation")
data_loc       <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset/WeightedDataset_09112024"
output         <- file.path(data_loc,"Summaries")

# Bring in BATA bridge trips from this process: 
# https://github.com/BayAreaMetro/Travel-Diary-Surveys/blob/master/BATS-2023/conflation-trip-summaries/BATS_2023_Survey_Facility_Margin_of_Error_Calculations_All_Facilities.r

bata_trips <- read.csv("M:/Data/Requests/Rebecca Long/BATA Bridge Use Versus All Residents/BATA_Bridge_Trips.csv")

poverty_bata <- bata_trips %>% 
  filter(poverty_status=="under_2x_poverty") %>% 
  mutate(dummy=1) %>% 
  group_by(hh_id) %>% 
  summarize(total=sum(dummy)) %>% 
  ungroup() %>% 
  mutate(bata_use=if_else(total>=1,1,0)) %>% 
  select(-total)

# Bring in BATS 2023 trip file and append poverty BATS households
 
trip <- read.csv(file=file.path(data_loc, "trip.csv")) %>% 
  left_join(., poverty_bata, by = "hh_id") %>% 
  mutate(bata_use = ifelse(is.na(bata_use), 0, bata_use),
         bata_use_alpha = ifelse(bata_use==1,"bata_poverty","not_bata_poverty"))
  
# Summarize mode type for Bay Area BATA_poverty

bay_bata_poverty <- trip %>% 
  filter(bata_use_alpha=="bata_poverty") %>% 
  group_by(mode_type) %>% 
  summarize(total=sum(trip_weight_rmove_only)) %>% 
  ungroup()

# Summarize mode type for Bay Area generally

bay_all <- trip %>% 
  group_by(mode_type) %>% 
  summarize(total=sum(trip_weight_rmove_only))

# Output file to CSV

write.csv(bay_bata_poverty,file=file.path(output,"BATS_2023_bay_bata_poverty_trips.csv"),row.names=F)
write.csv(bay_all,file=file.path(output,"BATS_2023_bay_all_trips.csv"),row.names=F)









