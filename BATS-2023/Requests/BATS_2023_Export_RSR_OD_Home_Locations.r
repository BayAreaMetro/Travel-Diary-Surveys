# BATS_2023_Export_RSR_OD_Home_Locations.r
# Export trips that use the RSR Bridge, include O/D locations and weight

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

# Import trip data

all_trips <- read.csv(file.path(data_loc,"trip.csv"))

# Import freeway boolean file

booleans <- read.csv(file.path(conflation_loc,"BATS 2023 Facility Use Booleans Toll.csv")) %>% 
  filter(rsr_bridge==1)

# Join the two files and output appropriate variables 

joined <- left_join(booleans,all_trips,by="trip_id") %>% 
  select(grep("_id|lat|lon|weight|rsr", names(.), value = TRUE))

# Output file to CSV

write.csv(joined,file=file.path(output,"BATS_2023_rsr_trip_origin_destination.csv"),row.names=F)










