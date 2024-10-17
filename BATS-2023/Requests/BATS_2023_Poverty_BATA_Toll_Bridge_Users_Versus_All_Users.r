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

# Bring in BATA bridge trips excerpted from this process: 
# https://github.com/BayAreaMetro/Travel-Diary-Surveys/blob/master/BATS-2023/conflation-trip-summaries/BATS_2023_Survey_Facility_Margin_of_Error_Calculations_All_Facilities.r

bata_trips <- read.csv("M:/Data/Requests/Rebecca Long/BATA Bridge Use Versus All Residents/BATA_Bridge_Trips.csv")

# Get unique households of people making BATA bridge trips and get their poverty status

bata_hhs <- unique(bata_trips$hh_id)

poverty_hhs <- read.csv("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset/WeightedDataset_09112024/derived_variables/BATShh_ImputedIncomeValues.csv") %>% 
  filter(hh_id %in% bata_hhs) %>% 
  select(-hhInc_continuous)

# Bring in BATS 2023 trip file and append poverty BATS household data
# Recode mode names and keep only trips with a poverty_status value (i.e., hhs with a BATA bridge trip)
 
trip_poverty <- read.csv(file=file.path(data_loc, "trip.csv")) %>% 
  left_join(., poverty_hhs, by = "hh_id") %>% 
  mutate(mode_recode=case_when(
           mode_type==1	            ~ "Walk",
           mode_type==10	          ~ "School bus",
           mode_type==11	          ~ "Shuttle/vanpool",
           mode_type==12	          ~ "Ferry",
           mode_type==13	          ~ "Transit",
           mode_type==14	          ~ "Long distance passenger",
           mode_type==2	            ~ "Bike",
           mode_type==3	            ~ "Bikeshare",
           mode_type==4	            ~ "Scootershare",
           mode_type==5	            ~ "Taxi",
           mode_type==6	            ~ "TNC",
           mode_type==7	            ~ "Other",
           mode_type==8	            ~ "Car",
           mode_type==9	            ~ "Carshare",
           mode_type==995	          ~ "Missing Response")) %>% 
  filter(!is.na(poverty_status))

# Summarize mode type for BATA trip households by above and below poverty

bay_bata_poverty <- trip_poverty %>% 
  group_by(poverty_status,mode_recode) %>% 
  summarize(total=sum(trip_weight_rmove_only)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "poverty_status",values_from = "total",values_fill = 0 )

# Output file to CSV

write.csv(bay_bata_poverty,file=file.path(output,"BATS_2023_bay_bata_poverty_trips.csv"),row.names=F)










