# TNC Linked Trip Summary for Walking and Biking Trips.R
# Summarize non-motorized trips by origin/destination TAZs
# December 27, 2022

# Remove scientific notation

options(scipen=999)

# Import Libraries

suppressMessages(library(tidyverse))

# Set up working directory

temp <- "M:/Data/HomeInterview/TNC Survey/Data/Task 8 Data Refinement Version"
file_location <- file.path(temp,"Final Updated Dataset as of 10-18-2021","RSG_HTS_Oct2021_bayarea")

USERPROFILE          <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
BOX_TM               <- file.path(USERPROFILE, "Box", "Modeling and Surveys")
Output               <- file.path(BOX_TM,"Share Data","bespoke","2023 TIP Investment Analysis")

# Bring in data

trip_location 	<- file.path(file_location,"trip_linked.tsv")
hh_location     <- file.path(file_location,"hh.tsv")
person_location <- file.path(file_location,"person.tsv")

trip            <- read_tsv(trip_location,col_names=TRUE)
hh              <- read_tsv(hh_location,col_names=TRUE)
person          <- read_tsv(person_location,col_names=TRUE)

# Create new variable for TNC access/egress, summarize share of transit trips

nonmotorized <- trip %>% 
  filter(mode_type %in% c(1,2))

final <- nonmotorized %>% 
  group_by(o_taz,d_taz,mode_1,mode_2,mode_3) %>% 
  summarize(weekday=sum(daywt_alladult_wkday),sevenday=sum(daywt_alladult_7day)) %>% 
  ungroup()

write.csv(final,file.path("M:\\Data\\Requests\\Julia Griswold","BATS2019_NonMotorizedModes_TAZs.csv"),row.names = F)
