# TNC Survey Trips by Mode Frequency.r
# Summarize trip modes by frequency

# Import Libraries

suppressMessages(library(tidyverse))
options(scipen = 999)

# Set up working directory

temp          <- "M:/Data/HomeInterview/TNC Survey/Data/Task 8 Data Refinement Version"
file_location <- file.path(temp,"Final Updated Dataset as of 10-18-2021","RSG_HTS_Oct2021_bayarea")
trip_location <- file.path(file_location,"trip_linked.tsv")



USERPROFILE   <- gsub("////","/", Sys.getenv("USERPROFILE"))
BOX_TM        <- file.path(USERPROFILE, "Box", "Modeling and Surveys", "Surveys")
Output        <- file.path(BOX_TM,"Travel Diary Survey","Requests","Shari Gershenfeld")


# Bring in data

trip            <- read_tsv(trip_location,col_names=TRUE)

# Apply weeklong weights and summarize data
  
mode1 <- trip %>% 
  group_by(mode_1) %>% 
  summarize(mode1_total=sum(daywt_alladult_wkday))
mode2 <- trip %>% 
  group_by(mode_2) %>% 
  summarize(mode2_total=sum(daywt_alladult_wkday))
mode3 <- trip %>% 
  group_by(mode_3) %>% 
  summarize(mode3_total=sum(daywt_alladult_wkday))
mode4 <- trip %>% 
  group_by(mode_4) %>% 
  summarize(mode4_total=sum(daywt_alladult_wkday))
mode_type <- trip %>% 
  group_by(mode_type) %>% 
  summarize(modetype_total=sum(daywt_alladult_wkday))
allmodes <- left_join(mode1,mode2,by=c("mode_1"="mode_2")) %>% 
  left_join(.,mode3,by=c("mode_1"="mode_3")) %>%
  left_join(.,mode4,b=c("mode_1"="mode_4")) %>% 
  mutate_at(vars(mode1_total,mode2_total,mode3_total,mode4_total),~if_else(is.na(.),0,.))

# Output summary

write.csv(allmodes,file.path(Output,"2019 TNC Survey Weekday Unlinked Modes.csv"), row.names = F)
write.csv(mode_type,file.path(Output,"2019 TNC Survey Weekday Mode Type.csv"), row.names = F)




