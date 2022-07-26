# TNC Survey Trip Length Frequency Distribution Expanded.r
# Summarize trip lengths for weeklong travel

# Import Libraries

suppressMessages(library(tidyverse))

# Set up working directory

temp          <- "M:/Data/HomeInterview/TNC Survey/Data/Task 8 Data Refinement Version"
file_location <- file.path(temp,"Final Updated Dataset as of 10-18-2021","RSG_HTS_Oct2021_bayarea")
trip_location <- file.path(file_location,"trip_linked.tsv")

skim_location <- "Z:/Projects/2015_TM152_IPA_17/skims/skims_csv/HWYSKMAM.csv"

USERPROFILE   <- gsub("////","/", Sys.getenv("USERPROFILE"))
BOX_TM        <- file.path(USERPROFILE, "Box", "Modeling and Surveys", "Surveys")
Output        <- file.path(BOX_TM,"Travel Diary Survey","Requests","Kara Oberg")


# Bring in data

trip            <- read_tsv(trip_location,col_names=TRUE)
skim            <- read.csv(skim_location) %>% 
  select(orig,dest,skim_distance=DISTDA)

# Join data on origin/destination and recode to trip lengths

working <- left_join(trip,skim, by=c("o_taz"="orig","d_taz"="dest")) %>% 
  filter(!is.na(skim_distance)) %>% 
  mutate(skim_trip_bin=case_when(
    skim_distance>=0 & skim_distance<1                 ~ "0-1 miles",
    skim_distance>=1 & skim_distance<2                 ~ "1-2 miles",
    skim_distance>=2 & skim_distance<3                 ~ "2-3 miles",
    skim_distance>=3 & skim_distance<4                 ~ "3-4 miles",
    skim_distance>=4 & skim_distance<5                 ~ "4-5 miles",
    skim_distance>=5 & skim_distance<6                 ~ "5-6 miles",
    skim_distance>=6 & skim_distance<7                 ~ "6-7 miles",
    skim_distance>=7 & skim_distance<8                 ~ "7-8 miles",
    skim_distance>=8 & skim_distance<9                 ~ "8-9 miles",
    skim_distance>=9 & skim_distance<10                ~ "9-10 miles",
    skim_distance>=10 & skim_distance<15               ~ "10-15 miles",
    skim_distance>=15 & skim_distance<20               ~ "15-20 miles",
    skim_distance>=20 & skim_distance<25               ~ "20-25 miles",
    skim_distance>=25 & skim_distance<30               ~ "25-30 miles",
    skim_distance>=30 & skim_distance<35               ~ "30-35 miles",
    skim_distance>=35 & skim_distance<40               ~ "35-40 miles",
    skim_distance>=40 & skim_distance<45               ~ "40-45 miles",
    skim_distance>=45 & skim_distance<50               ~ "45-50 miles",
    skim_distance>=50                                  ~ "50+ miles",
    TRUE                                               ~ "Miscoded"),
    
    rsg_trip_bin=case_when(
      distance>=0 & distance<1                 ~ "0-1 miles",
      distance>=1 & distance<2                 ~ "1-2 miles",
      distance>=2 & distance<3                 ~ "2-3 miles",
      distance>=3 & distance<4                 ~ "3-4 miles",
      distance>=4 & distance<5                 ~ "4-5 miles",
      distance>=5 & distance<6                 ~ "5-6 miles",
      distance>=6 & distance<7                 ~ "6-7 miles",
      distance>=7 & distance<8                 ~ "7-8 miles",
      distance>=8 & distance<9                 ~ "8-9 miles",
      distance>=9 & distance<10                ~ "9-10 miles",
      distance>=10 & distance<15               ~ "10-15 miles",
      distance>=15 & distance<20               ~ "15-20 miles",
      distance>=20 & distance<25               ~ "20-25 miles",
      distance>=25 & distance<30               ~ "25-30 miles",
      distance>=30 & distance<35               ~ "30-35 miles",
      distance>=35 & distance<40               ~ "35-40 miles",
      distance>=40 & distance<45               ~ "40-45 miles",
      distance>=45 & distance<50               ~ "45-50 miles",
      distance>=50                             ~ "50+ miles",
      TRUE                                     ~ "Miscoded"))
    

# Apply weeklong weights and summarize data
  
skim_final <- working %>% 
  group_by(skim_trip_bin) %>% 
  summarize(skim_total=sum(daywt_alladult_7day))

rsg_final <- working %>% 
  group_by(rsg_trip_bin) %>% 
  summarize(rsg_total=sum(daywt_alladult_7day))

final <- left_join(rsg_final,skim_final,by=c("rsg_trip_bin"="skim_trip_bin"))

# Output summary

write.csv(final,file.path(Output,"2019 TNC Survey Weeklong Trip Distances Expanded.csv"), row.names = F)




