# TNC Survey Trip Length Frequency Distribution.r
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
  mutate(trip_bin=case_when(
    skim_distance>=0 & skim_distance<1                 ~ "0-1 miles",
    skim_distance>=1 & skim_distance<2                 ~ "1-2 miles",
    skim_distance>=2 & skim_distance<3                 ~ "2-3 miles",
    skim_distance>=3                                   ~ "3+ miles",
    TRUE                                               ~ "Miscoded"))

# Apply weeklong weights and summarize data
  
final <- working %>% 
  group_by(trip_bin) %>% 
  summarize(total=sum(daywt_alladult_7day))

# Output summary

write.csv(final,file.path(Output,"2019 TNC Survey Weeklong Trip Distances.csv"), row.names = F)




