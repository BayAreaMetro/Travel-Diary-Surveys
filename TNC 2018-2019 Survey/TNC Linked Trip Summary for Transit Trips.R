# TNC Linked Trip Summary for Transit Trips.R
# Summarize trips by Access/Egress using TNCs
# December 14, 2022

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

transit <- trip %>% 
  filter(is_transit==1)

final <- transit %>% 
  mutate(tnc_access_egress=if_else(egress_mode_type==9 | access_mode_type==9,daywt_alladult_wkday,0)) %>% 
  summarize(tnc_access_egress=sum(tnc_access_egress),total=sum(daywt_alladult_wkday))

# TNC_replace

transit_alternative <- trip %>% 
  filter(is_tnc_trip==1) %>% 
  mutate(transit_trip=if_else(tnc_replace %in% c(4,9,10),daywt_alladult_wkday,0)) %>% 
  summarize(transit=sum(transit_trip),total=sum(daywt_alladult_wkday)) %>%
  mutate(share=transit/total) %>% 
  ungroup()

# Under 35

under_35 <- trip %>% 
  filter(is_tnc_trip==1) %>% 
  left_join(.,person,by="person_id") %>% 
  mutate(under_35=if_else(age<6,daywt_alladult_7day,0)) %>% 
  summarize(under_35=sum(under_35),total=sum(daywt_alladult_wkday)) %>%
  mutate(share=under_35/total) 

# Share that TNC trips is for all trips, weekday

tnc_alternative <- trip %>% 
  mutate(tnc_total=if_else(is_tnc_trip==1,daywt_alladult_wkday,0)) %>% 
  summarize(tnc_total=sum(tnc_total),total=sum(daywt_alladult_wkday)) %>%
  mutate(share=tnc_total/total) %>% 
  ungroup()

View(tnc_alternative)

# Weeklong

tnc_alternative_week <- trip %>% 
  mutate(tnc_total=if_else(is_tnc_trip==1,daywt_alladult_7day,0)) %>% 
  summarize(tnc_total=sum(tnc_total),total=sum(daywt_alladult_7day)) %>%
  mutate(share=tnc_total/total) %>% 
  ungroup()

View(tnc_alternative_week)
