# TNC Linked Trips Summary.R
# Summarize Mode_Type variable 
# July 8, 2021

# Import Libraries

suppressMessages(library(tidyverse))

# Set up working directory

file_location <- "M:/Data/HomeInterview/TNC Survey/Task Order 8 Additional Data Refinement/Final Deliverables/"
wd <- "C:/Users/sisrael/Documents/GitHub/Travel-Diary-Surveys/TNC 2018-2019 Survey/"  # work directory
setwd(wd)
output <- "H:/Presentations/Planning Section 2021/"

# Bring in data

trip_location 	<- paste0(file_location,"Final Updated Dataset as of 4-1-2021/RSG_HTS_February2021_bayarea/trip_linked.tsv")

trip            <- read_tsv(trip_location,col_names=TRUE)

final <- trip %>% 
  filter(!(mode_type %in% c(-9998,995))) %>% 
  mutate(
    mode_rc=case_when(
      mode_type==1                    ~ "1_walk",
      mode_type==2                    ~ "2_bike",
      mode_type==3                    ~ "3_car",
      mode_type==4                    ~ "4_other",
      mode_type==5                    ~ "5_transit",
      mode_type==6                    ~ "4_other",
      mode_type==7                    ~ "4_other",
      mode_type==8                    ~ "4_other",
      mode_type==9                    ~ "4_other",
      mode_type==10                    ~ "3_car",
      mode_type==11                    ~ "2_bike",
      mode_type==12                    ~ "4_other",
      mode_type==13                    ~ "4_other"))

summary <- final %>% 
  group_by(mode_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday)) %>% 
  spread(mode_rc,total,fill=0) 

write.csv(summary,paste0(output,"2019 TNC Survey Mode Summary.csv"), row.names = F)
      

 
