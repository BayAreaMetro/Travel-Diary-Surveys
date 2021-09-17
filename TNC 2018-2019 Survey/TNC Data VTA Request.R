# TNC Data VTA Request.R

# Import Libraries

suppressMessages(library(tidyverse))

# Set up working directory

file_location <- "M:/Data/HomeInterview/TNC Survey/Task Order 8 Additional Data Refinement/Final Deliverables/"
wd <- "C:/Users/sisrael/Documents/GitHub/Travel-Diary-Surveys/TNC 2018-2019 Survey/"  # work directory
setwd(wd)
output <- "H:/Presentations/Planning Section 2021/"

# Bring in data

trip_location 	<- paste0(file_location,"Final Updated Dataset as of 4-1-2021/RSG_HTS_February2021_bayarea/trip_linked.tsv")
hh_location     <- paste0(file_location,"Final Updated Dataset as of 4-1-2021/RSG_HTS_February2021_bayarea/hh.tsv")
trip            <- read_tsv(trip_location,col_names=TRUE)
hh              <- read_tsv(hh_location,col_names=TRUE)

hh_join <- hh %>% select(hh_id,home_county_fips) %>% 
  mutate(home_county_name=recode(home_county_fips,
                     "1" ="Alameda",
                     "13"="Contra Costa",
                     "41"="Marin",
                     "55"="Napa",
                     "75"="San Francisco",
                     "81"="San Mateo",
                     "85"="Santa Clara",
                     "95"="Solano",
                     "97"="Sonoma"))

sum1_data <- trip %>% 
  left_join(.,hh_join,by="hh_id") %>% 
  filter(!(mode_type_imputed %in% c(-9998,995))) %>% 
  mutate(
    mode_rc=case_when(
      mode_type_imputed==1                                 ~ "1_walk",
      mode_type_imputed==2                                 ~ "2_bike",
      mode_type_imputed==3                                 ~ "3_car",
      mode_type_imputed==4                                 ~ "4_other",
      mode_type_imputed==5                                 ~ "5_transit",
      mode_type_imputed==6                                 ~ "4_other",
      mode_type_imputed==7                                 ~ "4_other",
      mode_type_imputed==8                                 ~ "4_other",
      mode_type_imputed==9 & mode_uber !=1 & mode_lyft !=1 ~ "6_TNC not pooled",
      mode_type_imputed==9 & (mode_uber==1 | mode_lyft==1) ~ "7_TNC pooled",
      mode_type_imputed==10                                ~ "3_car",
      mode_type_imputed==11                                ~ "2_bike",
      mode_type_imputed==12                                ~ "4_other",
      mode_type_imputed==13                                ~ "4_other",
      TRUE                                                 ~ "Not coded"))

summary1 <- final %>% 
  group_by(home_county_name,mode_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday)) %>% 
  spread(mode_rc,total,fill=0) 


sum2_data <- sum1_data %>% 
  mutate(temp1=if_else(d_purpose_category_imputed==-1 &
                               d_purpose_category !=-9998,d_purpose_category,d_purpose_category_imputed)) %>% 
  filter(temp1 !=-1) %>% 
  mutate(dest_purp=recode(temp1,
                          "1"     =	"Home",
                          "2"     =	"Work",
                          "3"     =	"Work-related",
                          "4"     =	"School",
                          "5"     =	"Escort",
                          "6"     =	"Shop",
                          "7"     =	"Meal",
                          "8"     =	"Social/recreation",
                          "9"     =	"Errand/appointment",
                          "10"	=	"Change mode",
                          "11"	=	"Spent the night at non-home location",
                          "12"	=	"Other/Missing",
                          "14"	=	"School-related"))

sum3_data <- sum2_data %>% 
  filter(tnc_wait_time!=995 & mode_rc %in% c("6_TNC not pooled","7_TNC pooled")) 

summary3 <- sum3_data %>%  
  group_by(home_county_name,mode_rc,dest_purp) %>% 
  summarize(avg_wait=weighted.mean(tnc_wait_time,daywt_alladult_wkday),data_records=n()) %>% 
  ungroup()

summary4 <- sum3_data %>% 
  group_by(home_county_name,mode_rc,dest_purp) %>% 
  summarize(avg_fixed_cost=weighted.mean(taxi_cost,daywt_alladult_wkday, na.rm=TRUE),data_records=n()) %>% 
  ungroup()


  

write.csv(summary,paste0(output,"2019 TNC Survey Mode Summary.csv"), row.names = F)
      
save(trip_linked, file = "M:/Data/HomeInterview/TNC Survey/Task Order 8 Additional Data Refinement/Final Deliverables/Final Updated Dataset as of 4-1-2021/RSG_HTS_February2021_bayarea/trip_linked.Rdata")
save(hh, file = "M:/Data/HomeInterview/TNC Survey/Task Order 8 Additional Data Refinement/Final Deliverables/Final Updated Dataset as of 4-1-2021/RSG_HTS_February2021_bayarea/hh.Rdata")

unlinked_location     <- paste0(file_location,"Final Updated Dataset as of 4-1-2021/RSG_HTS_February2021_bayarea/trip.tsv")
unlinked <- read_tsv(unlinked_location,col_names=TRUE)
trip <- unlinked
save(trip, file = "M:/Data/HomeInterview/TNC Survey/Task Order 8 Additional Data Refinement/Final Deliverables/Final Updated Dataset as of 4-1-2021/RSG_HTS_February2021_bayarea/trip.Rdata")

trial <- trip_unlinked %>% 
  filter(!(mode_type_imputed %in% c(-9998,995))) %>% 
  mutate(
    mode_rc=case_when(
      mode_type_imputed==1                    ~ "1_walk",
      mode_type_imputed==2                    ~ "2_bike",
      mode_type_imputed==3                    ~ "3_car",
      mode_type_imputed==4                    ~ "4_other",
      mode_type_imputed==5                    ~ "5_transit",
      mode_type_imputed==6                    ~ "4_other",
      mode_type_imputed==7                    ~ "4_other",
      mode_type_imputed==8                    ~ "4_other",
      mode_type_imputed==9 & tnc_pooled==995  ~ "6_TNC not pooled",
      mode_type_imputed==9 & tnc_pooled<995   ~ "7_TNC pooled",
      mode_type_imputed==10                    ~ "3_car",
      mode_type_imputed==11                    ~ "2_bike",
      mode_type_imputed==12                    ~ "4_other",
      mode_type_imputed==13                    ~ "4_other",
      TRUE                                     ~ "Not coded"))


trial <- trip %>%
 