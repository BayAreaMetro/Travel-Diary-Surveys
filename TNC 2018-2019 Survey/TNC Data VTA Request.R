# TNC Data VTA Request.R

# Import Libraries

suppressMessages(library(tidyverse))
library(spatstat)

# Set up working directory

file_location <- "M:/Data/HomeInterview/TNC Survey/Task Order 8 Additional Data Refinement/Final Deliverables/"
wd <- "M:/Data/Requests/Louisa Leung/TNC Data/"  # work directory
setwd(wd)

# Bring in data
# Filter out trips with no weekday trip weight and with a trip end outside the region

trip_location 	<- paste0(file_location,"Final Updated Dataset as of 4-1-2021/RSG_HTS_February2021_bayarea/trip_linked.tsv")
hh_location     <- paste0(file_location,"Final Updated Dataset as of 4-1-2021/RSG_HTS_February2021_bayarea/hh.tsv")

trip_in         <- read_tsv(trip_location,col_names=TRUE) 

trip <- trip_in %>% 
  filter(daywt_alladult_wkday>0 & o_taz>0 & d_taz>0)

hh              <- read_tsv(hh_location,col_names=TRUE)

skim_location <- "Z:/Projects/2015_TM152_IPA_17/skims/skims_csv/HWYSKMPM_TOLLDISTS2.csv"

skim            <- read.csv(skim_location,header = TRUE) %>% 
  gather(d_taz,skim_distance,-TOLLDISTS2) %>% 
  rename(o_taz=TOLLDISTS2) %>% 
  mutate(d_taz=as.numeric(str_replace(d_taz,"X","")))

# Make data modifications

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

# Join HH location county and recode trips

sum1_data <- trip %>% 
  left_join(.,hh_join,by="hh_id") %>% 
  filter(!(mode_type_imputed %in% c(-9998,995))) %>% 
  left_join(.,skim,by=c("o_taz","d_taz")) %>% 
  mutate(
    mode_rc=case_when(
      mode_type_imputed==1                                 ~ "walk",
      mode_type_imputed==2                                 ~ "bike",
      mode_type_imputed==3                                 ~ "car",
      mode_type_imputed==4                                 ~ "taxi",
      mode_type_imputed==5                                 ~ "transit",
      mode_type_imputed==6                                 ~ "schoolbus",
      mode_type_imputed==7                                 ~ "other",
      mode_type_imputed==8                                 ~ "shuttle/vanpool",
      mode_type_imputed==9 & mode_uber !=1 & mode_lyft !=1 ~ "TNC not pooled",
      mode_type_imputed==9 & (mode_uber==1 | mode_lyft==1) ~ "TNC pooled",
      mode_type_imputed==10                                ~ "carshare",
      mode_type_imputed==11                                ~ "bikeshare",
      mode_type_imputed==12                                ~ "scootershare",
      mode_type_imputed==13                                ~ "long distance",
      TRUE                                                 ~ "Not coded"),
    imputed_distance=if_else((distance>1.5*skim_distance | distance<skim_distance/1.5),skim_distance,distance)) %>% 
  mutate(temp1=if_else(d_purpose_category_imputed==-1 &
                         d_purpose_category !=-9998,d_purpose_category,d_purpose_category_imputed)) %>% 
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


summary1_TNC <- sum1_data %>% 
  filter(temp1 !=-1 & mode_rc %in% c("TNC not pooled","TNC pooled")) %>% 
  group_by(home_county_name,mode_rc) %>% 
  summarize(total_TNC=sum(daywt_alladult_wkday),total_records=n()) %>% 
  ungroup()

summary1_All <- sum1_data %>% 
  group_by(home_county_name) %>% 
  summarize(total_trips=sum(daywt_alladult_wkday),total_records=n()) %>% 
  ungroup()

sum2_data <- sum1_data

summary2 <- sum2_data %>% 
  group_by(mode_rc,home_county_name,dest_purp) %>% 
  summarize(avg_distance=weighted.mean(distance,daywt_alladult_wkday), 
            avg_skim_distance=weighted.mean(skim_distance,daywt_alladult_wkday),
            avg_imputed_distance=weighted.mean(imputed_distance,daywt_alladult_wkday),
            total_records=n()) %>% 
  ungroup()

sum3_data <- sum2_data %>% 
  filter(tnc_wait_time!=995)

summary3 <- sum3_data %>%  
  group_by(home_county_name,mode_rc,dest_purp) %>% 
  summarize(avg_wait=weighted.mean(tnc_wait_time,daywt_alladult_wkday),data_records=n()) %>% 
  ungroup()

summary4 <- sum3_data %>% 
  group_by(home_county_name,mode_rc,dest_purp) %>% 
  summarize(avg_fixed_cost=weighted.mean(taxi_cost,daywt_alladult_wkday, na.rm=TRUE),data_records=n()) %>% 
  ungroup()

# Output the data

write.csv(summary1_All,"2019 TNC Mode Share by Purpose - All Trips.csv", row.names = F)
write.csv(summary1_TNC,"2019 TNC Mode Share by Purpose - TNC Trips.csv", row.names = F)
write.csv(summary2,"2019 TNC Trip Length by Purpose.csv", row.names = F)
write.csv(summary3,"2019 TNC Wait Time.csv", row.names = F)
write.csv(summary4,"2019 TNC Fixed Cost.csv", row.names = F)

   