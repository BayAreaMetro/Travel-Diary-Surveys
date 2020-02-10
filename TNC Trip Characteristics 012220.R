# TNC Trip Characteristics 012220.R
# Analyze TNC trip characteristics for draft dataset
# February 6, 2020

# Import Libraries

suppressMessages(library(tidyverse))

# Set up working directory

file_location <- "M:/Data/HomeInterview/TNC Survey/2020/Draft012220/BayArea_dataset_20200122/"
wd <- "M:/Data/HomeInterview/TNC Survey/2020/Draft012220/Analysis/"  # work directory
setwd(wd)

# Bring in data and most recent weights

TNC_Trip 	    <- paste0(file_location,"ex_trip.tsv")
TNC_Person 	  <- paste0(file_location,"ex_person.tsv")
TNC_Location 	<- paste0(file_location,"ex_location.tsv")
TNC_Household	<- paste0(file_location,"ex_hh.tsv")
TNC_Vehicle   <- paste0(file_location,"ex_vehicle.tsv")
TNC_Day 	    <- paste0(file_location,"ex_day.tsv")
TNC_Trip_Other<- paste0(file_location,"ex_trip_with_purpose_other.tsv")
TNC_Weights   <- "M:/Data/HomeInterview/TNC Survey/2020/Draft012220/Weights_update_Feb1/wts_bayarea_alladult4i.csv"

trip       <- read_tsv(TNC_Trip,col_names=TRUE)
person     <- read_tsv(TNC_Person,col_names=TRUE) 
location   <- read_tsv(TNC_Location,col_names=TRUE) 
household  <- read_tsv(TNC_Household,col_names=TRUE) 
vehicle    <- read_tsv(TNC_Vehicle,col_names=TRUE) 
day        <- read_tsv(TNC_Day,col_names=TRUE) 
trip_other <- read_tsv(TNC_Trip_Other,col_names=TRUE)
weight     <- read.csv(TNC_Weights,header = TRUE)

household_income <- household %>% 
  select(hh_id,income_detailed,income_followup,income_aggregate)

person_weighted <- left_join(person,weight, by="person_id") %>%    # Include only persons with a weight value 
  filter(!is.na(wt_alladult_WkDay)) %>% 
  left_join(.,household_income,by="hh_id") %>% 
  mutate(
    incomerc=case_when(
      income_imputed == 1   ~"1_less to 25k",
      income_imputed == 2   ~"2_25k to 49,999",
      income_imputed == 3   ~"3_50k to 74,999",
      income_imputed == 4   ~"4_75k to 99,999",
      income_imputed == 5   ~"5_100k to 149,999",
      income_imputed == 6   ~"6_150k to 199,999",
      income_imputed == 7   ~"7_200k to 249,999",
      income_imputed == 8   ~"8_250k+",
      TRUE                  ~"Uncoded"),
    racerc=case_when(
      ethnicity_white == 1 & ethnicity_multi == 0                       ~"1_White",
      ethnicity_af_am == 1 & ethnicity_multi == 0                       ~"2_Black",
      ethnicity_asian == 1 & ethnicity_multi == 0                       ~"3_Asian",
      ethnicity_aiak == 1 & ethnicity_multi ==  0                       ~"4_Other",
      ethnicity_hapi == 1 & ethnicity_multi ==  0                       ~"4_Other",
      ethnicity_mideast == 1 & ethnicity_multi == 0                     ~"4_Other",
      ethnicity_other == 1 & ethnicity_multi ==    0                    ~"4_Other",
      ethnicity_no_answer %in% c (1,-9998) & raceeth_imputed == 1       ~"1_White",
      ethnicity_no_answer %in% c (1,-9998) & raceeth_imputed == 2       ~"2_Black",
      ethnicity_no_answer %in% c (1,-9998) & raceeth_imputed == 3       ~"3_Asian",  # Note this includes PI in data.
      ethnicity_no_answer %in% c (1,-9998) & raceeth_imputed == 4       ~"5_Hispanic",
      ethnicity_hisp == 1                                               ~"5_Hispanic",
      ethnicity_multi == 1                                              ~"4_Other",
      TRUE                                                              ~"Uncoded"),
    agerc=case_when(
      age==4           ~"1_between 18 and 24",
      age==5           ~"2_between 25 and 34",
      age==6           ~"3_between 35 and 44",
      age==7           ~"4_between 45 and 54",
      age==8           ~"5_between 55 and 64",
      age==9 | age==10 ~"6_65+",
      TRUE                  ~"Uncoded")
  )

trial <- person_weighted %>% 
  filter(ethnicity_multi==1) %>% 
  select(ethnicity_af_am,ethnicity_aiak,ethnicity_asian,ethnicity_hapi,ethnicity_hisp,ethnicity_white,ethnicity_mideast,
         ethnicity_other,ethnicity_multi,ethnicity_no_answer,raceeth_imputed)

%>% 
  select(SERIALNO,SPORDER,PUMA,PUMA_Name,County_Name,wt_alladult_WkDay,age,JWTR,HINCP,adjustedinc, agerc,incomerc,racerc,RELP)


trial <- person_weighted %>% 
  select(income_imputed,income_detailed,income_followup,income_aggregate)
  

# Get median distance by mode
# Recode modes

trip2 <- trip %>%
  mutate(
    mode_recode=case_when(
      mode_type==1	~"Walk",
      mode_type==2	~"Bike",
      mode_type==3	~"Car",
      mode_type==4	~"Taxi",
      mode_type==5	~"Transit",
      mode_type==6	~"Schoolbus",
      mode_type==7	~"Other",
      mode_type==8	~"Shuttle/vanpool",
      mode_type==9	~"TNC",
      mode_type==10	~"Carshare",
      mode_type==11	~"Bikeshare",
      mode_type==12	~"Scooter share",
      mode_type==13	~"Long-distance passenger mode",
      TRUE~"Unknown")
)
  
# Total Median
median_dist_duration_speed <- trip2 %>%
  summarize(median_dist=median(distance,na.rm=TRUE), median_duration=median(duration,na.rm=TRUE), 
            median_speed=median(speed_mph,na.rm=TRUE))

# Median distance, duration, speed by mode
median_dist_duration_speed_mode <- trip2 %>%
  group_by(mode_recode) %>%
  summarize(median_dist=median(distance, na.rm=TRUE), median_duration=median(duration, na.rm=TRUE), 
            median_speed=median(speed_mph,na.rm=TRUE))


# Separate out Uber and Lyft trips

tnc_only <- trip %>%
  filter(mode_1 %in% c(64,65) | mode_2 %in% c(64,65) | mode_3 %in% c(64,65)) %>% mutate(
    fare_type=case_when(
      mode_lyft==1 | mode_uber==1 ~ "Pooled",
      mode_lyft==2 | mode_uber==2 ~ "Regular or Econo",
      mode_lyft==3 | mode_uber==3 ~ "Premium",
      TRUE ~ "Unknown"
    ))

tnc_only2 <- tnc_only %>% 
  filter(mode_type!=9)

# Join income to trip table

hh_income <- households %>%
  select(hh_id,income_detailed)

tnc_only <- left_join(tnc_only,hh_income, by="hh_id") 

# Recode income and trip purpose

tnc_only <- tnc_only %>% mutate(
  hh_income=case_when(
    income_detailed==1 ~ "Under $15,000",
    income_detailed==2 ~ "$15,000-$24,999",
    income_detailed==3 ~ "$25,000-$34,999",
    income_detailed==4 ~ "$35,000-$49,999",
    income_detailed==5 ~ "$50,000-$74,999",
    income_detailed==6 ~ "$75,000-$99,999",
    income_detailed==7 ~ "$100,000-$149,999",
    income_detailed==8 ~ "$150,000-$199,999",
    income_detailed==9 ~ "$200,000-$249,999",
    income_detailed==10 ~ "$250,000 or more",
    income_detailed==999 ~ "Prefer not to answer"),
  origin_purpose=case_when(
    o_purp_cat==1 ~ "Home",
    o_purp_cat==2 ~ "Work",
    o_purp_cat==3 ~ "Work-related",
    o_purp_cat==4 ~ "School",
    o_purp_cat==5 ~ "Escort",
    o_purp_cat==6 ~ "Shop",
    o_purp_cat==7 ~ "Meal",
    o_purp_cat==8 ~ "Social/recreation",
    o_purp_cat==9 ~ "Errand/other",
    o_purp_cat==10 ~ "Change mode"),
  destination_purpose=case_when(
    d_purp_cat==1 ~ "Home",
    d_purp_cat==2 ~ "Work",
    d_purp_cat==3 ~ "Work-related",
    d_purp_cat==4 ~ "School",
    d_purp_cat==5 ~ "Escort",
    d_purp_cat==6 ~ "Shop",
    d_purp_cat==7 ~ "Meal",
    d_purp_cat==8 ~ "Social/recreation",
    d_purp_cat==9 ~ "Errand/other",
    d_purp_cat==10 ~ "Change mode")
)

# Write out TNC-only trip file

write.csv(tnc_only, "TNC_Pilot_Trips.csv", row.names = FALSE, quote = T)

# Recode modes to get mode share for all trips and output summary

trips2 <- trips %>% mutate(
  mode_recode=case_when(
    mode_1==36 | mode_2==36 | mode_3==36                                   ~ "Taxi",
    mode_1 %in% c(64,65) | mode_2 %in% c(64,65) | mode_3 %in% c(64,65)     ~ "TNC",
    TRUE                                                                   ~ "Other"
    ),
  mode_recode2=case_when(
    mode_recode=="TNC" & (mode_uber==1 | mode_lyft==1)                     ~ "Pooled TNC",
    mode_recode=="TNC" & (mode_uber==2 | mode_lyft==2)                     ~ "Regular or Premium TNC",
    mode_recode=="TNC" & (mode_uber==3 | mode_lyft==3)                     ~ "Regular or Premium TNC",
    TRUE                                                                   ~ mode_recode
  )
)
  
summary <- trips2 %>%
  group_by(mode_recode2) %>%
  summarize(total=sum(weight))


trial <- trip %>%
  group_by(mode_1) %>%
  summarize(measure=median(distance))

trial <- trip %>%
  filter (hh_id==181027787 & person_num==1 & day_num==1)

trial <- trip %>%
  filter (hh_id==181145778 & person_num==2 & day_num==6)

trial2 <- trip %>%
  filter (hh_id==193038995 & person_num==1 & day_num==3)

person1 <- person %>% 
  filter (hh_id==181057950)

other <- trip %>% 
  filter(!is.na(d_purpose_other)) %>% 
  select(hh_id,person_num,trip_num,d_purpose_other,d_purpose_category_imputed,d_purpose_imputed, 
         d_purpose, d_purpose_category)


missing_mode <- trip %>% filter(mode_type==-9998 & mode_1>0)





 