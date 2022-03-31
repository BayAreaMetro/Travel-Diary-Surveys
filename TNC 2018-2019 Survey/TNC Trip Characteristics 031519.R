# TNC Trip Characteristics.R
# Analyze TNC trip characteristics for draft dataset
# March 15, 2019

# Import Libraries

suppressMessages(library(dplyr))

# Set up working directory

wd <- "M:/Data/HomeInterview/2018 TNC Survey/"
setwd(wd)

# Bring in data

TNC_Trips 	    <- "M:/Data/HomeInterview/2018 TNC Survey/2019-02-28_Bay_Area_TNC_Datasets/Bay_Area_TNC_Fall2018_Trips.csv"
TNC_Persons 	  <- "M:/Data/HomeInterview/2018 TNC Survey/2019-02-28_Bay_Area_TNC_Datasets/Bay_Area_TNC_Fall2018_Persons.csv"
TNC_Locations 	<- "M:/Data/HomeInterview/2018 TNC Survey/2019-02-28_Bay_Area_TNC_Datasets/Bay_Area_TNC_Fall2018_Locations.csv"
TNC_Households 	<- "M:/Data/HomeInterview/2018 TNC Survey/2019-02-28_Bay_Area_TNC_Datasets/Bay_Area_TNC_Fall2018_Households.csv"
TNC_Vehicles 	  <- "M:/Data/HomeInterview/2018 TNC Survey/2019-02-28_Bay_Area_TNC_Datasets/Bay_Area_TNC_Fall2018_Vehicles.csv"
TNC_Days 	      <- "M:/Data/HomeInterview/2018 TNC Survey/2019-02-28_Bay_Area_TNC_Datasets/Bay_Area_TNC_Fall2018_Persons.csv"

trips       <- read.csv(TNC_Trips,header=TRUE) %>% mutate(weight=1)
persons     <- read.csv(TNC_Persons,header=TRUE) 
locations   <- read.csv(TNC_Locations,header=TRUE) 
households  <- read.csv(TNC_Households,header=TRUE) 
vehicles    <- read.csv(TNC_Vehicles,header=TRUE) 
days        <- read.csv(TNC_Days,header=TRUE) 

# Separate out Uber and Lyft trips

tnc_only <- trips %>%
  filter(mode_1 %in% c(64,65) | mode_2 %in% c(64,65) | mode_3 %in% c(64,65)) %>% mutate(
    fare_type=case_when(
      mode_lyft==1 | mode_uber==1 ~ "Pooled",
      mode_lyft==2 | mode_uber==2 ~ "Regular or Econo",
      mode_lyft==3 | mode_uber==3 ~ "Premium",
      TRUE ~ "Unknown"
    ))

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






 