# TNC Survey Telecommute.R
# Summarize TNC survey data telecommute
# SI

# Import Libraries

suppressMessages(library(tidyverse))

# Set up working directory

wd <- "M:/Data/HomeInterview/TNC Survey/Analysis/"
setwd(wd)

# Bring in day data
# Bring in household file to get county of residence information

load("M:/Data/HomeInterview/TNC Survey/Data/Final Version/Raw/R/day.rdata")
load("M:/Data/HomeInterview/TNC Survey/Data/Final Version/Raw/R/person.rdata")
load("M:/Data/HomeInterview/TNC Survey/Data/Final Version/Raw/R/household.rdata")


# Join home county to trips, give county names and an SF/non-SF designation, recode key variables

person_emp <- person %>% 
  select(person_id,employment,job_type,is_active_participant) %>% 
  filter(is_active_participant==1 & employment %in% c(1,2,3))     # Include employed, active participants only

home_fips <- household %>% 
  select(hh_id,home_county_fips)

temp <- inner_join(day,person_emp,by="person_id")

joined <- left_join(temp,home_fips,by="hh_id") %>% 
  mutate(
  county_name=case_when(
    home_county_fips=="001"   ~ "Alameda",
    home_county_fips=="013"   ~ "Contra Costa",
    home_county_fips=="041"   ~ "Marin",
    home_county_fips=="055"   ~ "Napa",
    home_county_fips=="075"   ~ "San Francisco",
    home_county_fips=="081"   ~ "San Mateo",
    home_county_fips=="085"   ~ "Santa Clara",
    home_county_fips=="095"   ~ "Solano",
    home_county_fips=="097"   ~ "Sonoma"
  ),
  sf_or_not=case_when(
    county_name=="San Francisco"    ~ "SF",
    TRUE                            ~ "Not SF"
  ),
  telecommute=case_when(
    telework_time==0                                ~ "0 Minutes",
    telework_time>0 & telework_time<60              ~ "1 to 59 minutes",
    telework_time>=60 & telework_time<120           ~ "60 to 119 minutes",
    telework_time>=120 & telework_time<180          ~ "120 to 179 minutes",
    telework_time>=180 & telework_time<240          ~ "180 to 239 minutes",
    telework_time>=240 & telework_time<300          ~ "240 to 299 minutes",
    telework_time>=300 & telework_time<360          ~ "300 to 359 minutes",
    telework_time>=360 & telework_time<420          ~ "360 to 419 minutes",
    telework_time>=420 & telework_time<480          ~ "420 to 479 minutes",
    telework_time>=480                              ~ "480+ minutes",
    TRUE                                            ~ "No value given"
  ),
  job_typerc=case_when(
    job_type==1                                     ~ "1_One location, may telework",
    job_type==2                                     ~ "2_Work location varies",
    job_type==3                                     ~ "3_Work at home only",
    job_type==4                                     ~ "4_Drive/travel for work",
    TRUE                                            ~ "5_No value given"
  )
) 

# Sum person days by SF or not,for weekdays only (using weekday weight), by job type and number of telecommute hours

final <- joined %>% 
  group_by(sf_or_not,job_typerc,telecommute) %>% 
  summarize(total=sum(daywt_alladult_wkday))

write.csv(final, file="Weekday Telecommute Time by SF or Not.csv", quote = T,row.names = FALSE)