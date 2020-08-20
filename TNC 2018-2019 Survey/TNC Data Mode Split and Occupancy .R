# TNC Occupancy.R
# Summarize TNC trips by occupancy
# SI

# Import Libraries

suppressMessages(library(tidyverse))

# Set up working directory

wd <- "M:/Data/HomeInterview/TNC Survey/Analysis/"
setwd(wd)

# Bring in unlinked trip data, as we're not worried about transit trips
# Bring in household file to get county of residence information

load("M:/Data/HomeInterview/TNC Survey/Data/Final Version/Raw/R/trip.rdata")
load("M:/Data/HomeInterview/TNC Survey/Data/Final Version/Raw/R/household.rdata")


# Join home county to trips, give county names and an SF/non-SF designation, recode, subset relevant vars

home_fips <- household %>% 
  select(hh_id,home_county_fips)

joined <- left_join(trip,home_fips,by="hh_id") %>% 
  filter(is_tnc_trip==1) %>%                            # TNC trips only
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
    day=case_when(
      travel_date_dow==1   ~ "Monday",
      travel_date_dow==2   ~ "Tuesday",
      travel_date_dow==3   ~ "Wednesday",
      travel_date_dow==4   ~ "Thursday",
      travel_date_dow==5   ~ "Friday",
      travel_date_dow==6   ~ "Saturday",
      travel_date_dow==7   ~ "Sunday"
    ),
    travelers=case_when(
      num_travelers==1     ~ "1",
      num_travelers==2     ~ "2",
      num_travelers>=3     ~ "3+",
      TRUE                 ~ "4_unknown"
    )
  ) %>% 
  select(hh_id,person_num,trip_num,mode_1,mode_2,mode_3,mode_4,mode_type_imputed,is_tnc_trip,num_travelers,travelers,
         county_name,sf_or_not,day,daywt_alladult_wkday,daywt_alladult_7day)

# Sum by occupancy and SF or not,for weekdays only (using weekday weight)

final <- joined %>% 
  group_by(sf_or_not,travelers) %>% 
  summarize(total=sum(daywt_alladult_wkday))

write.csv(final, file="Weekday TNC Occupancies by SF or Not.csv", quote = T,row.names = FALSE)

