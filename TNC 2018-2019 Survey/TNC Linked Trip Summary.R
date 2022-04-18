# TNC Linked Trips Summary.R
# Summarize Mode_Type variable 
# July 8, 2021

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

# HH income joiner for trip file
# Recode income
# Recode county FIPS to the county strata used for weighting the data

income_joiner <- hh %>% 
  select(hh_id,income_detailed,home_county_fips) %>% 
  mutate(
    income_rc=case_when(
      income_detailed==1               ~ "1_under 25",
      income_detailed==2               ~ "1_under 25",
      income_detailed==3               ~ "2_25-50",
      income_detailed==4               ~ "2_25-50",
      income_detailed==5               ~ "3_50-75",
      income_detailed==6               ~ "4_75-100",
      income_detailed==7               ~ "5_100-150",
      income_detailed==8               ~ "6_150+",
      income_detailed==9               ~ "6_150+",
      income_detailed==10              ~ "6_150+",
      TRUE                             ~ "7_missing"
    ),
    county_rc=case_when(
      home_county_fips==1              ~ "Alameda",
      home_county_fips==13             ~ "Contra Costa",
      home_county_fips==41             ~ "Marin",
      home_county_fips==55             ~ "Napa and Sonoma",
      home_county_fips==75             ~ "San Francisco",
      home_county_fips==81             ~ "San Mateo",
      home_county_fips==85             ~ "Santa Clara",
      home_county_fips==95             ~ "Solano",
      home_county_fips==97             ~ "Napa and Sonoma"
    )
  )

# Race and age joiner from the person file
# Filter out active participants and recode race/ethnicity to survey categories

race_joiner <- person %>% 
  filter(is_active_participant==1) %>% 
  select(hh_id,person_id,raceeth_new_imputed,age) %>% mutate(
  raceeth_rc = case_when(
    raceeth_new_imputed== -1                    ~ "missing",
    raceeth_new_imputed== 1                     ~ "5_Hispanic",
    raceeth_new_imputed== 2                     ~ "2_Black",
    raceeth_new_imputed== 3                     ~ "3_Asian/PI",
    raceeth_new_imputed== 4                     ~ "1_White",
    raceeth_new_imputed== 5                     ~ "4_Other"
    ),
  age_rc = case_when(
    age %in% 1:8                        ~ "1_under65",
    age %in% 9:10                       ~ "2_65+",
    TRUE                                ~ "not coded"
  )
  )

# Recode by trip type and join to race and income joiner
# Filter missing mode records and long-distance trips

final <- trip %>% 
  filter(!(mode_type_imputed %in% c(-9998,995,13))) %>% 
  mutate(
    mode_rc=case_when(
      mode_type_imputed==1                    ~ "1_roadway", # walk
      mode_type_imputed==2                    ~ "1_roadway", # bike
      mode_type_imputed==3                    ~ "1_roadway", # car
      mode_type_imputed==4                    ~ "1_roadway", # taxi
      mode_type_imputed==5                    ~ "2_transit", # transit
      mode_type_imputed==6                    ~ "2_transit", # schoolbus
      mode_type_imputed==7                    ~ "1_roadway", # other
      mode_type_imputed==8                    ~ "1_roadway", # shuttle/vanpool
      mode_type_imputed==9                    ~ "1_roadway", # TNC
      mode_type_imputed==10                   ~ "1_roadway", # Carshare
      mode_type_imputed==11                   ~ "1_roadway", # Bikeshare
      mode_type_imputed==12                   ~ "1_roadway", # Scootershare
      TRUE                                    ~ "not coded"))  %>% 
  left_join(.,race_joiner,by=c("hh_id","person_id")) %>% 
  left_join(.,income_joiner,by="hh_id")

### Summaries

## Income

# Totals by income

income_tot_summary <- final %>% 
  group_by(county_rc,income_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday)) %>% 
  spread(income_rc,total,fill=0) 

# Totals by mode and income

inc_mode_summary <- final %>% 
  group_by(county_rc,mode_rc,income_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday)) %>% 
  spread(income_rc,total,fill=0) %>% 
  arrange(mode_rc,county_rc)

## Race

# Totals by race

race_tot_summary <- final %>% 
  group_by(county_rc,raceeth_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday)) %>% 
  spread(raceeth_rc,total,fill=0) 

# Totals by race and mode

race_mode_summary <- final %>% 
  group_by(county_rc,mode_rc,raceeth_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday)) %>% 
  spread(raceeth_rc,total,fill=0) %>% 
  arrange(mode_rc,county_rc)

## Age

# Totals by age

age_tot_summary <- final %>% 
  group_by(county_rc,age_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday)) %>% 
  spread(age_rc,total,fill=0) 

# Totals by age and mode

age_mode_summary <- final %>% 
  group_by(county_rc,mode_rc,age_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday)) %>% 
  spread(age_rc,total,fill=0) %>% 
  arrange(mode_rc,county_rc)

## VMT

# Total VMT by Income

vmt_income_tot_summary <- final %>% 
  group_by(county_rc,income_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday*distance)) %>% 
  spread(income_rc,total,fill=0) 

# Total VMT by mode and income

vmt_inc_mode_summary <- final %>% 
  group_by(county_rc,mode_rc,income_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday*distance)) %>% 
  spread(income_rc,total,fill=0) %>% 
  arrange(mode_rc,county_rc)

# Total VMT by race

vmt_race_tot_summary <- final %>% 
  group_by(county_rc,raceeth_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday*distance)) %>% 
  spread(raceeth_rc,total,fill=0) 

# Total VMT by race and mode

vmt_race_mode_summary <- final %>% 
  group_by(county_rc,mode_rc,raceeth_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday*distance)) %>% 
  spread(raceeth_rc,total,fill=0) %>% 
  arrange(mode_rc,county_rc)

# Total VMT by age

vmt_age_tot_summary <- final %>% 
  group_by(county_rc,age_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday*distance)) %>% 
  spread(age_rc,total,fill=0) 

# Total VMT by age and mode

vmt_age_mode_summary <- final %>% 
  group_by(county_rc,mode_rc,age_rc) %>% 
  summarize(total=sum(daywt_alladult_wkday*distance)) %>% 
  spread(age_rc,total,fill=0) %>% 
  arrange(mode_rc,county_rc)

# Export files

write.csv(income_tot_summary,    file.path(Output,"2019 TNC Survey County Income Trips Summary.csv"), row.names = F)
write.csv(inc_mode_summary,      file.path(Output,"2019 TNC Survey County Income Mode Trips Summary.csv"), row.names = F)
write.csv(race_tot_summary,      file.path(Output,"2019 TNC Survey County Race Trips Summary.csv"), row.names = F)
write.csv(race_mode_summary,     file.path(Output,"2019 TNC Survey County Race Mode Trips Summary.csv"), row.names = F)
write.csv(age_tot_summary,       file.path(Output,"2019 TNC Survey County Age Trips Summary.csv"), row.names = F)
write.csv(age_mode_summary,      file.path(Output,"2019 TNC Survey County Age Mode Trips Summary.csv"), row.names = F)
write.csv(vmt_income_tot_summary,file.path(Output,"2019 TNC Survey County VMT Income Summary.csv"), row.names = F)
write.csv(vmt_inc_mode_summary,  file.path(Output,"2019 TNC Survey County VMT Income Mode Summary.csv"), row.names = F)
write.csv(vmt_race_tot_summary,  file.path(Output,"2019 TNC Survey County VMT Race Summary.csv"), row.names = F)
write.csv(vmt_race_mode_summary, file.path(Output,"2019 TNC Survey County VMT Race Mode Summary.csv"), row.names = F)
write.csv(vmt_age_tot_summary,   file.path(Output,"2019 TNC Survey County VMT Age Summary.csv"), row.names = F)
write.csv(vmt_age_mode_summary,  file.path(Output,"2019 TNC Survey County VMT Age Mode Summary.csv"), row.names = F)



 
