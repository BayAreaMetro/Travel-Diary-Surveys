# Travel_Diary_ACS_2022_Persons_HH_Weight.R
# Compare persons using household weight vs person weight vs PUMS 2022 vs ACS 2022

# Include libraries for Census data extraction and working with tidyverse tools

library(tidyverse)
library(tidycensus)

# Travel diary files

# Load the survey data
folder  <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset/WeightedDataset_09112024"
hh      <- read.csv(file.path(folder,"hh.csv"))
person  <- read.csv(file.path(folder,"person.csv"))

# Bring in ACS 2022 population data

person_county_acs <- get_acs(
  geography = "county",
  county = c("Alameda","Contra Costa","Marin","Napa","San Francisco","San Mateo","Santa Clara","Solano","Sonoma"),
  state = "california",
  table = "B01003",
  year = 2022,
  survey = "acs1") %>% 
  rename(County=NAME) %>% 
  mutate(County = gsub(" County, California", "", County)) %>% 
  select(County,ACS_Pop=estimate)

person_ACS_Bay <- person_county_acs %>% 
  summarize(County="Bay Area",ACS_Pop=sum(ACS_Pop))

person_ACS_stratum <- person_county_acs %>% 
  filter(County %in% c("Marin","Sonoma","Napa")) %>% 
  summarize(County="Marin_Napa_Sonoma",ACS_Pop=sum(ACS_Pop))

final_acs <- rbind(person_county_acs,person_ACS_stratum,person_ACS_Bay)

# Survey processing

related_persons <- person %>%
  filter(relationship != "6") %>%          # Remove unrelated persons
  group_by(hh_id) %>%                      # Group by hh_id
  summarize(num_persons_related = n())  

household_survey <- hh %>% 
  left_join(.,related_persons, by="hh_id") %>% 
  select(hh_id,home_county,num_people,num_persons_related,grep("weight",names(.))) %>%
  mutate(home_county = recode(home_county,
                              "6001" = "Alameda",
                              "6013" = "Contra Costa",
                              "6041" = "Marin",
                              "6055" = "Napa",
                              "6075" = "San Francisco",
                              "6081" = "San Mateo",
                              "6085" = "Santa Clara",
                              "6095" = "Solano",
                              "6097" = "Sonoma")) 
  
total_hh_persons <- household_survey %>% 
  group_by(home_county) %>% 
  summarize(total=sum(hh_weight*num_people)) %>% 
  ungroup()

bay_hh_persons <- household_survey %>% 
  summarize(home_county="Bay Area",total=sum(hh_weight*num_people)) %>% 
  ungroup()

stratum_hh_persons <- household_survey %>% 
  filter(home_county %in% c("Marin","Sonoma","Napa")) %>% 
  summarize(home_county="Marin_Napa_Sonoma",total=sum(hh_weight*num_people)) %>% 
  ungroup()

final_hh_survey <- rbind(total_hh_persons,stratum_hh_persons,bay_hh_persons) %>% 
  rename(County=home_county,survey_hhwgt=total)

related_hh_persons <- household_survey %>% 
  group_by(home_county) %>% 
  summarize(total=sum(hh_weight*num_persons_related)) %>% 
  ungroup()

bay_related <- household_survey %>% 
  summarize(home_county="Bay Area",total=sum(hh_weight*num_persons_related)) %>% 
  ungroup()

stratum_related <- household_survey %>% 
  filter(home_county %in% c("Marin","Sonoma","Napa")) %>% 
  summarize(home_county="Marin_Napa_Sonoma",total=sum(hh_weight*num_persons_related)) %>% 
  ungroup()

final_related <- rbind(related_hh_persons,stratum_related,bay_related) %>% 
  rename(County=home_county,related_hhwgt=total)

person_sum <- person %>% 
  left_join(.,household_survey %>% select(hh_id,home_county),by="hh_id") %>% 
  group_by(home_county) %>% 
  summarize(total=sum(person_weight))

person_sum_Bay <- person_sum %>% 
  summarize(home_county="Bay Area",total=sum(total))

person_sum_stratum <- person_sum %>% 
  filter(home_county %in% c("Marin","Sonoma","Napa")) %>% 
  summarize(home_county="Marin_Napa_Sonoma",total=sum(total))

final_person_survey <- rbind(person_sum,person_sum_stratum,person_sum_Bay) %>% 
  rename(County=home_county,person_wgt=total)

# PUMS

baypuma       <- c("00101","00111","00112","00113","00114","00115","00116","00117","00118","00119","00120","00121","00122",
                   "00123","01301","01305","01308","01309","01310","01311","01312","01313","01314","04103","04104", "05500",
                   "07507","07508","07509","07510","07511","07512","07513","07514","08101","08102","08103","08104","08105","08106",
                   "08505","08506","08507","08508","08510","08511","08512","08515","08516","08517","08518","08519","08520","08521",
                   "08522","09501", "09502", "09503","09702","09704","09705","09706")

alameda       <- c("00101","00111","00112","00113","00114","00115","00116","00117","00118","00119","00120","00121","00122","00123")
contra_costa  <- c("01301","01305","01308","01309","01310","01311","01312","01313","01314")
marin         <- c("04103","04104")
napa          <- c("05500")
san_francisco <- c("07507","07508","07509","07510","07511","07512","07513","07514")
san_mateo     <- c("08101","08102","08103","08104","08105","08106")
santa_clara   <- c("08505","08506","08507","08508","08510","08511","08512","08515","08516","08517","08518","08519","08520","08521","08522")
solano        <- c("09501", "09502", "09503")
sonoma        <- c("09702","09704","09705","09706")

# Script to sum 2022 PUMS hh persons

bay_pums <- get_pums(
  variables = c("PUMA","NP","SPORDER"),
  survey = "acs1",
  state = "CA",
  year = 2022,
  recode = TRUE) %>% 
  filter(PUMA %in% baypuma) %>% 
  mutate(county=case_when(
    PUMA %in% alameda                   ~ "Alameda",
    PUMA %in% contra_costa              ~ "Contra Costa",
    PUMA %in% marin                     ~ "Marin",
    PUMA %in% napa                      ~ "Napa",
    PUMA %in% san_francisco             ~ "San Francisco",
    PUMA %in% san_mateo                 ~ "San Mateo",
    PUMA %in% santa_clara               ~ "Santa Clara",
    PUMA %in% solano                    ~ "Solano",
    PUMA %in% sonoma                    ~ "Sonoma",
    TRUE                                ~ "Miscoded"
  ))

county_pop_pums_hh <- bay_pums %>%
  filter(SPORDER==1) %>% 
  group_by(county) %>% 
  summarize(total=sum(as.numeric(NP)*WGTP)) %>% 
  ungroup()
  
bay_pop_pums_hh <- county_pop_pums_hh %>% 
  summarize(county="Bay Area",total=sum(total))

stratum_pop_pums_hh <- county_pop_pums_hh %>% 
  filter(county %in% c("Marin","Napa","Sonoma")) %>% 
  summarize(county="Marin_Napa_Sonoma",total=sum(total)) 

final_pop_pums_hh <- rbind(county_pop_pums_hh,stratum_pop_pums_hh,bay_pop_pums_hh) %>% 
  rename(County=county,PUMS_hh_person=total)

# Script to sum 2022 PUMS persons

county_pop_pums_person <- bay_pums %>%
  group_by(county) %>% 
  summarize(total=sum(PWGTP)) %>% 
  ungroup()

bay_pop_pums_person <- county_pop_pums_person %>% 
  summarize(county="Bay Area",total=sum(total))

stratum_pop_pums_person <- county_pop_pums_person %>% 
  filter(county %in% c("Marin","Napa","Sonoma")) %>% 
  summarize(county="Marin_Napa_Sonoma",total=sum(total)) 

final_pop_pums_person <- rbind(county_pop_pums_person,stratum_pop_pums_person,bay_pop_pums_person) %>% 
  rename(County=county,PUMS_person=total)

# Merge all together

dfs <- list(final_person_survey,final_hh_survey,final_related,final_pop_pums_person,final_pop_pums_hh,final_acs)

final <- Reduce(function(x,y) full_join(x,y,by="County"),dfs)

# Export the data

write.csv(final,file = "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset/WeightedDataset_09112024/BAT2023_SepDeliverable_Review/Comparison of weights.csv",row.names = F)
