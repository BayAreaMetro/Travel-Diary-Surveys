# Compare_Survey_ACS_HHs_by_Size.R

library(pacman)
p_load(tidycensus)

# Load the survey data
folder  <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset/WeightedDataset_08092024"
hh      <- read.csv(file.path(folder,"hh.csv"))
person  <- read.csv(file.path(folder,"person.csv"))

# Bring in 2022 ACS data by HH size

household_county_acs <- get_acs(
  geography = "county",
  county = c("Alameda","Contra Costa","Marin","Napa","San Francisco","San Mateo","Santa Clara","Solano","Sonoma"),
  state = "california",
  table = "B25009",
  year = 2022,
  survey = "acs1") %>% 
  rename(County=NAME) %>% 
  mutate(County = gsub(" County, California", "", County)) %>% 
  select(-GEOID,-moe) %>% 
  filter(!(variable %in% c("B25009_001","B25009_002","B25009_010"))) %>% 
  mutate(ACS_size_recode=case_when(
    variable=="B25009_003"            ~ "hh1",
    variable=="B25009_004"            ~ "hh2",
    variable=="B25009_005"            ~ "hh3",
    variable=="B25009_006"            ~ "hh4p",
    variable=="B25009_007"            ~ "hh4p",
    variable=="B25009_008"            ~ "hh4p",
    variable=="B25009_009"            ~ "hh4p",
    variable=="B25009_011"            ~ "hh1",
    variable=="B25009_012"            ~ "hh2",
    variable=="B25009_013"            ~ "hh3",
    variable=="B25009_014"            ~ "hh4p",
    variable=="B25009_015"            ~ "hh4p",
    variable=="B25009_016"            ~ "hh4p",
    variable=="B25009_017"            ~ "hh4p",
    TRUE                              ~ "coding mistake"
  )) %>% 
  group_by(County,variable_recode) %>% 
  summarize(estimate=sum(estimate))

related_persons <- person %>%
  filter(relationship != "6") %>%          # Remove unrelated persons
  group_by(hh_id) %>%                      # Group by hh_id
  summarize(num_persons_related = n())  

household_survey <- hh %>% 
  left_join(.,related_persons, by="hh_id") %>% 
  select(hh_id,home_county,num_people,num_persons_related,grep("weight",names(.))) %>% 
  mutate(survey_recode=case_when(
    num_people==1                     ~ "hh1",
    num_people==2                     ~ "hh2",
    num_people==3                     ~ "hh3",
    num_people>=4                     ~ "hh4p",
  ),
  related_recode=case_when(
    num_persons_related==1            ~ "hh1",
    num_persons_related==2            ~ "hh2",
    num_persons_related==3            ~ "hh3",
    num_persons_related>=4            ~ "hh4p",
  )) %>%
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


total_hhs <- household_survey %>% 
  group_by(home_county,survey_recode) %>% 
  summarize(total=sum(hh_weight)) %>% 
  ungroup()

related_hhs <- household_survey %>% 
  group_by(home_county,related_recode) %>% 
  summarize(total=sum(hh_weight)) %>% 
  ungroup()

final <- total_hhs %>% 
  left_join(.,related_hhs,by=c("home_county","survey_recode") %>% 
  left_join(.,household_county_acs,by=c("home_county"="County","survey_recode"="variable_recode"))
