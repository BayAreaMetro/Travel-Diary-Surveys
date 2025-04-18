# Transportation_Investments_Income_Race.R
# Transportation Investments Towards Households with Low Incomes and Populations of Color

# Bring in libraries
suppressMessages(library(tidyverse))
library(tidycensus)



# Set file directories for input and output

userprofile     <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
box_dir         <- file.path(userprofile, "Box", "Modeling and Surveys","Surveys","Travel Diary Survey","Biennial Travel Diary Survey","MTC_RSG_Partner Repository")
TDSdata_dir     <- file.path(box_dir,"5.Deliverables","Task 10 - Weighting and Expansion Data Files","WeightedDataset_02212025")
output_dir      <- file.path(userprofile,"Box","Plan Bay Area 2050+","Performance and Equity","Equity Analysis","Investment Analysis")


# -----------------------------------------------------------------------
# Bring in BATS 2023 survey files, recode county name
# -----------------------------------------------------------------------


person_df      <- read.csv(file=file.path(TDSdata_dir,"person.csv")) %>% 
  select(person_id,hh_id,matches("race|ethnicity|weight",ignore.case = T))

household_df   <- read.csv(file=file.path(TDSdata_dir,"hh.csv")) %>% 
  mutate(home_county = recode(home_county,
                              "6001" = "Alameda",
                              "6013" = "Contra Costa",
                              "6041" = "Marin",
                              "6055" = "Napa",
                              "6075" = "San Francisco",
                              "6081" = "San Mateo",
                              "6085" = "Santa Clara",
                              "6095" = "Solano",
                              "6097" = "Sonoma")) %>% 
select(hh_id,home_county,matches("income",ignore.case = T))

trip_df        <- read.csv(file=file.path(TDSdata_dir,"trip.csv")) %>% 
  select(person_id,hh_id,matches("mode|weight",ignore.case = T))

# Join income and race/ethnicity to trip file
# Recode mode into roadway and transit. From the "mode_type" variable, transit is ferry and "transit". Roadway is everything else. 
# Race uses the race_imputed and ethnicity_imputed variables. If ethnicity_imputed is "hispanic" then the person is Hispanic. Otherwise, 
# they are the race_imputed and not_hispanic. 

combined <- left_join(trip_df,household_df,by="hh_id") %>% 
  left_join(.,person_df,by=c("hh_id","person_id")) %>% 
  mutate(mode_simple=case_when(
    mode_type %in% c(1:11,14,995)        ~ "roadway",
    mode_type %in% c(12:13)              ~ "transit",
    TRUE                                 ~ "miscoded"
  ),
  race_simple=if_else(ethnicity_imputed=="hispanic","hispanic",race_imputed)) %>% 
  relocate(mode_simple,.after = "mode_type") %>% 
  relocate(race_simple,.after = "race_imputed")

# Summarize total trips by income, then roadway and transit

total_income <- combined %>% 
  group_by(home_county,income_imputed) %>% 
  summarize(total=sum(trip_weight),.groups = "drop") %>% 
  pivot_wider(.,names_from = income_imputed,values_from = total) %>% 
  select("home_county", "Under $25,000", "$25,000-$49,999","$50,000-$74,999", "$75,000-$99,999",
         "$100,000-$199,999", "$200,000 or more")

# Roadway by income

roadway_income <- combined %>% 
  filter(mode_simple=="roadway") %>% 
  group_by(home_county,income_imputed) %>% 
  summarize(total=sum(trip_weight),.groups = "drop") %>% 
  pivot_wider(.,names_from = income_imputed,values_from = total) %>% 
  select("home_county", "Under $25,000", "$25,000-$49,999","$50,000-$74,999", "$75,000-$99,999",
         "$100,000-$199,999", "$200,000 or more")

# Transit by income, replace NA values with zero

transit_income <- combined %>% 
  filter(mode_simple=="transit") %>% 
  group_by(home_county,income_imputed) %>% 
  summarize(total=sum(trip_weight),.groups = "drop") %>% 
  pivot_wider(.,names_from = income_imputed,values_from = total) %>% 
  select("home_county", "Under $25,000", "$25,000-$49,999","$50,000-$74,999", "$75,000-$99,999",
         "$100,000-$199,999", "$200,000 or more") %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# Summarize total trips by race, then roadway and transit

total_race <- combined %>%
  group_by(home_county,race_simple) %>% 
  summarize(total=sum(trip_weight),.groups = "drop") %>% 
  pivot_wider(.,names_from = race_simple,values_from = total) %>% 
  select(home_county,white_nh=white, black_nh=afam,asian_pacific_nh=asian_pacific,other_nh=other,hispanic)

# Roadway by race

roadway_race <- combined %>% 
  filter(mode_simple=="roadway") %>%
  group_by(home_county,race_simple) %>% 
  summarize(total=sum(trip_weight),.groups = "drop") %>% 
  pivot_wider(.,names_from = race_simple,values_from = total) %>% 
  select(home_county,white_nh=white, black_nh=afam,asian_pacific_nh=asian_pacific,other_nh=other,hispanic)

# Transit by race, replace NA values with zero

transit_race <- combined %>% 
  filter(mode_simple=="transit") %>%
  group_by(home_county,race_simple) %>% 
  summarize(total=sum(trip_weight),.groups = "drop") %>% 
  pivot_wider(.,names_from = race_simple,values_from = total) %>% 
  select(home_county,white_nh=white, black_nh=afam,asian_pacific_nh=asian_pacific,other_nh=other,hispanic) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# Output files

write.csv(total_income,file.path(output_dir,"BATS_2023_Total_Income.csv"),row.names=F)
write.csv(roadway_income,file.path(output_dir,"BATS_2023_Roadway_Income.csv"),row.names=F)
write.csv(transit_income,file.path(output_dir,"BATS_2023_Transit_Income.csv"),row.names=F)

write.csv(total_race,file.path(output_dir,"BATS_2023_Total_Race.csv"),row.names=F)
write.csv(roadway_race,file.path(output_dir,"BATS_2023_Roadway_Race.csv"),row.names=F)
write.csv(transit_race,file.path(output_dir,"BATS_2023_Transit_Race.csv"),row.names=F)

