# Transportation_Investments_Income_Race.R
# Transportation Investments Towards Households with Low Incomes and Populations of Color

# Bring in libraries
suppressMessages(library(tidyverse))
library(tidycensus)



# Set file directories for input and output

userprofile     <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
box_dir         <- file.path(userprofile, "Box", "Modeling and Surveys","Surveys","Travel Diary Survey","Biennial Travel Diary Survey","MTC_RSG_Partner Repository")
TDSdata_dir     <- file.path(box_dir,"5.Deliverables","Task 10 - Weighting and Expansion Data Files","WeightedDataset_02212025")
output_dir      <- file.path("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data","Full Weighted 2023 Dataset","WeightedDataset_02212025")


# -----------------------------------------------------------------------
# Bring in BATS 2023 survey files, recode county name
# -----------------------------------------------------------------------


person_df      <- read.csv(file=file.path(TDSdata_dir,"person.csv")) %>% 
  select(hh_id,person_id,person_weight,person_weight_rmove_only)
household_df   <- read.csv(file=file.path(TDSdata_dir,"hh.csv")) %>% 
  select(hh_id,home_county) %>% 
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
trip_df        <- read.csv(file=file.path(TDSdata_dir,"trip.csv")) 

# Append poverty status, sum person weights by household poverty status and county

derived        <- read.csv(file=file.path(impute_dir,"BATShh_ImputedIncomeValues.csv")) %>% 
  select(hh_id,poverty_status)

combined <- left_join(derived,household_df,by="hh_id") 

person_combined <- left_join(person_df,combined,by="hh_id") %>% 
  mutate(person_weight_rmove_only=if_else(is.na(person_weight_rmove_only),0,person_weight_rmove_only))

bats_cleaned <- person_combined %>% 
  group_by(home_county,poverty_status) %>% 
  summarize(Total_Base=sum(person_weight),Total_rMove=sum(person_weight_rmove_only))

bats_cleaned_base <- bats_cleaned %>% 
  select(-Total_rMove) %>% 
  pivot_wider(names_from=poverty_status,values_from=Total_Base) %>% 
  select(County=home_county,under_2x_poverty,over_2x_poverty) %>% 
  mutate(Total=under_2x_poverty+over_2x_poverty,Source="BATS_Base") %>% 
  ungroup()

bats_cleaned_rmove <- bats_cleaned %>% 
  select(-Total_Base) %>% 
  pivot_wider(names_from=poverty_status,values_from=Total_rMove) %>% 
  select(County=home_county,under_2x_poverty,over_2x_poverty) %>% 
  mutate(Total=under_2x_poverty+over_2x_poverty,Source="BATS_rMove") %>% 
  ungroup()

bay_bats_base <- bats_cleaned_base %>% 
  summarize(County="Bay Area", under_2x_poverty=sum(under_2x_poverty),over_2x_poverty=sum(over_2x_poverty),
            Total=sum(Total), Source="BATS_Base")

bay_bats_rmove <- bats_cleaned_rmove %>% 
  summarize(County="Bay Area", under_2x_poverty=sum(under_2x_poverty),over_2x_poverty=sum(over_2x_poverty),
            Total=sum(Total), Source="BATS_rMove")

marin_napa_bats_base <- bats_cleaned_base %>%
  filter(County %in% c("Marin","Napa")) %>% 
  summarize(County="Marin_Napa", under_2x_poverty=sum(under_2x_poverty),over_2x_poverty=sum(over_2x_poverty),
            Total=sum(Total), Source="BATS_Base")

marin_napa_bats_rmove <- bats_cleaned_rmove %>%
  filter(County %in% c("Marin","Napa")) %>% 
  summarize(County="Marin_Napa", under_2x_poverty=sum(under_2x_poverty),over_2x_poverty=sum(over_2x_poverty),
            Total=sum(Total), Source="BATS_rMove")

combined_bats <- bind_rows(bats_cleaned_base,marin_napa_bats_base,bay_bats_base,bats_cleaned_rmove,marin_napa_bats_rmove,bay_bats_rmove)

share_bats <- combined_bats %>% 
  mutate(under_2x_share=under_2x_poverty/Total) %>% 
  select(County,under_2x_share,Source)

# Now download ACS 2023 poverty data

bay_counties <- c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo",
                  "Santa Clara", "Solano","Sonoma")

acs_poverty_2023 <- get_acs(
  geography = "county",
  table = "C17002",
  year = 2023,  
  state = "CA",  
  county = bay_counties,
  survey = "acs1",
  output = "wide"
)

# Remove MOE values

acs_cleaned <- acs_poverty_2023 %>% 
  mutate(NAME = str_remove(NAME, " County, California$"),
         under_2x_poverty=C17002_001E-C17002_008E,
         over_2x_poverty=C17002_008E,
         Source="ACS") %>% 
  rename(County=NAME, Total=C17002_001E) %>% 
  select(-matches("_00[1-8]"),-GEOID) %>% 
  relocate(Total,.after = over_2x_poverty)

bay_acs <- acs_cleaned %>% 
  summarize(County="Bay Area", under_2x_poverty=sum(under_2x_poverty),over_2x_poverty=sum(over_2x_poverty),
            Total=sum(Total), Source="ACS")

marin_napa_acs <- acs_cleaned %>%
  filter(County %in% c("Marin","Napa")) %>% 
  summarize(County="Marin_Napa", under_2x_poverty=sum(under_2x_poverty),over_2x_poverty=sum(over_2x_poverty),
            Total=sum(Total), Source="ACS")

combined_acs <- bind_rows(acs_cleaned,marin_napa_acs,bay_acs)

share_acs <- combined_acs %>% 
  mutate(under_2x_share=under_2x_poverty/Total) %>% 
  select(County,under_2x_share,Source)

combined_data <- bind_rows(share_bats,share_acs)
  
# Output files

write.csv(combined_data,file=file.path(output_dir,"BATS_2023_Poverty_Summary.csv"),row.names=F)
write.csv(combined_bats,file=file.path(output_dir,"BATS_2023_Poverty_Totals.csv"),row.names=F)
