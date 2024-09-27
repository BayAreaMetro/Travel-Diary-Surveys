# Validate_ACS_2023_Poverty_Against_BATS_2023.r
# Compare ACS 2023 persons under 2x poverty vs. BATS 2023

# Bring in libraries
suppressMessages(library(tidyverse))


# Set file directories for input and output

TDSdata_dir     <- file.path("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data","Full Weighted 2023 Dataset","WeightedDataset_09112024")
output_dir      <- file.path("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data","Full Weighted 2023 Dataset","WeightedDataset_09112024")


# -----------------------------------------------------------------------
# Bring in BATS 2023 survey files, recode county name
# -----------------------------------------------------------------------


person_df      <- read.csv(file=file.path(TDSdata_dir,"person.csv")) %>% 
  select(hh_id,person_id,person_weight)
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

# Append poverty status, sum person weights by household poverty status and county

derived        <- read.csv(file=file.path(TDSdata_dir,"derived_variables","BATShh_ImputedIncomeValues.csv")) %>% 
  select(hh_id,poverty_status)

combined <- left_join(derived,household_df,by="hh_id") 

person_combined <- left_join(person_df,combined,by="hh_id")

final <- person_combined %>% 
  group_by(home_county,poverty_status) %>% 
  summarize(Total=sum(person_weight)) %>% 
  pivot_wider(names_from=poverty_status,values_from=Total) %>% 
  select(County=home_county,under_2x_poverty,over_2x_poverty) %>% 
  mutate(Total=under_2x_poverty+over_2x_poverty)

# Output file

write.csv(final,file=file.path(output_dir,"BAT2023_SepDeliverable_Review","BATS_2023_Poverty_Summary.csv"),row.names=F)
