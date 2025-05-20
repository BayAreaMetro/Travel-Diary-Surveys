# Summarize_Completes_by_County_Using_5_Day_Standard.R
# Summarize completes by county for finalizing incentive payment

# Bring in libraries
suppressMessages(library(tidyverse))

# Set file directories for input and output

userprofile     <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
box_dir         <- file.path(userprofile, "Box", "Modeling and Surveys","Surveys","Travel Diary Survey","Biennial Travel Diary Survey","MTC_RSG_Partner Repository")
TDSdata_dir     <- file.path(box_dir,"5.Deliverables","Task 10 - Weighting and Expansion Data Files","WeightedDataset_02212025")


# -----------------------------------------------------------------------
# Bring in BATS 2023 household file
# -----------------------------------------------------------------------

households   <- read.csv(file=file.path(TDSdata_dir,"hh.csv")) %>% 
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
  select(1:9,home_county)

final <- households %>% 
  mutate(completion = case_when(
    diary_platform %in% c("browser","call") & num_days_complete>=1   ~ "complete",
    diary_platform == "rmove" & num_days_complete>=5                 ~ "complete",
    TRUE                                                             ~ "incomplete"
    )) %>% 
  group_by(home_county,completion) %>% 
  summarize(total=n(),.groups = "drop") %>% 
  pivot_wider(names_from = completion,values_from = total)


Print(final)