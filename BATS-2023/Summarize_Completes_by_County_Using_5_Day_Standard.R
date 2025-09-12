# Summarize_Completes_by_County_Using_5_Day_Standard.R
# Summarize completes by county for finalizing incentive payment
# Perform for both weighted (2/2025) and full unweighted (6/2025) datasets

# Bring in libraries
suppressMessages(library(tidyverse))

# Set file directories for input and output

userprofile              <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
box_dir                  <- file.path(userprofile, "Box", "Modeling and Surveys","Surveys","Travel Diary Survey","BATS_2023","MTC_RSG_Partner Repository")
weighted_TDSdata_dir     <- file.path(box_dir,"5.Deliverables","Task 10 - Weighting and Expansion Data Files","WeightedDataset_02212025")
unweighted_TDSdata_dir   <- file.path(box_dir,"5.Deliverables","Task 10 - Weighting and Expansion Data Files","UnweightedDataset")


# -----------------------------------------------------------------------
# Bring in weighted BATS 2023 household file
# -----------------------------------------------------------------------

weighted_households   <- read.csv(file=file.path(weighted_TDSdata_dir,"hh.csv")) %>% 
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

weighted_temp <- weighted_households %>% 
  mutate(completion = case_when(
    diary_platform %in% c("browser","call") & num_days_complete>=1   ~ "complete",
    diary_platform == "rmove" & num_days_complete>=5                 ~ "complete",
    TRUE                                                             ~ "incomplete"
    )) 

weighted_final <- weighted_temp %>% 
  group_by(home_county,completion) %>% 
  summarize(total=n(),.groups = "drop") %>% 
  pivot_wider(names_from = completion,values_from = total)

weighted_final_bay <- weighted_temp %>% 
  group_by(completion) %>% 
  summarize(total=n(),.groups = "drop") %>% 
  pivot_wider(names_from = completion,values_from = total) %>%
  mutate(home_county="Bay Area") 

weighted_final <- bind_rows(weighted_final,weighted_final_bay)

weighted_incomplete <- weighted_temp %>% 
  filter(completion=="incomplete") %>% 
  summarize(total_remainder_days=sum(num_days_complete),.groups = "drop")


print(weighted_final)
print(weighted_incomplete)

# -----------------------------------------------------------------------
# Bring in unweighted BATS 2023 household file and do the same analysis
# -----------------------------------------------------------------------

unweighted_households   <- read.csv(file=file.path(unweighted_TDSdata_dir,"hh.csv")) %>% 
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

unweighted_temp <- unweighted_households %>% 
  mutate(completion = case_when(
    diary_platform %in% c("browser","call") & num_days_complete>=1   ~ "complete",
    diary_platform == "rmove" & num_days_complete>=5                 ~ "complete",
    TRUE                                                             ~ "incomplete"
  )) 

unweighted_final <- unweighted_temp %>% 
  group_by(home_county,completion) %>% 
  summarize(total=n(),.groups = "drop") %>% 
  pivot_wider(names_from = completion,values_from = total)

unweighted_final_bay <- unweighted_temp %>% 
  group_by(completion) %>% 
  summarize(total=n(),.groups = "drop") %>% 
  pivot_wider(names_from = completion,values_from = total) %>%
  mutate(home_county="Bay Area") 

unweighted_final <- bind_rows(unweighted_final,unweighted_final_bay)

unweighted_incomplete <- unweighted_temp %>% 
  filter(completion=="incomplete") %>% 
  summarize(total_remainder_days=sum(num_days_complete),.groups = "drop")


print(unweighted_final)
print(unweighted_incomplete)


