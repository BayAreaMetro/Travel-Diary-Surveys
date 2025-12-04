library(dplyr)

# Read 2019 linked trip file
# Note that the 2019 file is space delimited
LinkedTrips2019_df <- read.table("E:/Box/Modeling and Surveys/Surveys/Travel Diary Survey/MPO Partner Household Travel Survey (Includes 2018_2019 TNC Survey)/Bay Area Travel Study 2018-2019/Data from 2018_2019 Survey/BATS2018_2019_SFCTA_processed/20200228/survey2018_tripx.dat",
                                  header = TRUE,
                                  sep = "") %>% 
  mutate(survey_cycle = 2019)

# Label the mode variable
LinkedTrips2019_df <- LinkedTrips2019_df %>%
  mutate(mode_label = case_when(
    mode == 0 ~ "OTHER",
    mode == 1 ~ "WALK",
    mode == 2 ~ "BIKE",
    mode == 3 ~ "DA",
    mode == 4 ~ "HOV2",
    mode == 5 ~ "HOV3",
    mode == 6 ~ "WALKTRAN",
    mode == 7 ~ "DRIVETRAN",
    mode == 8 ~ "SCHBUS",
    mode == 9 ~ "TNC",
    TRUE ~ NA_character_
  ))

weighted_trips_by_mode <- LinkedTrips2019_df %>%
  group_by(mode_label) %>%
  summarise(
    weighted_trips = sum(trexpfac),
    unweighted_trips = n()
  )

print(weighted_trips_by_mode)

