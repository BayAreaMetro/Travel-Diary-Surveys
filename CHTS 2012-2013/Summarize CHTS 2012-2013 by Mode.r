# Summarize CHTS 2012-2013 by Mode.r

'Drove Alone
Carpooled
Transit
Walk
Bike
Work at Home
Other'

# Library

suppressMessages(library(tidyverse))

# Link to CHTS trip file and weights

CHTS_in         <- "M:/Data/HomeInterview/2010/Data from CHTS/Processed by PB 021715/trips.csv"
CHTS_weights_in <- "M:/Data/HomeInterview/2010/Data from CHTS/Processed by PB 021715/Household_typical_weekday_weight.csv"

# Bring in data and join weights, recode mode

'Drove Alone
Carpooled
Transit
Walk
Bike
Work at Home
Other'

CHTS     <- read.csv(CHTS_in,header = T)
weight   <- read.csv(CHTS_weights_in, header = T)
combined <- left_join(CHTS,weight,by=c("HH_ID"="SAMPN")) %>% filter(!is.na(TYP_WKDY_WGT) & TRIPMODE !=0) %>% 
  mutate(mode_rc=case_when(
    TRIPMODE %in% c(1,2)                                                    ~ "1_SOV auto",
    TRIPMODE %in% c(3,4,5,6)                                                ~ "2_carpool",
    TRIPMODE %in% c(9,10,11,12,13,14,15,16,17,21,22,23,31,32,33,41,42,43)   ~ "3_transit",
    TRIPMODE==7                                                             ~ "4_walk",
    TRIPMODE==8                                                             ~ "5_bike",
    TRIPMODE %in% c(18,19,20)                                               ~ "6_other"
  ))

final <- combined %>% 
  group_by(mode_rc) %>% 
  summarize(total=sum(TYP_WKDY_WGT)) 

# Summarize by mode



