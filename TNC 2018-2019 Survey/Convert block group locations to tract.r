# Convert block group locations to tract.r
# July 12, 2021

suppressMessages(library(tidyverse))
library(readxl)

input <- "C:/Users/sisrael/Box/Modeling and Surveys/Share Data/Protected Data/TNC2019-Survey/UC-Berkeley-Deliverable/ex_hh.xlsx"

household <- read_excel(input, col_names = T) %>% 
  mutate(home_tract_geoid = substr(home_bg_geoid,1,9))

tract_summary <- household %>% 
  group_by(home_tract_geoid) %>% 
  summarize(households=n()) 

bg_summary <- household %>% 
  group_by(home_bg_geoid) %>% 
  summarize(households=n())