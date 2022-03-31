# TNC Survey Unique Blocks and Block Groups for PII.r
# Summarize number of surveyed HHs in each block group and block to determine if aggregation passes PII concerns
# SI

# Set working directory

wd <- "M:/Data/Requests/Matt Lavrinets/"
setwd(wd)

# Import libraries

suppressMessages(library(tidyverse))
library(readxl)

# Set up input and output directories

username      <- Sys.getenv("USERNAME")
Box_location  <- paste0("C:/Users/",username,"/Box/Modeling and Surveys/Share Data/Protected Data/")
File_location <- paste0(Box_location,"TNC2019-Survey/UC-Davis-Deliverable/ex_hh.xlsx")
household     <- read_excel (File_location) %>% select(-"...1") # Remove first column data processing index

# Summarize auto sufficiency for Big 7, weekday only

Tract <- household %>%
  mutate(home_tract_geoid=substr(home_bg_geoid,1,11)) %>% 
  group_by(home_tract_geoid, home_county_fips) %>% 
  summarize(Number_Households=n()) %>% 
  mutate(
    County_Name=recode(home_county_fips,"001"="Alameda", "013"="Contra Costa",
                       "041"="Marin","055"="Napa", "075"="San Francisco", "081"="San Mateo",
                       "085"="Santa Clara", "095"="Solano","097"="Sonoma")) %>% 
  select(-home_county_fips)


BG <- household %>% 
  group_by(home_bg_geoid, home_county_fips) %>% 
  summarize(Number_Households=n()) %>% 
  mutate(
    County_Name=recode(home_county_fips,"001"="Alameda", "013"="Contra Costa",
                       "041"="Marin","055"="Napa", "075"="San Francisco", "081"="San Mateo",
                       "085"="Santa Clara", "095"="Solano","097"="Sonoma")) %>% 
  select(-home_county_fips)

Block <- household %>% 
  group_by(home_block_geo_id, home_county_fips) %>% 
  summarize(Number_Households=n()) %>% 
  mutate(
    County_Name=recode(home_county_fips,"001"="Alameda", "013"="Contra Costa",
                       "041"="Marin","055"="Napa", "075"="San Francisco", "081"="San Mateo",
                       "085"="Santa Clara", "095"="Solano","097"="Sonoma")) %>% 
  select(-home_county_fips)


# Write out final CSV files

write.csv(Tract,"TNC Survey Tract.csv",row.names = FALSE,quote=T)
write.csv(BG,"TNC Survey Block Group.csv",row.names = FALSE,quote=T)
write.csv(Block,"TNC Survey Block.csv",row.names = FALSE,quote=T)




