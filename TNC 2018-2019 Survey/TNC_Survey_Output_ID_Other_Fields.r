# TNC_Survey_Output_ID_Other_Fields.r
# For SF Practicum LLM project, output ID field with "other" write-in fields

# Remove scientific notation

options(scipen = 999)

# Set data and output directories

data_folder        <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations/Final Updated Dataset as of 4-1-2021/RSG_HTS_February2021_bayarea"
userprofile        <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
output_folder      <- file.path(userprofile,"Box"," USF Data Science Practicum 2024-25","Projects","Project 4- Travel Survey Free Response Classification","Data")

 
# Bring in library

suppressMessages(library(tidyverse))

# Set up working directory

day_location          <- file.path(data_folder,"day.tsv")
hh_location           <- file.path(data_folder,"hh.tsv")
person_location       <- file.path(data_folder,"person.tsv")
trip_location         <- file.path(data_folder,"trip.tsv")
trip_other_location   <- file.path(data_folder,"trip_with_purpose_other.tsv")
vehicle_location      <- file.path(data_folder,"vehicle.tsv")


# Bring in datasets

day            <- read_tsv(day_location,col_names=TRUE)
household      <- read_tsv(hh_location,col_names=TRUE)
person         <- read_tsv(person_location,col_names=TRUE)
trip           <- read_tsv(trip_location,col_names=TRUE)
trip_other     <- read_tsv(trip_other_location,col_names=TRUE)
vehicle        <- read_tsv(vehicle_location,col_names=TRUE)

# Select out versions with just IDs and "other" variables

day_w_other <- day %>% select(person_id,hh_id,grep("other",names(.),ignore.case = T))
hh_w_other  <- household %>% select(hh_id,grep("other",names(.),ignore.case = T))
person_w_other <- person %>% select(person_id,hh_id,grep("other",names(.),ignore.case = T))
trip_w_other <- trip %>% select(person_id,trip_id, hh_id,grep("other",names(.),ignore.case = T))
trip_other_w_other <- trip_other %>% select(person_id,trip_id, hh_id,grep("other",names(.),ignore.case = T))
vehicle_other <- vehicle %>% select(vehicle_id,hh_id,grep("other",names(.),ignore.case = T))

# Only keep the trip_other_w_other file and filter for records with responses

final <- trip_other_w_other %>% 
  filter(!is.na(o_purpose_other) | !is.na(d_purpose_other))
 
# Output recoded files

write.csv(final,file.path(output_folder,"BATS_2019_Other_Trip_Purposes.csv"),row.names = FALSE)

