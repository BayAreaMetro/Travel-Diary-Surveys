# Investigate Trips on Freeway Segments for Mode.R

# Set options to get rid of scientific noation

options(scipen = 999)

# Bring in libraries

suppressMessages(library(tidyverse))

# Set output directory

USERPROFILE   <- gsub("////","/", Sys.getenv("USERPROFILE"))
Box_TM1       <- file.path(USERPROFILE, "Box", "Modeling and Surveys", "Surveys", "Travel Diary Survey")
Box_TM2       <- file.path(Box_TM1,"MPO Partner Household Travel Survey","TNC Work","SFCTA Map Matching")
Output        <- file.path(Box_TM2,"NextGen Freeway Analysis")

# Bring in TNC survey files
# Commented out files that may be needed for future analyses

temp                   <- "M:/Data/HomeInterview/TNC Survey/Data/Task 8 Data Refinement Version"
file_location          <- file.path(temp,"Final Updated Dataset as of 10-18-2021","RSG_HTS_Oct2021_bayarea")
person_location        <- file.path(file_location,"person.tsv")
trip_location          <- file.path(file_location,"trip.tsv")
trip_linked_location   <- file.path(file_location,"trip_linked.tsv")
hh_location            <- file.path(file_location,"hh.tsv")


# Bring in TNC survey datasets
# Commented out files that may be needed for future analyses

person          <- read_tsv(person_location,col_names=TRUE)
trip            <- read_tsv(trip_location,col_names=TRUE)      
linked_trip     <- read_tsv(trip_linked_location,col_names=TRUE)
household       <- read_tsv(hh_location,col_names=TRUE) %>% 
  select(hh_id,income_detailed)

# Bring in facility flag file (file that indicates whether a given trip traverses a given freeway)
# Create vector of facilities for analysis

facility_flag <- read.csv(file = file.path(Output,"TNC Survey Trips Per Facility.csv"))

full_facilities_list <- c("Al_SF_80_PlazaTo101", "SF_101_80ToSM", 
                "SF_280_StartToSM", "SM_101_SFToSC", "SM_280_SFToSC", "SC_101_SMTo680", 
                "SC_101_680ToGilroy", "SC_237_101To880", "SC_280_SMTo101", "Al_SC_680_101To580", 
                "Al_SC_880_101To238", "Al_880_238ToPlaza", "Al_580_SanJoaquinTo238", 
                "Al_580_238To80", "Al_80_580ToPlaza", "Al_CC_80_4To580", "CC_Al_24_680To580", 
                "CC_Al_680_4To580", "CC_4_160To680", "Sol_80_YoloToCarquinez", 
                "North_37_101To80", "Mar_Son_101_12To580","All_Freeways")

# Recode linked trip file using imputed HH income and race/ethnicity from person file
# Join with household file to append detailed_income variable
# Run discrete income function defined above to get a discrete income variable for each categorical income
# Get share of regional median income and group those values into a new ami variable
# Select significant variables of interest

person_joiner <- person %>% 
  left_join(.,household,by="hh_id") %>% 
  filter(is_active_participant==1) %>%                   # Only include participants
  mutate(
    income_recoded=case_when(
      income_imputed %in% c(1,2)                         ~ "Under $50,000",
      income_imputed %in% c(3,4)                         ~ "$50,000-$99,999",
      income_imputed %in% c(5,6)                         ~ "$100,000-$199,999",
      income_imputed %in% c(7,8)                         ~ "Over $200,000",
      TRUE                                               ~ "Miscoded"
    ),
    race_recoded=case_when(
      raceeth_new_imputed==1                             ~ "Hispanic",
      raceeth_new_imputed==2                             ~ "Black",
      raceeth_new_imputed==3                             ~ "Asian/Pacific Islander",
      raceeth_new_imputed==4                             ~ "White",
      raceeth_new_imputed %in% c(-1,5)                   ~ "Other",
      TRUE                                               ~ "Miscoded"
    )
  ) %>% 
  select(hh_id,person_id,income_recoded,race_recoded,income_detailed,income_imputed,raceeth_new_imputed)

# Recoded trip purpose on linked trip file

recoded_trip <- trip %>% 
  mutate(
    purpose_recoded=case_when(
      d_purpose_category_imputed %in% c(1,11)            ~ "Home", # Home, spent night elsewhere
      d_purpose_category_imputed %in% c(2,3)             ~ "Work", # Work or work related
      d_purpose_category_imputed %in% c(4,5,14)          ~ "School/escort", # School, related, or escort
      d_purpose_category_imputed %in% c(6,7,8)           ~ "Shop,meal,social,recreational",
      d_purpose_category_imputed==9                      ~ "Errand/appointment",
      d_purpose_category_imputed==10                     ~ "Change mode",
      d_purpose_category_imputed %in% c(-1,12)           ~ "Other/missing",
      TRUE                                               ~ "Miscoded"
    )
  )

# Join trips file with facility flag file 
# Create flag for all freeways, used later to sum trip characteristics among full freeway network

working <- left_join(facility_flag,recoded_trip,by=c("hh_id","person_id","trip_id")) %>% 
  left_join(.,person_joiner,by=c("hh_id","person_id")) %>% 
  mutate(All_Freeways=1) %>% 
  relocate(All_Freeways,.after = "Mar_Son_101_12To580")

working2 <- working |> 
  filter(mode_1==30 | mode_2==30 | mode_3==30,CC_Al_24_680To580==1 | Al_580_SanJoaquinTo238) |> 
  relocate(c(CC_Al_24_680To580,Al_580_SanJoaquinTo238), .after = All_Freeways) |> 
  mutate(
    mode_1rc = recode(mode_1,
                      "1"="walk or bike",
                      "2"="walk or bike",
                      "4"="walk or bike",
                      "6"="drive",
                      "7"="drive",
                      "16"="drive",
                      "28"="bus",
                      "30"="bart",
                      "33"="drive",
                      "34"="drive",
                      "39"="light rail",
                      "41"="intercity rail (ACE,Amtrak,Caltrain)",
                      "43"="walk or bike",
                      "46"="bus",
                      "55"="express bus",
                      "62"="employee shuttle",
                      "63"="paratransit",
                      "64"="tnc",
                      "65"="tnc",
                      "67"="bus",
                      "68"="cable car",
                      "77"="walk or bike",
                      "995"="NA",
                      "997"="other mode"),
    mode_2rc = recode(mode_2,
                      "1"="walk or bike",
                      "2"="walk or bike",
                      "4"="walk or bike",
                      "6"="drive",
                      "7"="drive",
                      "16"="drive",
                      "28"="bus",
                      "30"="bart",
                      "33"="drive",
                      "34"="drive",
                      "39"="light rail",
                      "41"="intercity rail (ACE,Amtrak,Caltrain)",
                      "43"="walk or bike",
                      "46"="bus",
                      "55"="express bus",
                      "62"="employee shuttle",
                      "63"="paratransit",
                      "64"="tnc",
                      "65"="tnc",
                      "67"="bus",
                      "68"="cable car",
                      "77"="walk or bike",
                      "995"="NA",
                      "997"="other mode"),
    mode_3rc = recode(mode_3,
                      "1"="walk or bike",
                      "2"="walk or bike",
                      "4"="walk or bike",
                      "6"="drive",
                      "7"="drive",
                      "16"="drive",
                      "28"="bus",
                      "30"="bart",
                      "33"="drive",
                      "34"="drive",
                      "39"="light rail",
                      "41"="intercity rail (ACE,Amtrak,Caltrain)",
                      "43"="walk or bike",
                      "46"="bus",
                      "55"="express bus",
                      "62"="employee shuttle",
                      "63"="paratransit",
                      "64"="tnc",
                      "65"="tnc",
                      "67"="bus",
                      "68"="cable car",
                      "77"="walk or bike",
                      "995"="NA",
                      "997"="other mode")
                  ) |> 
  relocate(c(mode_1rc,mode_2rc,mode_3rc),.before = day_num) 

# Output file for analysis in Tableau

write.csv(final,file.path(Output,"BATS_2019_Facility_Daypart_Summary.csv"),row.names = FALSE)


