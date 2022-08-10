# TNC Survey Facility Margin of Error Calculations.r
# Summarize TNC survey data for key variables and calculate margins of error

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
#trip_linked_location  <- file.path(file_location,"trip_linked.tsv")
#hh_location           <- file.path(file_location,"hh.tsv")
#day_location          <- file.path(file_location,"day.tsv")
#location_location     <- file.path(file_location,"location.tsv")
#trip_w_other_location <- file.path(file_location,"trip_with_purpose_other.tsv")
#vehicle_location      <- file.path(file_location,"vehicle.tsv")

# Bring in TNC survey datasets
# Commented out files that may be needed for future analyses

person          <- read_tsv(person_location,col_names=TRUE)
trip            <- read_tsv(trip_location,col_names=TRUE)      
#linked_trip    <- read_tsv(trip_linked_location,col_names=TRUE)
#household      <- read_tsv(hh_location,col_names=TRUE)
#day            <- read_tsv(day_location,col_names=TRUE)
#location       <- read_tsv(location_location,col_names=TRUE)
#trip_other     <- read_tsv(trip_w_other_location,col_names=TRUE)
#vehicle        <- read_tsv(vehicle_location,col_names=TRUE)

# Bring in facility flag file (file that indicates whether a given trip traverses a given freeway)
# Create vector of facilities for analysis

facility_flag <- read.csv(file = file.path(Output,"TNC Survey Trips Per Facility.csv"))

facilities <- c("Al_SF_80_PlazaTo101", "SF_101_80ToSM", 
"SF_280_StartToSM", "SM_101_SFToSC", "SM_280_SFToSC", "SC_101_SMTo680", 
"SC_101_680ToGilroy", "SC_237_101To880", "SC_280_SMTo101", "Al_SC_680_101To580", 
"Al_SC_880_101To238", "Al_880_238ToPlaza", "Al_580_SanJoaquinTo238", 
"Al_580_238To80", "Al_80_580ToPlaza", "Al_CC_80_4To580", "CC_Al_24_680To580", 
"CC_Al_680_4To580", "CC_4_160To680", "Sol_80_YoloToCarquinez", 
"North_37_101To80", "Mar_Son_101_12To580")

# Recode linked trip file using imputed HH income and race/ethnicity from person file

person_joiner <- person %>% 
  filter(is_active_participant==1) %>%                   # Only include participants
  mutate(
    income_recoded=case_when(
      income_imputed %in% c(1,2)                         ~ "Under $50,000",
      income_imputed %in% c(3,4)                         ~ "$50,000-$99,999",
      income_imputed==5                                  ~ "$100,000-$149,999",
      income_imputed %in% c(6,7,8)                       ~ "Over $150,000",
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
  select(hh_id,person_id,income_recoded,race_recoded)

# Recoded trip purpose on linked trip file

recoded_trip <- trip %>% 
  mutate(
    purpose_recoded=case_when(
      d_purpose_category_imputed==1                      ~ "Home",
      d_purpose_category_imputed %in% c(2,3)             ~ "Work or work-related",
      d_purpose_category_imputed %in% c(4,14)            ~ "School or school-related",
      d_purpose_category_imputed==5                      ~ "Escort",
      d_purpose_category_imputed==6                      ~ "Shop",
      d_purpose_category_imputed==7                      ~ "Meal",
      d_purpose_category_imputed==8                      ~ "Social/recreation",
      d_purpose_category_imputed==9                      ~ "Errand/appointment",
      d_purpose_category_imputed==10                     ~ "Change mode",
      d_purpose_category_imputed==11                     ~ "Spent the night elsewhere",
      d_purpose_category_imputed %in% c(-1,12)           ~ "Other/missing",
      TRUE                                               ~ "Miscoded"
    )
  )

# Join trips file with facility flag file 

working <- left_join(facility_flag,recoded_trip,by=c("hh_id","person_id","trip_id")) %>% 
  left_join(.,person_joiner,by=c("hh_id","person_id"))

# Function to analyze data and calculate standard errors
# Filter for facility value==1 (i.e., traverses that facility) 
# Formula for SE of a weighted sample: 
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Standard_error_of_a_proportion_estimation_when_using_weighted_data
# Takes the form: sqrt(p(1-p)summation((weights standardized to 1)^2))
# Do separate summaries for race, trip purpose, and income, then concatenate and do additional calculations

# Start with input dataset based on desired time of day (df_tod), using depart_hour variable

all_day <- working                                 # all times of day
peak <- working %>%                                # morning and evening peak
  filter(depart_hour %in% c(6,7,8,9,15,16,17,18))
am_peak <- working %>%                             # morning peak
  filter(depart_hour %in% c(6,7,8,9))
pm_peak <- working %>%                             # evening peak
  filter(depart_hour %in% c(15,16,17,18))

# Now create function
# tod=time of day ("all_day","peak","am_peak","pm_peak")
# df_tod=data frame used for that time of day
# facility is the roadway analyzed

#calculations <- function(df_tod,facility){
 # temp_output <- data.frame()
  #temp_df <- df_tod %>% 
   # filter(.[[facility]]==1) %>% 
    #mutate(squared_standard_weights=(daywt_alladult_wkday/sum(daywt_alladult_wkday))^2)

calculations <- function(df,facility,tod){
temp_output <- data.frame()

stopifnot(tod %in% c("all_day","peak","am_peak","pm_peak"))

if (tod=="all_day"){
temp_df <- df %>% 
  filter(.[[facility]]==1) %>%
mutate(squared_standard_weights=(daywt_alladult_wkday/sum(daywt_alladult_wkday))^2)}

else if (tod=="peak"){
  temp_df <- df %>% 
    filter(.[[facility]]==1,.$depart_hour %in% c(6,7,8,9,15,16,17,18)) %>%
    mutate(squared_standard_weights=(daywt_alladult_wkday/sum(daywt_alladult_wkday))^2)}

else if (tod=="am_peak"){
  temp_df <- df %>% 
    filter(.[[facility]]==1,.$depart_hour %in% c(6,7,8,9)) %>%
    mutate(squared_standard_weights=(daywt_alladult_wkday/sum(daywt_alladult_wkday))^2)}

else if (tod=="pm_peak"){
  temp_df <- df %>% 
    filter(.[[facility]]==1,.$depart_hour %in% c(15,16,17,18)) %>%
    mutate(squared_standard_weights=(daywt_alladult_wkday/sum(daywt_alladult_wkday))^2)}

# Store value of summed squared standardized weights in a variable for later use  
  error_summation <- sum(temp_df$squared_standard_weights)
  
# Store total trips for calculating shares within each summary
  total_trips <- sum(temp_df$daywt_alladult_wkday)
  
# Summarize data by race/ethnicity

  race <- temp_df %>% 
    group_by(race_recoded) %>% 
    summarize(total=sum(daywt_alladult_wkday),share_value=sum(daywt_alladult_wkday)/total_trips) %>% 
    mutate(category="ethnicity") %>% 
    rename(metric=race_recoded) %>% 
    ungroup()
  
  purpose <- temp_df %>% 
    group_by(purpose_recoded) %>% 
    summarize(total=sum(daywt_alladult_wkday),share_value=sum(daywt_alladult_wkday)/total_trips) %>% 
    mutate(category="trip_purpose") %>% 
    rename(metric=purpose_recoded) %>% 
    ungroup()
  
  income <- temp_df %>% 
    group_by(income_recoded) %>% 
    summarize(total=sum(daywt_alladult_wkday),share_value=sum(daywt_alladult_wkday)/total_trips) %>% 
    mutate(category="income") %>% 
    rename(metric=income_recoded) %>% 
    ungroup()
  
# Calculate standard error, 90 percent confidence interval, lower and upper bound values
  
  temp_output <- bind_rows(temp_output,race,purpose,income) %>% 
    mutate(roadway=facility,
           standard_error=sqrt((share_value*(1-share_value)*error_summation)),
           ci_95=1.96*standard_error,
           lower_bound=if_else(share_value-ci_95>=0,share_value-ci_95,0),
           upper_bound=share_value+ci_95,
           time_period=tod) %>% 
    relocate(roadway,.before = metric) %>% 
    relocate(category,.after = roadway) %>% 
    relocate(time_period,.after = category)
  
  return(temp_output)
}

# Iterate function over all 22 facilities, by each time period, then bind all together

full_all_day <- purrr::map_dfr(facilities, ~{calculations(df=working,
                                                       facility = .x, 
                                                       tod = "all_day")})

full_peak <- purrr::map_dfr(facilities, ~{calculations(df=working,
                                                          facility = .x, 
                                                          tod = "peak")})

full_am_peak <- purrr::map_dfr(facilities, ~{calculations(df=working,
                                                       facility = .x, 
                                                       tod = "am_peak")})

full_pm_peak <- purrr::map_dfr(facilities, ~{calculations(df=working,
                                                          facility = .x, 
                                                          tod = "pm_peak")})

final <- bind_rows(full_all_day,full_peak,full_am_peak,full_pm_peak)


# Output file for analysis in Tableau

write.csv(final,file.path(Output,"BATS_2019_Facility_Daypart_Summary.csv"),row.names = FALSE)

