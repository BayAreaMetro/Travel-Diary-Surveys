# BATS_2023_Survey_Facility_Margin_of_Error_Calculations_Express_Lanes_hhs.r
# Summarize BATS 2023 survey data shares for key variables and calculate margins of error
# Remove all PII 

# Set options to get rid of scientific notation

options(scipen = 999)

# Bring in libraries

suppressMessages(library(tidyverse))
library(sf)
library(spatstat)

# Set file directories for input and output

USERPROFILE    <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
BOX_dir1       <- file.path(USERPROFILE, "Box", "Modeling and Surveys","Surveys","Travel Diary Survey")
Box_dir2       <- file.path(BOX_dir1,"Biennial Travel Diary Survey","Data","2023")
conflation_loc <- file.path(Box_dir2,"Survey Conflation")
data_loc       <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset/WeightedDataset_09112024"
output         <- file.path(data_loc,"Summaries")

# Bring in BATS 2023 survey files
# Select only the variables needed for this script
# Bring in derived variables (use only poverty_status)
# Join with HH file

person         <- read.csv(file=file.path(data_loc,"person.csv")) %>% 
  select(hh_id,person_id,relationship,grep("race|ethnicity|weight",names(.)))    
trip           <- read.csv(file=file.path(data_loc,"trip.csv")) 
derived_vars   <- read.csv(file=file.path(data_loc,"derived_variables","BATShh_ImputedIncomeValues.csv"))%>% 
  select(hh_id,poverty_status)
household      <- read.csv(file=file.path(data_loc,"hh.csv"))%>% 
  select(hh_id,grep("weight",names(.))) %>% 
  left_join(.,derived_vars,by="hh_id")

# Bring in facility flag file (file that indicates whether a trip traverses a given freeway/bridge)
# Create vector of facilities (full_facilities_list) for passing into a later function
# Limit analyses to only what's needed for express lanes calculation

facility_flag <- read.csv(file = file.path(conflation_loc,"BATS 2023 Facility Use Booleans Toll.csv"))

full_facilities_list <- c("i880_baybridge_to_237", "i880_baybridge_to_237_exp","i680_bm_bridge_to_580", 
                          "i680_bm_bridge_to_580_exp","i80_680_to_505")

# Join trip file with facility flag file 
# Create flag for all freeways included in this script, used later to sum trip characteristics among full study network (though not inclusive of all Bay Area freeways)
# Remove trips that don't have rmove weights

working <- left_join(facility_flag,trip %>% select(trip_id,hh_id,trip_weight_rmove_only),by="trip_id") %>% 
  filter(trip_weight_rmove_only>0)

# Function to analyze data and calculate standard errors
# Filter for facility value==1 (i.e., traverses that specific freeway/bridge) 
# Formula for SE of a weighted sample: 
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Standard_error_of_a_proportion_estimation_when_using_weighted_data
# Takes the form: sqrt(p(1-p)summation((weights standardized to 1)^2))
# Do separate summaries for race, trip purpose, poverty, and AMI, then append records to database

# Define tod=time of day using departure hour ("all_day","peak","am_peak","pm_peak","off_peak)
# "all_day" is by definition all day and therefore not a subset/filtering of data
# Note that AM peak here varies from the modeling time periods and is specified for the RSR project

peak <- c(5:9,15:18)
am_peak <- c(5:9)
pm_peak <- c(15:18)
off_peak <- c(0:4,10:14,19:23)

# Now create function for iterating by facility analyzed and time of day
# The variagle squared_standard_weights is saved to later calculate standard error of a weighted sample, as referenced above. 
# Weights for the subsetted facility need to be standardized to sum to 1 and then squared - done below

calculations <- function(df,facility,tod){       # Beginning of function
temp_output <- data.frame()

stopifnot(tod %in% c("all_day","peak","am_peak","pm_peak","off_peak"))

if (tod=="all_day"){
temp_df <- df %>% 
  filter(.[[facility]]==1) %>%
mutate(squared_standard_weights=(trip_weight_rmove_only/sum(trip_weight_rmove_only))^2)}

if (tod=="peak"){
  temp_df <- df %>% 
    filter(.[[facility]]==1,.$depart_hour %in% peak) %>%
    mutate(squared_standard_weights=(trip_weight_rmove_only/sum(trip_weight_rmove_only))^2)}

if (tod=="am_peak"){
  temp_df <- df %>% 
    filter(.[[facility]]==1,.$depart_hour %in% am_peak) %>%
    mutate(squared_standard_weights=(trip_weight_rmove_only/sum(trip_weight_rmove_only))^2)}

if (tod=="pm_peak"){
  temp_df <- df %>% 
    filter(.[[facility]]==1,.$depart_hour %in% pm_peak) %>%
    mutate(squared_standard_weights=(trip_weight_rmove_only/sum(trip_weight_rmove_only))^2)}

if (tod=="off_peak"){
  temp_df <- df %>% 
    filter(.[[facility]]==1,.$depart_hour %in% off_peak) %>%
    mutate(squared_standard_weights=(trip_weight_rmove_only/sum(trip_weight_rmove_only))^2)}

# Store value of summed squared standardized weights in a variable for later use  
  error_summation <- sum(temp_df$squared_standard_weights)
  
# Store total trips for calculating shares within each summary
  total_trips <- sum(temp_df$trip_weight_rmove_only)
  
# Summarize data by race/ethnicity, trip purpose, and income

  race <- temp_df %>% 
    group_by(race_recoded) %>% 
    summarize(count=n(),total=sum(trip_weight_rmove_only),share_value=sum(trip_weight_rmove_only)/total_trips) %>% 
    mutate(category="ethnicity") %>% 
    rename(metric=race_recoded) %>% 
    ungroup()
  
  purpose <- temp_df %>% 
    group_by(purpose_recoded) %>% 
    summarize(count=n(),total=sum(trip_weight_rmove_only),share_value=sum(trip_weight_rmove_only)/total_trips) %>% 
    mutate(category="trip_purpose") %>% 
    rename(metric=purpose_recoded) %>% 
    ungroup()
  
# Commenting out below paragraph that summarizes output data by income categories (using the poverty thresholds in lieu for this script)
#  income <- temp_df %>% 
#    group_by(income_recoded) %>% 
#    summarize(count=n(),total=sum(trip_weight_rmove_only),share_value=sum(trip_weight_rmove_only)/total_trips) %>% 
#    mutate(category="income") %>% 
#    rename(metric=income_recoded) %>% 
#    ungroup()
  
  income_ami <- temp_df %>% 
    group_by(ami_recoded) %>% 
    summarize(count=n(),total=sum(trip_weight_rmove_only),share_value=sum(trip_weight_rmove_only)/total_trips) %>% 
    mutate(category="ami_income") %>% 
    rename(metric=ami_recoded) %>% 
    ungroup()
  
  poverty <- temp_df %>% 
    group_by(poverty_status) %>% 
    summarize(count=n(),total=sum(trip_weight_rmove_only),share_value=sum(trip_weight_rmove_only)/total_trips) %>% 
    mutate(category="poverty_status") %>% 
    rename(metric=poverty_status) %>% 
    ungroup()
  
# Calculate standard error, 95 percent confidence interval, lower and upper bound values
# CV reliability
# U.S. Census case studies:
# • High reliability: CVs less than 15%
# • Medium Reliability: CVs between 15‐30% ‐ be careful
# • Low Reliability: CVs over 30% ‐ use with extreme caution  
# Page 2,http://sites.tufts.edu/gis/files/2013/11/Amercian-Community-Survey_Margin-of-error-tutorial.pdf
  
  temp_output <- bind_rows(temp_output,race,purpose,income_ami,poverty) %>% 
    mutate(roadway=facility,
           standard_error=sqrt((share_value*(1-share_value)*error_summation)),
           ci_95=1.96*standard_error,
           lower_bound=if_else(share_value-ci_95>=0,share_value-ci_95,0),     # Lower bound bottom codes at 0
           upper_bound=share_value+ci_95,
           range=upper_bound-lower_bound,
           cv=(standard_error/share_value)*100,  
           est_reliability=case_when(
             is.nan(cv)       ~ "Low",          # When the share value is zero (usually due to no n)
             cv<15            ~ "High",
             cv>=15 & cv <30  ~ "Medium",
             cv>=30           ~ "Low",
             TRUE             ~ "Miscoded"),
           time_period=tod) %>% 
    relocate(roadway,.before = metric) %>% 
    relocate(category,.after = roadway) %>% 
    relocate(time_period,.after = category)
  
  return(temp_output)
}                                               # End of function

# Iterate function over all 22 facilities, by each time period, then bind all together

full_all_day <- purrr::map_dfr(full_facilities_list, ~{calculations(df=working,
                                                       facility = .x, 
                                                       tod = "all_day")})

full_peak <- purrr::map_dfr(full_facilities_list, ~{calculations(df=working,
                                                          facility = .x, 
                                                          tod = "peak")})

full_am_peak <- purrr::map_dfr(full_facilities_list, ~{calculations(df=working,
                                                       facility = .x, 
                                                       tod = "am_peak")})

full_pm_peak <- purrr::map_dfr(full_facilities_list, ~{calculations(df=working,
                                                          facility = .x, 
                                                          tod = "pm_peak")})

full_off_peak <- purrr::map_dfr(full_facilities_list, ~{calculations(df=working,
                                                          facility = .x, 
                                                          tod = "off_peak")})

temp <- bind_rows(full_all_day,full_peak,full_am_peak,full_pm_peak,full_off_peak)

# Join PUMS household/person shares of AMI, race, and poverty to compare population shares vs. people driving on freeways/bridges
# Rename "count" and "total" variables in final dataset to "unweighted" and "weighted" (more intuitive)
# Populate missing fields with NA so all variables will bind neatly with trip summary

ami_joiner <- bay_income_med %>% 
  mutate(roadway="Bay_Area_Population_2022", category="ami_income",time_period="Bay Area Population",
         standard_error=NA_real_,ci_95=NA_real_,lower_bound=NA_real_,upper_bound=NA_real_,
         range=NA_real_,cv=NA_real_,est_reliability=NA_character_) %>% 
  rename(metric=ami_recoded, share_value=PUMS_household_share) 

race_joiner <- race22 %>% 
  mutate(roadway="Bay_Area_Population_2022", category="ethnicity",time_period="Bay Area Population",
         standard_error=NA_real_,ci_95=NA_real_,lower_bound=NA_real_,upper_bound=NA_real_,
         range=NA_real_,cv=NA_real_,est_reliability=NA_character_) %>% 
  rename(metric=race_recoded, share_value=PUMS_race_share) 

poverty_joiner <- bay_poverty %>% 
  mutate(roadway="Bay_Area_Population_2022", category="poverty_status",time_period="Bay Area Population",
         standard_error=NA_real_,ci_95=NA_real_,lower_bound=NA_real_,upper_bound=NA_real_,
         range=NA_real_,cv=NA_real_,est_reliability=NA_character_) %>% 
  rename(metric=poverty_status, share_value=PUMS_poverty_share) 

final <- bind_rows(temp,ami_joiner,race_joiner,poverty_joiner) %>% 
  rename(unweighted=count,weighted=total)

# Output file to CSV

write.csv(final,file=file.path(output,"BATS_2023_race_AMI_poverty_purpose_shares.csv"),row.names=F)









