# BATS_2023_Survey_Facility_Margin_of_Error_Calculations_Express_Lanes_hhs.r
# Summarize BATS 2023 survey data shares for poverty and calculate margins of error
# Remove all PII 

# Set options to get rid of scientific notation

options(scipen = 999)

# Bring in libraries

suppressMessages(library(tidyverse))

# Set file directories for input and output

BOX_dir1       <- file.path("E:", "Box", "Modeling and Surveys","Surveys","Travel Diary Survey")
Box_dir2       <- file.path(BOX_dir1,"BATS_2023","Data","2023")
conflation_loc <- file.path(Box_dir2,"Survey Conflation")
data_loc       <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset/WeightedDataset_02212025"
output         <- file.path(data_loc,"Summaries")

# Bring in BATS 2023 survey files
# Select only the variables needed for this script
# Bring in derived variables (use only poverty_status)
# Join with HH file

trip           <- read.csv(file=file.path(data_loc,"trip.csv")) 
derived_vars   <- read.csv(file=file.path(data_loc,"derived_variables","BATShh_ImputedIncomeValues.csv"))%>% 
  select(hh_id,poverty_status)
household      <- read.csv(file=file.path(data_loc,"hh.csv"))%>% 
  select(hh_id,grep("weight",names(.))) %>% 
  left_join(.,derived_vars,by="hh_id")

# Bring in facility flag file (file that indicates whether a trip traverses a given freeway)
# Create vector of facilities (full_facilities_list) for passing into a later function
# Limit analyses to only what's needed for express lanes calculation

facility_flag <- read.csv(file = file.path(conflation_loc,"BATS 2023 Facility Use Booleans Toll.csv"))

full_facilities_list <- c("i880_baybridge_to_237", "i880_baybridge_to_237_exp","i680_bm_bridge_to_580", 
                          "i680_bm_bridge_to_580_exp","i80_680_to_505")

# Join trip file with facility flag file 
# Retain only important variables
# Create flag for all freeways included in this script
# Remove trips that don't have rmove weights

working <- left_join(facility_flag,trip %>% select(trip_id,hh_id,depart_hour,trip_weight_rmove_only),by="trip_id") %>% 
  filter(trip_weight_rmove_only>0)

# Function to analyze data and calculate standard errors
# Filter for facility value==1 (i.e., traverses that specific freeway/bridge) 
# Formula for SE of a weighted sample: 
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Standard_error_of_a_proportion_estimation_when_using_weighted_data
# Takes the form: sqrt(p(1-p)summation((weights standardized to 1)^2))
# Summarize poverty_status variable
# Use operating period for express lanes (5a-8p)

AM peak period, 6 am to 10 am; PM peak period - 3 pm to 7 pm 
exp_operating <- c(05:20)        # All day: 5 AM - 8 PM
#exp_operating <- c(6:9, 15:18)  # Peak: 6-9 AM and 3-6 PM

# Now create function for iterating by facility analyzed and time of day
# The variable squared_standard_weights is saved to later calculate standard error of a weighted sample, as referenced above. 
# Weights for the subsetted facility need to be standardized to sum to 1 and then squared - done below

calculations <- function(df,facility,tod){       # Beginning of function
temp_output <- data.frame()

stopifnot(tod %in% c("exp_operating"))

if (tod == "exp_operating") {
  temp_df <- df %>%
    filter(.data[[facility]] == 1, .data$depart_hour %in% exp_operating) %>%
    group_by(hh_id) %>%
    summarize(tot_trips = sum(.data[[facility]])) %>%
    ungroup() %>% 
    filter(tot_trips > 0) %>%
    left_join(., household, by = "hh_id") %>%
    mutate(squared_standard_weights = (hh_weight_rmove_only / sum(hh_weight_rmove_only))^2)
}

# Store value of summed squared standardized weights in a variable for later use  
  error_summation <- sum(temp_df$squared_standard_weights)
  
# Store total hhs for calculating shares within each summary
  total_hhs <- sum(temp_df$hh_weight_rmove_only)
  
# Summarize data by poverty_status

  poverty <- temp_df %>% 
    group_by(poverty_status) %>% 
    summarize(count=n(),total=sum(hh_weight_rmove_only),share_value=sum(hh_weight_rmove_only)/total_hhs) %>% 
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
  
  temp_output <- poverty %>% 
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

# Iterate function over the relevant facilities, for the express lane operating period, then bind all together

temp <- purrr::map_dfr(full_facilities_list, ~{calculations(df=working,
                                                       facility = .x, 
                                                       tod = "exp_operating")})

# Rename "count" and "total" variables in final dataset to "unweighted" and "weighted" (more intuitive)

final <- temp %>% 
  rename(unweighted_hhs=count,weighted_hhs=total) %>% 
  mutate(weighted_hhs=round(weighted_hhs))

# Output file to CSV

write.csv(final,file=file.path(output,"BATS_2023_poverty_express_lanes.csv"),row.names=F)



