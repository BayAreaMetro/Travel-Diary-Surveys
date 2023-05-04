# TNC Survey Facility Margin of Error Calculations_All Facilities.r
# Summarize TNC survey data for key variables and calculate margins of error

# Set options to get rid of scientific noation

options(scipen = 999)

# Bring in libraries

suppressMessages(library(tidyverse))
library(sf)
library(spatstat)

# Set output directory

USERPROFILE   <- gsub("////","/", Sys.getenv("USERPROFILE"))
Box_TM1       <- file.path(USERPROFILE, "Box", "Modeling and Surveys", "Surveys", "Travel Diary Survey")
Box_TM2       <- file.path(Box_TM1,"MPO Partner Household Travel Survey","TNC Work","SFCTA Map Matching")
Output        <- file.path(Box_TM2,"NextGen Freeway Analysis")
OSM_Path      <- "M:/Data/HomeInterview/TNC Survey/SFCTA Map Matching/NextGen Freeway Project"

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
  select(hh_id,income_detailed,reported_home_lat,reported_home_lon)

# Create vector for car modes to ensure people using freeway segment are in a car for at least one mode
# Broadly-defined car modes, including bus (but no local bus nor rail)

car_vector <- c(6:22,24:28,33:36,38,47,55,59:60,62:66,76)

# Bring in freeway coordinates for placing pie charts on map
# File also has long name equivalencies for facilities and Bay Area geographical designations

freeway_coords_in <- file.path(Output,"Freeway Facility Coordinates.csv")
freeway_coords <- read.csv(freeway_coords_in)

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

# Function for imputing discrete value from PUMS distribution
# Start by bringing in 2015-2019 PUMS data

HH_RDATA = "M:/Data/Census/PUMS/PUMS 2015-19/hbayarea1519.Rdata"
load (HH_RDATA)

# Adjust income to inflation-correct values for 2019
# Remove group quarters and vacant housing

bay_income <- hbayarea1519  %>% 
  mutate(adjustment = ADJINC/1000000,
         income=HINCP*adjustment)  %>%  
  filter(!is.na(income))  

# Calculate area median income

bay_median <- weighted.median(x=bay_income$income,w=bay_income$WGTP)

# Calculate share of PUMS households by percentage AMI

bay_income_med <- bay_income  %>%  
mutate(
  ami_recoded=case_when(
    income/bay_median< 0.5                                  ~ "Under 50 percent AMI",
    income/bay_median>=0.5 & income/bay_median<1            ~ "50 to 100 percent AMI",
    income/bay_median>=1 & income/bay_median<2              ~ "100 to 200 percent AMI",
    income/bay_median>=2                                    ~ "Over 200 percent AMI",
    TRUE                                                    ~ "Miscoded"
  ))  %>%  
  group_by(ami_recoded)  %>%  
  summarize(count=n(),total=sum(WGTP))  %>%  
  transmute(ami_recoded,PUMS_household_share=total/sum(total)) %>% 
  ungroup()


# Create income function that samples from appropriate PUMS bins to get a discrete income value from categorical data
# Start with more detailed income (which has missing data) then use imputed records to catch missing records

set.seed(1)
discrete_income_f <- function(income_detailed,income_imputed){
  temp <- bay_income  %>%  
    filter(
      case_when(
        income_detailed==1              ~ .$income<15000,
        income_detailed==2              ~ .$income>=15000 & .$income<25000,
        income_detailed==3              ~ .$income>=25000 & .$income<35000,
        income_detailed==4              ~ .$income>=35000 & .$income<50000,
        income_detailed==5              ~ .$income>=50000 & .$income<75000,
        income_detailed==6              ~ .$income>=75000 & .$income<100000,
        income_detailed==7              ~ .$income>=100000 & .$income<150000,
        income_detailed==8              ~ .$income>=150000 & .$income<200000,
        income_detailed==9              ~ .$income>=200000 & .$income<250000,
        income_detailed==10             ~ .$income>=250000,
        income_imputed==1               ~ .$income<25000,
        income_imputed==2               ~ .$income>=25000 & .$income<50000,
        income_imputed==3               ~ .$income>=50000 & .$income<75000,
        income_imputed==4               ~ .$income>=75000 & .$income<100000,
        income_imputed==5               ~ .$income>=100000 & .$income<150000,
        income_imputed==6               ~ .$income>=150000 & .$income<200000,
        income_imputed==7               ~ .$income>=200000 & .$income<250000,
        income_imputed==8               ~ .$income>=250000))
  value <- sample(temp$income,replace = T,size = 1,prob = temp$WGTP)
  return(value)
}

# Recode linked trip file using imputed HH income and race/ethnicity from person file
# Join with household file to append detailed_income variable
# Run discrete income function defined above to get a discrete income variable for each categorical income
# Get share of regional median income and group those values into a new ami variable
# Select significant variables of interest

person_joiner <- person  %>%  
  left_join(.,household,by="hh_id")  %>%  
  filter(is_active_participant==1)  %>%                    # Only include participants
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
  rowwise()  %>%  
  mutate(discrete_income=discrete_income_f(income_detailed,income_imputed)) %>%   # Run discrete income generator function defined above
  ungroup() %>%  
  mutate(
    ami_recoded=case_when(
      discrete_income/bay_median< 0.5                                  ~ "Under 50 percent AMI",
      discrete_income/bay_median>=0.5 & discrete_income/bay_median<1   ~ "50 to 100 percent AMI",
      discrete_income/bay_median>=1 & discrete_income/bay_median<2     ~ "100 to 200 percent AMI",
      discrete_income/bay_median>=2                                    ~ "Over 200 percent AMI",
      TRUE                                                             ~ "Miscoded"
    ))  %>%  
  select(hh_id,person_id,income_recoded,race_recoded,income_detailed,income_imputed,raceeth_new_imputed,ami_recoded,discrete_income,reported_home_lat,reported_home_lon)

# Get race from PUMS for comparing to freeway facilities

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2019/pbayarea19.Rdata"
load (PERSON_RDATA)

race19 <- pbayarea19 %>% 
mutate(racerc=case_when(
  HISP>1               ~"Hispanic",
  HISP==1 & RAC1P==1   ~"White",
  HISP==1 & RAC1P==2   ~"Black",
  HISP==1 & RAC1P==3   ~"Other",
  HISP==1 & RAC1P==4   ~"Other",
  HISP==1 & RAC1P==5   ~"Other",
  HISP==1 & RAC1P==6   ~"Asian/Pacific Islander",
  HISP==1 & RAC1P==7   ~"Asian/Pacific Islander",
  HISP==1 & RAC1P>=8   ~"Other",
  TRUE                 ~"Uncoded")) %>% 
  group_by(racerc) %>% 
  summarize(total=sum(PWGTP)) %>% 
  mutate(race_share=total/sum(total))

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
# Filter out trips that don't have a drive mode (defined as "car_vector, above)
# Include trips with missing mode information (assuming drive)

working <- left_join(facility_flag,recoded_trip,by=c("hh_id","person_id","trip_id")) %>% 
  left_join(.,person_joiner,by=c("hh_id","person_id")) %>% 
  mutate(All_Freeways=1) %>% 
  relocate(All_Freeways,.after = "Mar_Son_101_12To580") %>% 
  filter((mode_1 %in% c(car_vector,997,-9998) | mode_2 %in% car_vector | mode_3 %in% car_vector | mode_4 %in% car_vector),daywt_alladult_wkday>0)

# Function to analyze data and calculate standard errors
# Filter for facility value==1 (i.e., traverses that facility) 
# Formula for SE of a weighted sample: 
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Standard_error_of_a_proportion_estimation_when_using_weighted_data
# Takes the form: sqrt(p(1-p)summation((weights standardized to 1)^2))
# Do separate summaries for race, trip purpose, and income, then concatenate and do additional calculations

# Define tod=time of day using departure hour ("all_day","peak","am_peak","pm_peak","off_peak)
# "all_day" is by definition all day and therefore not a subset/filtering of data

peak <- c(6,7,8,9,15,16,17,18)
am_peak <- c(6,7,8,9)
pm_peak <- c(15,16,17,18)
off_peak <- c(0:5,10:14,19:23)

# Now create function
# df_tod=data frame used for that time of day
# Facility is the roadway analyzed
# To later calculate standard error of a weighted sample, as referenced above, 
# Weights for the subsetted facility need to be standardized to sum to 1 and then squared - done below

calculations <- function(df,facility,tod){
temp_output <- data.frame()

stopifnot(tod %in% c("all_day","peak","am_peak","pm_peak","off_peak"))

if (tod=="all_day"){
temp_df <- df %>% 
  filter(.[[facility]]==1) %>%
mutate(squared_standard_weights=(daywt_alladult_wkday/sum(daywt_alladult_wkday))^2)}

if (tod=="peak"){
  temp_df <- df %>% 
    filter(.[[facility]]==1,.$depart_hour %in% peak) %>%
    mutate(squared_standard_weights=(daywt_alladult_wkday/sum(daywt_alladult_wkday))^2)}

if (tod=="am_peak"){
  temp_df <- df %>% 
    filter(.[[facility]]==1,.$depart_hour %in% am_peak) %>%
    mutate(squared_standard_weights=(daywt_alladult_wkday/sum(daywt_alladult_wkday))^2)}

if (tod=="pm_peak"){
  temp_df <- df %>% 
    filter(.[[facility]]==1,.$depart_hour %in% pm_peak) %>%
    mutate(squared_standard_weights=(daywt_alladult_wkday/sum(daywt_alladult_wkday))^2)}

if (tod=="off_peak"){
  temp_df <- df %>% 
    filter(.[[facility]]==1,.$depart_hour %in% off_peak) %>%
    mutate(squared_standard_weights=(daywt_alladult_wkday/sum(daywt_alladult_wkday))^2)}

# Store value of summed squared standardized weights in a variable for later use  
  error_summation <- sum(temp_df$squared_standard_weights)
  
# Store total trips for calculating shares within each summary
  total_trips <- sum(temp_df$daywt_alladult_wkday)
  
# Summarize data by race/ethnicity, trip purpose, and income

  race <- temp_df %>% 
    group_by(race_recoded) %>% 
    summarize(count=n(),total=sum(daywt_alladult_wkday),share_value=sum(daywt_alladult_wkday)/total_trips) %>% 
    mutate(category="ethnicity") %>% 
    rename(metric=race_recoded) %>% 
    ungroup()
  
  purpose <- temp_df %>% 
    group_by(purpose_recoded) %>% 
    summarize(count=n(),total=sum(daywt_alladult_wkday),share_value=sum(daywt_alladult_wkday)/total_trips) %>% 
    mutate(category="trip_purpose") %>% 
    rename(metric=purpose_recoded) %>% 
    ungroup()
  
  income <- temp_df %>% 
    group_by(income_recoded) %>% 
    summarize(count=n(),total=sum(daywt_alladult_wkday),share_value=sum(daywt_alladult_wkday)/total_trips) %>% 
    mutate(category="income") %>% 
    rename(metric=income_recoded) %>% 
    ungroup()
  
  income_ami <- temp_df %>% 
    group_by(ami_recoded) %>% 
    summarize(count=n(),total=sum(daywt_alladult_wkday),share_value=sum(daywt_alladult_wkday)/total_trips) %>% 
    mutate(category="ami_income") %>% 
    rename(metric=ami_recoded) %>% 
    ungroup()
  
# Calculate standard error, 95 percent confidence interval, lower and upper bound values
# CV reliability
# U.S. Census case studies:
# • High reliability: CVs less than 15%
# • Medium Reliability: CVs between 15‐30% ‐ be careful
# • Low Reliability: CVs over 30% ‐ use with extreme caution  
# Page 2,http://sites.tufts.edu/gis/files/2013/11/Amercian-Community-Survey_Margin-of-error-tutorial.pdf
  
  temp_output <- bind_rows(temp_output,race,purpose,income,income_ami) %>% 
    mutate(roadway=facility,
           standard_error=sqrt((share_value*(1-share_value)*error_summation)),
           ci_95=1.96*standard_error,
           lower_bound=if_else(share_value-ci_95>=0,share_value-ci_95,0),
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
}

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

final <- bind_rows(full_all_day,full_peak,full_am_peak,full_pm_peak,full_off_peak)

# Join PUMS household share of AMI to compare population shares vs. people driving on freeways

final1 <- left_join(final,bay_income_med,by=c("metric"="ami_recoded"))

# Join with location file to place pie charts on map

final2 <- left_join(final1,freeway_coords,by="roadway")

# Output file for analysis in Tableau

write.csv(final2,file.path(Output,"BATS_2019_Facility_Daypart_Summary.csv"),row.names = FALSE)

# Create a file for of just home locations by freeway users

all_locations <- working %>% 
  select("hh_id", "person_id", "trip_id", "Al_SF_80_PlazaTo101", "SF_101_80ToSM", 
         "SF_280_StartToSM", "SM_101_SFToSC", "SM_280_SFToSC", "SC_101_SMTo680", 
         "SC_101_680ToGilroy", "SC_237_101To880", "SC_280_SMTo101", "Al_SC_680_101To580", 
         "Al_SC_880_101To238", "Al_880_238ToPlaza", "Al_580_SanJoaquinTo238", 
         "Al_580_238To80", "Al_80_580ToPlaza", "Al_CC_80_4To580", "CC_Al_24_680To580", 
         "CC_Al_680_4To580", "CC_4_160To680", "Sol_80_YoloToCarquinez", 
         "North_37_101To80", "Mar_Son_101_12To580", "All_Freeways", 
         "daywt_alladult_wkday", "reported_home_lat", "reported_home_lon","o_lat",
         "o_lon","d_lat","d_lon") %>% 
  pivot_longer (.,4:26,names_to = "roadway", values_to = "dummy") %>% 
  filter(dummy==1)%>% 
  select(-dummy) %>% 
  left_join(.,freeway_coords,by="roadway") 

# Convert into long format for Tableau and rbind

homes <- all_locations %>% 
  select(-o_lat, -o_lon, -d_lat, -d_lon) %>% 
  rename(latitude=reported_home_lat,longitude=reported_home_lon) %>% 
  mutate(location="home")

origins <- all_locations %>% 
  select(-reported_home_lat, -reported_home_lon, -d_lat, -d_lon) %>% 
  rename(latitude=o_lat,longitude=o_lon) %>% 
  mutate(location="origin")

destinations <- all_locations %>% 
  select(-reported_home_lat, -reported_home_lon, -o_lat, -o_lon) %>% 
  rename(latitude=d_lat,longitude=d_lon) %>% 
  mutate(location="destination")

all_locations_long <- rbind(homes, origins, destinations)

write.csv(all_locations_long,file.path(Output,"BATS_2019_Freeway_Home_O_D_Locations.csv"),row.names = FALSE)

# Create a file with PUMS AMI income and race joined as rows, not columns, output file

ami_joiner <- bay_income_med %>% 
  mutate(roadway="Bay_Area_Population", category="ami_income",time_period="Bay Area Population",count=NA_real_,
         total=NA_real_,standard_error=NA_real_,ci_95=NA_real_,lower_bound=NA_real_,upper_bound=NA_real_,
         range=NA_real_,cv=NA_real_,est_reliability=NA_character_) %>% 
         rename(metric=ami_recoded, share_value=PUMS_household_share) 

race_joiner <- race19 %>% 
  mutate(roadway="Bay_Area_Population", category="ethnicity",time_period="Bay Area Population",count=NA_real_,
         total=NA_real_,standard_error=NA_real_,ci_95=NA_real_,lower_bound=NA_real_,upper_bound=NA_real_,
         range=NA_real_,cv=NA_real_,est_reliability=NA_character_) %>% 
  rename(metric=racerc, share_value=race_share) 

final3 <- rbind(final,ami_joiner) %>% 
  left_join(.,freeway_coords,by="roadway")

final4 <- rbind(final,race_joiner) %>% 
  left_join(.,freeway_coords,by="roadway")

write.csv(final3,file.path(Output,"BATS_2019_Facility_Daypart_Summary_AMI_in_Rows.csv"),row.names = FALSE)
write.csv(final4,file.path(Output,"BATS_2019_Facility_Daypart_Summary_Race_in_Rows.csv"),row.names = FALSE)

# Try to export highway shapefile to support color coding of segments

# Append facility name associated with links

Al_SF_80_PlazaTo101 <- read.csv(file.path(segment_in,"Al_SF_80_PlazaTo101.csv")) %>% 
  mutate(facility_links="Al_SF_80_PlazaTo101")

SF_101_80ToSM <- read.csv(file.path(segment_in,"SF_101_80ToSM.csv")) %>% 
  mutate(facility_links="SF_101_80ToSM")

SF_280_StartToSM <- read.csv(file.path(segment_in,"SF_280_StartToSM.csv")) %>% 
  mutate(facility_links="SF_280_StartToSM")

SM_101_SFToSC <- read.csv(file.path(segment_in,"SM_101_SFToSC.csv")) %>% 
  mutate(facility_links="SM_101_SFToSC")

SM_280_SFToSC <- read.csv(file.path(segment_in,"SM_280_SFToSC.csv")) %>% 
  mutate(facility_links="SM_280_SFToSC")

SC_101_SMTo680 <- read.csv(file.path(segment_in,"SC_101_SMTo680.csv")) %>% 
  mutate(facility_links="SC_101_SMTo680")

SC_101_680ToGilroy <- read.csv(file.path(segment_in,"SC_101_680ToGilroy.csv")) %>% 
  mutate(facility_links="SC_101_680ToGilroy")

SC_237_101To880 <- read.csv(file.path(segment_in,"SC_237_101To880.csv")) %>% 
  mutate(facility_links="SC_237_101To880")

SC_280_SMTo101 <- read.csv(file.path(segment_in,"SC_280_SMTo101.csv")) %>% 
  mutate(facility_links="SC_280_SMTo101")

Al_SC_680_101To580 <- read.csv(file.path(segment_in,"Al_SC_680_101To580.csv")) %>% 
  mutate(facility_links="Al_SC_680_101To580")

Al_SC_880_101To238 <- read.csv(file.path(segment_in,"Al_SC_880_101To238.csv")) %>% 
  mutate(facility_links="Al_SC_880_101To238")

Al_880_238ToPlaza <- read.csv(file.path(segment_in,"Al_880_238ToPlaza.csv")) %>% 
  mutate(facility_links="Al_880_238ToPlaza")

Al_580_SanJoaquinTo238 <- read.csv(file.path(segment_in,"Al_580_SanJoaquinTo238.csv")) %>% 
  mutate(facility_links="Al_580_SanJoaquinTo238")

Al_580_238To80 <- read.csv(file.path(segment_in,"Al_580_238To80.csv")) %>% 
  mutate(facility_links="Al_580_238To80")

Al_80_580ToPlaza <- read.csv(file.path(segment_in,"Al_80_580ToPlaza.csv")) %>% 
  mutate(facility_links="Al_80_580ToPlaza")

Al_CC_80_4To580 <- read.csv(file.path(segment_in,"Al_CC_80_4To580.csv")) %>% 
  mutate(facility_links="Al_CC_80_4To580")

CC_Al_24_680To580 <- read.csv(file.path(segment_in,"CC_Al_24_680To580.csv")) %>% 
  mutate(facility_links="CC_Al_24_680To580")

CC_Al_680_4To580 <- read.csv(file.path(segment_in,"CC_Al_680_4To580.csv")) %>% 
  mutate(facility_links="CC_Al_680_4To580")

CC_4_160To680 <- read.csv(file.path(segment_in,"CC_4_160To680.csv")) %>% 
  mutate(facility_links="CC_4_160To680")

Sol_80_YoloToCarquinez <- read.csv(file.path(segment_in,"Sol_80_YoloToCarquinez.csv")) %>% 
  mutate(facility_links="Sol_80_YoloToCarquinez")

North_37_101To80 <- read.csv(file.path(segment_in,"North_37_101To80.csv")) %>% 
  mutate(facility_links="North_37_101To80")

Mar_Son_101_12To580 <- read.csv(file.path(segment_in,"Mar_Son_101_12To580.csv")) %>% 
  mutate(facility_links="Mar_Son_101_12To580")

# Bind all segments together into a single file to merge with shapefile

all_segments <- bind_rows(
  Al_SF_80_PlazaTo101,
  SF_101_80ToSM,
  SF_280_StartToSM,
  SM_101_SFToSC,
  SM_280_SFToSC,
  SC_101_SMTo680,
  SC_101_680ToGilroy,
  SC_237_101To880,
  SC_280_SMTo101,
  Al_SC_680_101To580,
  Al_SC_880_101To238,
  Al_880_238ToPlaza,
  Al_580_SanJoaquinTo238,
  Al_580_238To80,
  Al_80_580ToPlaza,
  Al_CC_80_4To580,
  CC_Al_24_680To580,
  CC_Al_680_4To580,
  CC_4_160To680,
  Sol_80_YoloToCarquinez,
  North_37_101To80,
  Mar_Son_101_12To580) %>% 
  select("GID", "facility_links")

# Remove individual data frames to clean up workspace

rm(Al_SF_80_PlazaTo101,
   SF_101_80ToSM,
   SF_280_StartToSM,
   SM_101_SFToSC,
   SM_280_SFToSC,
   SC_101_SMTo680,
   SC_101_680ToGilroy,
   SC_237_101To880,
   SC_280_SMTo101,
   Al_SC_680_101To580,
   Al_SC_880_101To238,
   Al_880_238ToPlaza,
   Al_580_SanJoaquinTo238,
   Al_580_238To80,
   Al_80_580ToPlaza,
   Al_CC_80_4To580,
   CC_Al_24_680To580,
   CC_Al_680_4To580,
   CC_4_160To680,
   Sol_80_YoloToCarquinez,
   North_37_101To80,
   Mar_Son_101_12To580)

# Bring in shapefile, join with link information and equivalency for long names, then export

OSM_Shapefile <- st_read(file.path(OSM_Path,"osm.shp")) %>% 
  left_join(.,all_segments,by="GID") %>% 
  filter(!(is.na(facility_links))) %>% 
  left_join(.,final2, by=c("facility_links"="roadway")) 

st_write(OSM_Shapefile,file.path(OSM_Path, "osm_enhanced.shp"))





