# BATS_2023_Survey_Facility_Margin_of_Error_Calculations_All_Facilities.r
# Summarize BATS 2023 survey data for key variables and calculate margins of error
# Remove all PII to develop a version for Tableau Public sharing

# Set options to get rid of scientific notation

options(scipen = 999)

# Bring in libraries

suppressMessages(library(tidyverse))
library(sf)
library(spatstat)

# Set output directory

USERPROFILE    <- gsub("////","/", Sys.getenv("USERPROFILE"))
BOX_dir1       <- file.path(USERPROFILE, "Box", "Modeling and Surveys","Surveys","Travel Diary Survey")
Box_dir2       <- file.path(BOX_dir1,"Biennial Travel Diary Survey","Data","2023")
conflation_loc <- file.path(Box_dir2,"Survey Conflation")
data_loc       <- file.path(Box_dir2,"Full Weighted 2023 Dataset","WeightedDataset_08092024")

# Bring in BATS 2023 survey files
# Select only the variables needed for this script

person         <- read.csv(file=file.path(data_loc,"person.csv")) %>% 
  select(hh_id,person_id,relationship,grep("race|ethnicity|weight",names(.)))
trip           <- read.csv(file=file.path(data_loc,"trip.csv")) %>%     #Drop hh_id for easier joining later on trip_id
  select(-hh_id)
household      <- read.csv(file=file.path(data_loc,"hh.csv"))%>% 
  select(hh_id,num_people,income_detailed,income_imputed_rmove_only,grep("weight",names(.))) %>% 
  mutate(income_detailed_val=case_when(
    income_detailed==1                   ~ "Less than $15,000",
    income_detailed==2                   ~ "$15,000-$24,999",
    income_detailed==3                   ~ "$25,000-$34,999",
    income_detailed==4                   ~ "$35,000-$49,999",
    income_detailed==5                   ~ "$50,000-$74,999",
    income_detailed==6                   ~ "$75,000-$99,999",
    income_detailed==7                   ~ "$100,000-$149,999",
    income_detailed==8                   ~ "$150,000-$199,999",
    income_detailed==9                   ~ "$200,000-$249,999",
    income_detailed==10                  ~ "$250,000 or more",
    income_detailed==999                 ~ "Prefer not to answer"
  ))
  

# Bring in freeway coordinates for placing pie charts on map
# File also has long name equivalencies for facilities and Bay Area geographical designations

#freeway_coords_in <- file.path(Output,"Freeway Facility Coordinates.csv")
#freeway_coords <- read.csv(freeway_coords_in)

# Bring in facility flag file (file that indicates whether a given trip traverses a given freeway/bridge)
# Create vector of facilities for analysis

facility_flag <- read.csv(file = file.path(conflation_loc,"BATS 2023 Facility Use Booleans.csv"))

full_facilities_list <- c("bay_bridge",
                          "sm_bridge",
                          "dum_bridge",
                          "rsr_bridge",
                          "carq_bridge",
                          "bm_bridge",
                          "ant_bridge",
                          "gg_bridge",
                          "bata_bridges",
                          "sr37_80_to_mare",
                          "sr37_mare_to_121",
                          "sr37_121_to_101",
                          "sr37_80_to_101",
                          "i880_baybridge_to_237",
                          "i680_80_to_580",
                          "i680_580_to_101",
                          "sr4_80_to_160",
                          "i580_hayward_to_sanjoaquin",
                          "i580_hayward_to_baybridge",
                          "i80_13_to_580",
                          "i80_580_to_Carquinez",
                          "i80_680_to_12")
                          

# Function for imputing discrete value from PUMS distribution
# Start by bringing in 2022 PUMS data

HH_RDATA = "M:/Data/Census/PUMS/PUMS 2022/hbayarea22.Rdata"
load (HH_RDATA)

# Adjust income to inflation-correct values for 2019
# Remove group quarters and vacant housing

bay_income <- hbayarea22  %>% 
  mutate(adjustment = ADJINC/1000000,
         income=HINCP*adjustment)  %>%  
  filter(!is.na(income)) %>% 
  select(PUMA,SERIALNO,income,HINCP,ADJINC,adjustment,WGTP)

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
# The "TRUE" value below is for records with neither an income_detailed nor an income_imputed_rmove_only record
# This chooses a record from the full PUMS dataset to match. In the existing TDS dataset, no such records exist, but
# the line is inlcuded so the function runs in every case

set.seed(1)
discrete_income_f <- function(income_detailed,income_imputed_rmove_only){
  temp <- bay_income  %>%  
    filter(
      case_when(
        income_detailed==1                              ~ .$income<15000,
        income_detailed==2                              ~ .$income>=15000 & .$income<25000,
        income_detailed==3                              ~ .$income>=25000 & .$income<35000,
        income_detailed==4                              ~ .$income>=35000 & .$income<50000,
        income_detailed==5                              ~ .$income>=50000 & .$income<75000,
        income_detailed==6                              ~ .$income>=75000 & .$income<100000,
        income_detailed==7                              ~ .$income>=100000 & .$income<150000,
        income_detailed==8                              ~ .$income>=150000 & .$income<200000,
        income_detailed==9                              ~ .$income>=200000 & .$income<250000,
        income_detailed==10                             ~ .$income>=250000,
        income_imputed_rmove_only=="Under $25,000"      ~ .$income<25000,
        income_imputed_rmove_only=="$25,000-$49,999"    ~ .$income>=25000 & .$income<50000,
        income_imputed_rmove_only=="$50,000-$74,999"    ~ .$income>=50000 & .$income<75000,
        income_imputed_rmove_only=="$75,000-$99,999"    ~ .$income>=75000 & .$income<100000,
        income_imputed_rmove_only=="$100,000-$199,999"  ~ .$income>=100000 & .$income<200000,
        income_imputed_rmove_only=="$200,000 or more"   ~ .$income>=200000,
        TRUE                                            ~ .$income<250000)) 
  value <- sample(temp$income,replace = T,size = 1,prob = temp$WGTP)
  return(value)
}


# 
# Recode trip file using race/ethnicity from person file
# Join with household file to append detailed_income and imputed income variables
# Run discrete income function defined above to get a discrete income variable for each categorical income
# Get share of regional median income and group those values into a new ami variable
# Select significant variables of interest

related_persons <- person %>%
  filter(relationship != "6") %>%          # Remove unrelated persons
  group_by(hh_id) %>%                      # Group by hh_id
  summarize(num_persons_related = n())  

person_joiner <- person  %>%  
  left_join(.,related_persons, by="hh_id") %>% 
  left_join(.,household,by="hh_id") %>% 
  rowwise()  %>%  
  mutate(discrete_income=discrete_income_f(income_detailed,income_imputed_rmove_only)) %>%   # Run discrete income generator function defined above
  ungroup() %>% 
  mutate(race_recoded=case_when(
    ethnicity_imputed_rmove_only=="hispanic"           ~ "Hispanic",
    race_imputed_rmove_only=="afam"                    ~ "Black",
    race_imputed_rmove_only=="asian_pacific"           ~ "Asian/Pacific Islander",
    race_imputed_rmove_only=="white"                   ~ "White",
    race_imputed_rmove_only=="other"                   ~ "Other",
    TRUE                                               ~ "Miscoded"
  ),
  poverty_status=case_when(
    num_persons_related==1 & discrete_income<=30120        ~ "under_2x_poverty",
    num_persons_related==2 & discrete_income<=40880        ~ "under_2x_poverty",
    num_persons_related==3 & discrete_income<=51640        ~ "under_2x_poverty",
    num_persons_related==4 & discrete_income<=62400        ~ "under_2x_poverty",
    num_persons_related==5 & discrete_income<=73160        ~ "under_2x_poverty",
    num_persons_related==6 & discrete_income<=83920        ~ "under_2x_poverty",
    num_persons_related==7 & discrete_income<=94680        ~ "under_2x_poverty",
    num_persons_related>=8 & discrete_income<=105440       ~ "under_2x_poverty",
    TRUE                                                   ~ "over_2x_poverty"
  ),
  ami_recoded=case_when(
      discrete_income/bay_median< 0.5                                  ~ "Under 50 percent AMI",
      discrete_income/bay_median>=0.5 & discrete_income/bay_median<1   ~ "50 to 100 percent AMI",
      discrete_income/bay_median>=1 & discrete_income/bay_median<2     ~ "100 to 200 percent AMI",
      discrete_income/bay_median>=2                                    ~ "Over 200 percent AMI",
      TRUE                                                             ~ "Miscoded"
    ))  

# Get race from PUMS for comparing to freeway facilities

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2022/pbayarea22.Rdata"
load (PERSON_RDATA)

race22 <- pbayarea22 %>% 
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
      d_purpose_category %in% c(1,12)            ~ "Home", # Home, spent night elsewhere
      d_purpose_category %in% c(2,3)             ~ "Work", # Work or work related
      d_purpose_category %in% c(4,5)             ~ "School", # School, related
      d_purpose_category %in% c(6)               ~ "Escort", 
      d_purpose_category %in% c(7,8,9)           ~ "Shop,meal,social,recreational",
      d_purpose_category==10                     ~ "Errand/appointment",
      d_purpose_category==11                     ~ "Change mode",
      d_purpose_category %in% c(-1,13)           ~ "Other/missing",
      TRUE                                       ~ "Miscoded"
    )
  )

# Join trips file with facility flag file 
# Create flag for all freeways, used later to sum trip characteristics among full freeway network
# Remove trips that don't have rmove weights

working <- left_join(facility_flag,recoded_trip,by="trip_id") %>% 
  left_join(.,person_joiner,by=c("person_id")) %>% 
  mutate(All_Freeways=1) %>% 
  relocate(All_Freeways,.after = "i80_680_to_12") %>% 
  filter(trip_weight_rmove_only>0)

# Function to analyze data and calculate standard errors
# Filter for facility value==1 (i.e., traverses that facility) 
# Formula for SE of a weighted sample: 
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Standard_error_of_a_proportion_estimation_when_using_weighted_data
# Takes the form: sqrt(p(1-p)summation((weights standardized to 1)^2))
# Do separate summaries for race, trip purpose, and income, then concatenate and do additional calculations

# Define tod=time of day using departure hour ("all_day","peak","am_peak","pm_peak","off_peak)
# "all_day" is by definition all day and therefore not a subset/filtering of data

peak <- c(6:9,15:18)
am_peak <- c(6:9)
pm_peak <- c(15:18)
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
    rename(metric=poverty_recoded) %>% 
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

# Create a file with PUMS AMI income and race joined as rows, not columns, output file

ami_joiner <- bay_income_med %>% 
  mutate(roadway="Bay_Area_Population", category="ami_income",time_period="Bay Area Population",count=NA_real_,
         total=NA_real_,standard_error=NA_real_,ci_95=NA_real_,lower_bound=NA_real_,upper_bound=NA_real_,
         range=NA_real_,cv=NA_real_,est_reliability=NA_character_) %>% 
         rename(metric=ami_recoded, share_value=PUMS_household_share) 

race_joiner <- race22 %>% 
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







