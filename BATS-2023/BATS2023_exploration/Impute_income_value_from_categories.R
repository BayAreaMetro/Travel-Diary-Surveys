# -----------------------------------------------------------------------
# This script aims to add a two variables to the BATS household and person data file:
# - hhInc_continuous and poverty_status
#
# This script is largely an excerpt of BATS_2023_Survey_Facility_Margin_of_Error_Calculations_All_Facilities.r by Shimon
# Except that I added some modification in relation to the treatment of family and nonfamily households
#
# Shimon's original script BATS_2023_Survey_Facility_Margin_of_Error_Calculations_All_Facilities.r is in:
# https://github.com/BayAreaMetro/Travel-Diary-Surveys/blob/master/BATS-2023/conflation-trip-summaries/
#
# In the long term, I think BATS_2023_Survey_Facility_Margin_of_Error_Calculations_All_Facilities.r should be broken down into its components, so I started this script
#
# -----------------------------------------------------------------------


# -----------------------------------------------------------------------
# Initial setup
# -----------------------------------------------------------------------

# Bring in libraries
suppressMessages(library(tidyverse))


# Set file directories for input and output

USERPROFILE     <- gsub("////","/", Sys.getenv("USERPROFILE"))
TDS_dir         <- file.path(USERPROFILE, "Box", "Modeling and Surveys","Surveys","Travel Diary Survey")
TDSyear_dir     <- file.path(TDS_dir,"Biennial Travel Diary Survey","Data","2023")
TDSdata_dir     <- file.path(TDSyear_dir,"Full Weighted 2023 Dataset","WeightedDataset_08092024")
#output_dir      <- file.path(TDSyear_dir,"Full Weighted 2023 Dataset","WeightedDataset_08092024", "Processed")
output_dir      <- file.path(USERPROFILE, "Documents", "temp","BridgeTollAnalysis")


# -----------------------------------------------------------------------
# Bring in BATS 2023 survey files
# -----------------------------------------------------------------------


person_df      <- read.csv(file=file.path(TDSdata_dir,"person.csv"))

household_df   <- read.csv(file=file.path(TDSdata_dir,"hh.csv"))%>% 
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
  


# -----------------------------------------------------------------------
# PUMS processing
# -----------------------------------------------------------------------

# Bring in 2022 PUMS data, household and person files for various tasks

# TODO: add a reference to the script that generate hbayarea22.Rdata and pbayarea22.Rdata
PUMS_hBayArea_Rdata = "M:/Data/Census/PUMS/PUMS 2022/hbayarea22.Rdata"
load(PUMS_hBayArea_Rdata)

# the data frame in hbayarea22.Rdata is named hbayarea22
# rename it to make it clear that this is PUMS data
PUMS_hBayArea_df <-hbayarea22
rm(hbayarea22)


# Use PUMS 2022 adjustment variable to inflation-correct values for 2022 (data collected over 12 months, so provides a constant dollar value)
# Remove group quarters and vacant housing

PUMS_hBayArea_income_df <- PUMS_hBayArea_df  %>% 
  mutate(adjustment = ADJINC/1000000,                        # Adjustment variable is 7 digits
         income=HINCP*adjustment)  %>%  
  filter(!is.na(income)) %>%                                 # Remove records with no income (vacant houses, group quarters)
  select(PUMA,SERIALNO,income,HINCP,ADJINC,adjustment,WGTP)  # Select relevant variables

# TODO: Important!!! need to select only families and 1 person households in PUMS

# -----------------------------------------------------------------------
# Defines a function to impute an income value from the income categories
# by drawing a random sample from the distribution in the PUMS dataset
# the sample() function in R is used
# -----------------------------------------------------------------------

# Function for imputing discrete household income value for travel diary categories - using PUMS weight distribution
# The discrete income value is guessed (imputed) in the tightest income category for which there is a categorical data response
# Start with more detailed income (which has missing data) then use imputed records to addressing missing records
# The "TRUE" value in the function below is for records with neither an income_detailed nor an income_imputed_rmove_only record
# In such a condition, a record is chosen at random from the full PUMS dataset to match. 
# In the existing TDS dataset, no such records exist, but the line is included so the function runs in every case (other datasets, etc.)

set.seed(1)
ImputeIncomeFromCategories_f <- function(income_detailed,income_imputed_rmove_only){
  PUMS_hBayArea_income_filtered_df <- PUMS_hBayArea_income_df  %>%  
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
  value <- sample(PUMS_hBayArea_income_filtered_df$income,replace = T,size = 1,prob = PUMS_hBayArea_income_filtered_df$WGTP)
  return(value)
}

# -----------------------------------------------------------------------
# Apply the income imputation to BATS data
#
# I think we need to revisit this income imputation method after we have a full understanding of what RSG has delivered to us.
# For the non family households, we collected the income of the partial household. Did RSG make any correction for this in their imputation? 
# If so, then the income category distribution of the nonfamily household in BATS should match the income category in PUMS well. This is still to be reviewed.
#
# For now, should we filter both PUMS and BATS to include only family households and 1 person households?
# Because we know that for these two groups we will be able to make apples to apples comparison
#
# -----------------------------------------------------------------------

# Returning to travel diary datasets
# Append a variable for related persons on the BATS 2023 person file (differs from HH size in some instances - roommates, etc.)
# Join related persons count to person file for poverty calculation
# Recode trip file using race/ethnicity from person file
# Join with household file to append detailed_income and imputed income variables
# Run discrete income function defined above to get a value for each categorical income
# Get share of 2x poverty, regional AMI and group those values into new variables
# Poverty thresholds come from Katey Hinkle 8/22/24 email to Shimon/Flavia
# See Asana comment: https://app.asana.com/0/0/1207826359892595/1208153772057008/f

# count the number of related person in each household
hh_related_persons_df <- person_df %>%
  filter(relationship != "6") %>%          # Remove unrelated persons
  group_by(hh_id) %>%                      # Group by hh_id
  summarize(num_persons_related = n())  

# TODO: create a dataframe of households including only families and 1 person households (because we did not collect income for nonfamily households)
# i.e. exclude nonfamily households
person_hhldType_df <- person_df %>%
  group_by(hh_id) %>%
  mutate(
    Num_People = n(),
    Household_Has_Spouse_Or_Partner = ifelse(any(relationship == 1), 1, 0),
    Household_Has_Family_Person = ifelse(any(relationship %in% c(2, 3, 4, 5)), 1, 0),
    Household_Type_3Groups = case_when(
      Num_People == 1                                                         ~ "1-person Household",
      Household_Has_Spouse_Or_Partner == 1                                    ~ "Family Household",
      Household_Has_Spouse_Or_Partner == 0 & Household_Has_Family_Person == 1 ~ "Family Household",
      TRUE ~ "Non-Family Household"
    )
  ) %>%
  ungroup() %>%
  select(hh_id, Num_People, person_num, relationship, Household_Has_Spouse_Or_Partner, Household_Has_Family_Person, Household_Type_3Groups, everything())

# Create a houeshold dataset with the household types (i.e. family vs non-family) variable 
hh_hhldType_df <- person_hhldType_df  %>%  
   filter(relationship == 0) # Filters only "self"

hh_hhldType_df <- person_hhldType_df %>%
  filter(relationship == 0) %>%  # Filter for "self"
  select(hh_id, Num_People, Household_Has_Spouse_Or_Partner, Household_Has_Family_Person, Household_Type_3Groups)

# add the household type variables to the BATS household data
BATShh_AddedVars_df <- household_df %>%
  left_join(.,hh_hhldType_df, by="hh_id") %>%
  select(hh_id, Num_People, Household_Has_Spouse_Or_Partner, Household_Has_Family_Person, Household_Type_3Groups, everything())

# filter so that only family and 1 person households are included 
BATShh_FamAnd1Per_df <- BATShh_AddedVars_df %>%
  filter(Household_Type_3Groups != "Non-Family Household")

# Apply the income imputation to BATS data
BATShh_FamAnd1Per_incomeImputed_df  <- BATShh_FamAnd1Per_df  %>% 
  rowwise()  %>%  
  mutate(hhInc_continuous=ImputeIncomeFromCategories_f(income_detailed,income_imputed_rmove_only)) %>%   # Run income imputation function defined above
  ungroup() %>%
  select(hh_id, hhInc_continuous, Num_People, everything()) #put hhInc_continuous as the second column for easy review


# add poverty status
BATShh_FamAnd1Per_incomeImputed_df <- BATShh_FamAnd1Per_incomeImputed_df %>%
  mutate(
    poverty_status = case_when(
      Num_People == 1 & hhInc_continuous <= 30120   ~ "under_2x_poverty",
      Num_People == 2 & hhInc_continuous <= 40880   ~ "under_2x_poverty",
      Num_People == 3 & hhInc_continuous <= 51640   ~ "under_2x_poverty",
      Num_People == 4 & hhInc_continuous <= 62400   ~ "under_2x_poverty",
      Num_People == 5 & hhInc_continuous <= 73160   ~ "under_2x_poverty",
      Num_People == 6 & hhInc_continuous <= 83920   ~ "under_2x_poverty",
      Num_People == 7 & hhInc_continuous <= 94680   ~ "under_2x_poverty",
      Num_People >= 8 & hhInc_continuous <= 105440  ~ "under_2x_poverty",
      TRUE                                          ~ "over_2x_poverty"
    )
  )

# TODO: explicit code those with unknown income

# TODO: some visualizations of PUMS income distribution vs that in the imputed BATS would be good

# -----------------------------------------------------------------------
# Write a household file
# -----------------------------------------------------------------------
write.csv(BATShh_FamAnd1Per_incomeImputed_df, file.path(output_dir,"BATShh_FamAnd1Per_incomeImputed.csv"), row.names = FALSE)

