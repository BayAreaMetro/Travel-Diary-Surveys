# -------------------------
# This trip rate and trip distance analysis is a person-day level analysis
# The universe is all adults (18+) because the 2019 survey was adult-only
# -------------------------


# Load required libraries
library(readr)
library(dplyr)
library(glue)
library(tidyr) # to use replace_na


# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023"

# Start a log file
log_file <- glue("{working_dir}/BATS_multi_year_TripRate_TripDistance_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for person-day trip rate and trip distance calculations: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

# -------------------------
# The unit of analysis is person-day level
# The universe is all adults (18+) because the 2019 survey was adult-only
# -------------------------

# Run the script that create the person-day level dataset
source("E:/GitHub/Travel-Diary-Surveys/BATS_2019_2023/Create_PersonDay_df_with_demographic_and_strata_vars.R")
# alternatively, one can just read the output from the above process:
# PersonDays_2019_2023_df <- read_csv("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023/ProcessedPersonDays_2019_2023.csv")

# The universe is all adults (18+) because the 2019 survey was adult-only
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  filter(age>=4)

# Drop if weight is 0 (to get correct unweighted count), although only have negligible impacts on the weighted shares, se, ci, cv calc (4 or 5 digits after the decimal)
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  filter(pdexpfac>0)

# Write PersonDays_2019_2023_df to csv for subsequent processes
output_trips_csv <- glue("{working_dir}/PersonDays_2019_2023_Adults.csv")
write.csv(ProcessedPersonDays_2019_2023_df, file = output_trips_csv, row.names = FALSE)
print(glue("Wrote {nrow(ProcessedPersonDays_2019_2023_df)} rows to {output_trips_csv}"))

#-----------------------------------------
# Create 5 "commute categories" 
# - full time workers who commuted
# - full time workers who telecommuted (4+ hours)
# - full time workers who telecommuted (less than 4 hours)
# - full time workres who didn't work (took time off or sick day)
# - adults (18+) who are not full time workers
#
# if someone both commuted and telecommuted, they will be counted as commuted
#-----------------------------------------
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  mutate(
    commute_cat = case_when(
      employment == 1 & commuted_on_travel_day == 1                           ~ "1. Commuted",
      employment == 1 & telecommute_time >= 420 & commuted_on_travel_day == 0 ~ "2. Telecommuted 7+ hours and not Commuted",
      employment == 1 & telecommute_time > 0 & commuted_on_travel_day == 0    ~ "3. Telecommuted <7 hours and not Commuted",
      employment == 1 & telecommute_time == 0 & commuted_on_travel_day == 0   ~ "4. Did not work",
      TRUE                                                                    ~ "5. Not full-time worker" 
    )
  ) 

# -------------------------
# Calculate mean, se, ci, cv etc
# -------------------------
library(srvyr)

# Create survey design object
srv_design <- ProcessedPersonDays_2019_2023_df %>%
  as_survey_design(
    weights = pdexpfac,
    strata = c(survey_cycle, stratification_var)
  )


# Mean distance (and SE/CI/CV) by subgroup
srv_results_dist <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_dist = survey_mean(personDay_dist_in_miles, vartype = c("se", "ci", "cv"))
  )

# Mean non-work distance (and SE/CI/CV) by subgroup
srv_results_PbShMeSo_dist <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_PbShMeSo_dist = survey_mean(personDay_dist_PbShMeSo_miles, vartype = c("se", "ci", "cv"))
  )

# Mean number of trips (and SE/CI/CV) by subgroup
srv_results_trips <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips = survey_mean(num_trips, vartype = c("se", "ci", "cv"))
  )

# Mean number of discretionary trips (and SE/CI/CV) by subgroup
srv_results_PbShMeSo_trips <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_PbShMeSo_trips = survey_mean(num_PbShMeSo_trips, vartype = c("se", "ci", "cv"))
  )

# Mean number of discretionary trips (and SE/CI/CV) by subgroup
srv_results_ShMeSo_trips <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_ShMeSo_trips = survey_mean(num_ShMeSo_trips, vartype = c("se", "ci", "cv"))
  )  

# Mean number of HOME trips (and SE/CI/CV) by subgroup
srv_results_trips_HOME <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_HOME = survey_mean(num_trips_HOME, vartype = c("se", "ci", "cv"))
  )

# Mean number of WORK trips (and SE/CI/CV) by subgroup
srv_results_trips_WORK <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_WORK = survey_mean(num_trips_WORK, vartype = c("se", "ci", "cv"))
  )

# Mean number of SCHOOL trips (and SE/CI/CV) by subgroup
srv_results_trips_SCHOOL <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SCHOOL = survey_mean(num_trips_SCHOOL, vartype = c("se", "ci", "cv"))
  )

# Mean number of ESCORT trips (and SE/CI/CV) by subgroup
srv_results_trips_ESCORT <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_ESCORT = survey_mean(num_trips_ESCORT, vartype = c("se", "ci", "cv"))
  )

# Mean number of PERS_BUS trips (and SE/CI/CV) by subgroup
srv_results_trips_PERS_BUS <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_PERS_BUS = survey_mean(num_trips_PERS_BUS, vartype = c("se", "ci", "cv"))
  )

# Mean number of SHOP trips (and SE/CI/CV) by subgroup
srv_results_trips_SHOP <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SHOP = survey_mean(num_trips_SHOP, vartype = c("se", "ci", "cv"))
  )

# Mean number of MEAL trips (and SE/CI/CV) by subgroup
srv_results_trips_MEAL <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_MEAL = survey_mean(num_trips_MEAL, vartype = c("se", "ci", "cv"))
  )

# Mean number of SOCREC trips (and SE/CI/CV) by subgroup
srv_results_trips_SOCREC <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SOCREC = survey_mean(num_trips_SOCREC, vartype = c("se", "ci", "cv"))
  )

# Mean number of OTHER trips (and SE/CI/CV) by subgroup
srv_results_trips_OTHER <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_OTHER = survey_mean(num_trips_OTHER, vartype = c("se", "ci", "cv"))
  )


#---------------------------------------------------------------------------------------------

# --- Average trip length (total miles / total trips) by subgroup ---
srv_results_avg_triplen <- srv_design %>%
  group_by(commute_cat, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted   = survey_total(),
    avg_trip_length = survey_ratio(
      numerator   = personDay_dist_in_miles,
      denominator = num_trips,
      vartype = c("se","ci","cv")
    )
  )

# --- Average trip length - ALL ADULTS ---
srv_results_avg_triplen_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted   = survey_total(),
    avg_trip_length = survey_ratio(
      numerator   = personDay_dist_in_miles,
      denominator = num_trips,
      vartype = c("se","ci","cv")
    )
  )

# -------------------------
# Calculate mean, se, ci, cv etc - ALL ADULTS (not by commute_cat)
# -------------------------

# Mean distance - ALL ADULTS
srv_results_dist_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_dist = survey_mean(personDay_dist_in_miles, vartype = c("se", "ci", "cv"))
  )

# Mean non-work distance - ALL ADULTS
srv_results_PbShMeSo_dist_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_PbShMeSo_dist = survey_mean(personDay_dist_PbShMeSo_miles, vartype = c("se", "ci", "cv"))
  )

# Mean number of trips - ALL ADULTS
srv_results_trips_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips = survey_mean(num_trips, vartype = c("se", "ci", "cv"))
  )

# Mean number of discretionary trips - ALL ADULTS
srv_results_PbShMeSo_trips_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_PbShMeSo_trips = survey_mean(num_PbShMeSo_trips, vartype = c("se", "ci", "cv"))
  )

# Mean number of ShMeSo trips - ALL ADULTS
srv_results_ShMeSo_trips_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_ShMeSo_trips = survey_mean(num_ShMeSo_trips, vartype = c("se", "ci", "cv"))
  )

# Mean number of HOME trips - ALL ADULTS
srv_results_trips_HOME_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_HOME = survey_mean(num_trips_HOME, vartype = c("se", "ci", "cv"))
  )

# Mean number of WORK trips - ALL ADULTS
srv_results_trips_WORK_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_WORK = survey_mean(num_trips_WORK, vartype = c("se", "ci", "cv"))
  )

# Mean number of SCHOOL trips - ALL ADULTS
srv_results_trips_SCHOOL_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SCHOOL = survey_mean(num_trips_SCHOOL, vartype = c("se", "ci", "cv"))
  )

# Mean number of ESCORT trips - ALL ADULTS
srv_results_trips_ESCORT_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_ESCORT = survey_mean(num_trips_ESCORT, vartype = c("se", "ci", "cv"))
  )

# Mean number of PERS_BUS trips - ALL ADULTS
srv_results_trips_PERS_BUS_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_PERS_BUS = survey_mean(num_trips_PERS_BUS, vartype = c("se", "ci", "cv"))
  )

# Mean number of SHOP trips - ALL ADULTS
srv_results_trips_SHOP_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SHOP = survey_mean(num_trips_SHOP, vartype = c("se", "ci", "cv"))
  )

# Mean number of MEAL trips - ALL ADULTS
srv_results_trips_MEAL_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_MEAL = survey_mean(num_trips_MEAL, vartype = c("se", "ci", "cv"))
  )

# Mean number of SOCREC trips - ALL ADULTS
srv_results_trips_SOCREC_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SOCREC = survey_mean(num_trips_SOCREC, vartype = c("se", "ci", "cv"))
  )

# Mean number of OTHER trips - ALL ADULTS
srv_results_trips_OTHER_AllAdults <- srv_design %>%
  group_by(survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_OTHER = survey_mean(num_trips_OTHER, vartype = c("se", "ci", "cv"))
  )


# -------------------------
# Calculate mean, se, ci, cv etc - BY COUNTY (all adults by home county)
# -------------------------

# Mean distance - BY COUNTY
srv_results_dist_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_dist = survey_mean(personDay_dist_in_miles, vartype = c("se", "ci", "cv"))
  )

# Mean non-work distance - BY COUNTY
srv_results_PbShMeSo_dist_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_PbShMeSo_dist = survey_mean(personDay_dist_PbShMeSo_miles, vartype = c("se", "ci", "cv"))
  )

# Mean number of trips - BY COUNTY
srv_results_trips_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips = survey_mean(num_trips, vartype = c("se", "ci", "cv"))
  )

# Mean number of discretionary trips - BY COUNTY
srv_results_PbShMeSo_trips_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_PbShMeSo_trips = survey_mean(num_PbShMeSo_trips, vartype = c("se", "ci", "cv"))
  )

# Mean number of ShMeSo trips - BY COUNTY
srv_results_ShMeSo_trips_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_ShMeSo_trips = survey_mean(num_ShMeSo_trips, vartype = c("se", "ci", "cv"))
  )

# Continue for all trip purposes by county...
srv_results_trips_HOME_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_HOME = survey_mean(num_trips_HOME, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_WORK_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_WORK = survey_mean(num_trips_WORK, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_SCHOOL_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SCHOOL = survey_mean(num_trips_SCHOOL, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_ESCORT_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_ESCORT = survey_mean(num_trips_ESCORT, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_PERS_BUS_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_PERS_BUS = survey_mean(num_trips_PERS_BUS, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_SHOP_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SHOP = survey_mean(num_trips_SHOP, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_MEAL_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_MEAL = survey_mean(num_trips_MEAL, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_SOCREC_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SOCREC = survey_mean(num_trips_SOCREC, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_OTHER_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_OTHER = survey_mean(num_trips_OTHER, vartype = c("se", "ci", "cv"))
  )

# Average trip length - BY COUNTY
srv_results_avg_triplen_ByCounty <- srv_design %>%
  group_by(home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted   = survey_total(),
    avg_trip_length = survey_ratio(
      numerator   = personDay_dist_in_miles,
      denominator = num_trips,
      vartype = c("se","ci","cv")
    )
  )

# -------------------------
# Calculate mean, se, ci, cv etc - BY COMMUTE CATEGORY AND COUNTY
# -------------------------

# Mean distance - BY COMMUTE CATEGORY AND COUNTY
srv_results_dist_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_dist = survey_mean(personDay_dist_in_miles, vartype = c("se", "ci", "cv"))
  )

# Mean non-work distance - BY COMMUTE CATEGORY AND COUNTY
srv_results_PbShMeSo_dist_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_PbShMeSo_dist = survey_mean(personDay_dist_PbShMeSo_miles, vartype = c("se", "ci", "cv"))
  )

# Mean number of trips - BY COMMUTE CATEGORY AND COUNTY
srv_results_trips_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips = survey_mean(num_trips, vartype = c("se", "ci", "cv"))
  )

# Mean number of discretionary trips - BY COMMUTE CATEGORY AND COUNTY
srv_results_PbShMeSo_trips_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_PbShMeSo_trips = survey_mean(num_PbShMeSo_trips, vartype = c("se", "ci", "cv"))
  )

# Mean number of ShMeSo trips - BY COMMUTE CATEGORY AND COUNTY
srv_results_ShMeSo_trips_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_ShMeSo_trips = survey_mean(num_ShMeSo_trips, vartype = c("se", "ci", "cv"))
  )

# Continue for all trip purposes by commute category and county...
srv_results_trips_HOME_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_HOME = survey_mean(num_trips_HOME, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_WORK_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_WORK = survey_mean(num_trips_WORK, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_SCHOOL_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SCHOOL = survey_mean(num_trips_SCHOOL, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_ESCORT_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_ESCORT = survey_mean(num_trips_ESCORT, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_PERS_BUS_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_PERS_BUS = survey_mean(num_trips_PERS_BUS, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_SHOP_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SHOP = survey_mean(num_trips_SHOP, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_MEAL_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_MEAL = survey_mean(num_trips_MEAL, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_SOCREC_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_SOCREC = survey_mean(num_trips_SOCREC, vartype = c("se", "ci", "cv"))
  )

srv_results_trips_OTHER_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_num_trips_OTHER = survey_mean(num_trips_OTHER, vartype = c("se", "ci", "cv"))
  )

# Average trip length - BY COMMUTE CATEGORY AND COUNTY
srv_results_avg_triplen_ByCommuteCatCounty <- srv_design %>%
  group_by(commute_cat, home_county_label, survey_cycle) %>%
  summarize(
    n_unweighted = unweighted(n()),
    n_weighted   = survey_total(),
    avg_trip_length = survey_ratio(
      numerator   = personDay_dist_in_miles,
      denominator = num_trips,
      vartype = c("se","ci","cv")
    )
  )

# Display all results
srv_results_dist
srv_results_PbShMeSo_dist
srv_results_trips
srv_results_PbShMeSo_trips
srv_results_ShMeSo_trips
srv_results_trips_HOME
srv_results_trips_WORK
srv_results_trips_SCHOOL
srv_results_trips_ESCORT
srv_results_trips_PERS_BUS
srv_results_trips_SHOP
srv_results_trips_MEAL
srv_results_trips_SOCREC
srv_results_trips_OTHER

# -------------------------
# Create summary table
# -------------------------

# Function to process each survey result 
# Handles both by-commute-cat and all adults summaries
process_survey_result <- function(srv_result, summary_col_name, summary_level) {
  if (summary_level == "By Commute Category") {
    srv_result %>%
      mutate(
        summary_col = summary_col_name,
        summary_level = summary_level,
        chart_label = paste(survey_cycle, " - ", case_when(
          commute_cat == "1. Commuted"              ~ "Commuted",
          commute_cat == "2. Telecommuted 7+ hours and not Commuted" ~ "Telecommuted 7+ hours",
          commute_cat == "3. Telecommuted <7 hours and not Commuted" ~ "Telecommuted <7 hours",
          commute_cat == "4. Did not work"          ~ "Did not work",
          commute_cat == "5. Not full-time worker"  ~ "Not full-time worker",
          TRUE ~ "Unknown"
        ))
      )
  } else if (summary_level == "By County") {
  # For county-level summaries (all adults by county)
   srv_result %>%
      mutate(
        summary_col = summary_col_name,
        summary_level = summary_level,
        commute_cat = NA_character_,
        chart_label = paste(survey_cycle, " - ", home_county_label)
      )
  } else if (summary_level == "By Commute Category and County") {
    # For commute category and county summaries
    srv_result %>%
      mutate(
        summary_col = summary_col_name,
        summary_level = summary_level,
        chart_label = paste(survey_cycle, " - ", case_when(
          commute_cat == "1. Commuted"              ~ "Commuted",
          commute_cat == "2. Telecommuted 7+ hours and not Commuted" ~ "Telecommuted 7+ hours",
          commute_cat == "3. Telecommuted <7 hours and not Commuted" ~ "Telecommuted <7 hours",
          commute_cat == "4. Did not work"          ~ "Did not work",
          commute_cat == "5. Not full-time worker"  ~ "Not full-time worker",
          TRUE ~ "Unknown"
        ), " - ", home_county_label)
      )
  } else {
    # For all adults summaries
    srv_result %>%
      mutate(
        summary_col = summary_col_name,
        summary_level = summary_level,
        commute_cat = NA_character_,
        chart_label = paste(survey_cycle, " - All")
      )
  }
}


# Process each result and standardize column names
summary_list <- list(
  # Distance - by commute category
  process_survey_result(srv_results_dist, "personDay_dist_in_miles", "By Commute Category") %>%
    rename(mean = mean_dist, se = mean_dist_se, ci_lower_95 = mean_dist_low, 
           ci_upper_95 = mean_dist_upp, coeff_of_var = mean_dist_cv),
  
  # Distance - all adults
  process_survey_result(srv_results_dist_AllAdults, "personDay_dist_in_miles", "All adults") %>%
    rename(mean = mean_dist, se = mean_dist_se, ci_lower_95 = mean_dist_low, 
           ci_upper_95 = mean_dist_upp, coeff_of_var = mean_dist_cv),
  
  # PbShMeSo distance - by commute category
  process_survey_result(srv_results_PbShMeSo_dist, "personDay_dist_PbShMeSo_miles", "By Commute Category") %>%
    rename(mean = mean_PbShMeSo_dist, se = mean_PbShMeSo_dist_se, 
           ci_lower_95 = mean_PbShMeSo_dist_low, ci_upper_95 = mean_PbShMeSo_dist_upp, 
           coeff_of_var = mean_PbShMeSo_dist_cv),
  
  # PbShMeSo distance - all adults
  process_survey_result(srv_results_PbShMeSo_dist_AllAdults, "personDay_dist_PbShMeSo_miles", "All adults") %>%
    rename(mean = mean_PbShMeSo_dist, se = mean_PbShMeSo_dist_se, 
           ci_lower_95 = mean_PbShMeSo_dist_low, ci_upper_95 = mean_PbShMeSo_dist_upp, 
           coeff_of_var = mean_PbShMeSo_dist_cv),
  
  # Trips - by commute category
  process_survey_result(srv_results_trips, "num_trips", "By Commute Category") %>%
    rename(mean = mean_num_trips, se = mean_num_trips_se, 
           ci_lower_95 = mean_num_trips_low, ci_upper_95 = mean_num_trips_upp, 
           coeff_of_var = mean_num_trips_cv),
  
  # Trips - all adults
  process_survey_result(srv_results_trips_AllAdults, "num_trips", "All adults") %>%
    rename(mean = mean_num_trips, se = mean_num_trips_se, 
           ci_lower_95 = mean_num_trips_low, ci_upper_95 = mean_num_trips_upp, 
           coeff_of_var = mean_num_trips_cv),
  
  # PbShMeSo trips - by commute category
  process_survey_result(srv_results_PbShMeSo_trips, "num_PbShMeSo_trips", "By Commute Category") %>%
    rename(mean = mean_num_PbShMeSo_trips, se = mean_num_PbShMeSo_trips_se, 
           ci_lower_95 = mean_num_PbShMeSo_trips_low, ci_upper_95 = mean_num_PbShMeSo_trips_upp, 
           coeff_of_var = mean_num_PbShMeSo_trips_cv),
  
  # PbShMeSo trips - all adults
  process_survey_result(srv_results_PbShMeSo_trips_AllAdults, "num_PbShMeSo_trips", "All adults") %>%
    rename(mean = mean_num_PbShMeSo_trips, se = mean_num_PbShMeSo_trips_se, 
           ci_lower_95 = mean_num_PbShMeSo_trips_low, ci_upper_95 = mean_num_PbShMeSo_trips_upp, 
           coeff_of_var = mean_num_PbShMeSo_trips_cv),

  # ShMeSo trips - by commute category
  process_survey_result(srv_results_ShMeSo_trips, "num_ShMeSo_trips", "By Commute Category") %>%
    rename(mean = mean_num_ShMeSo_trips, se = mean_num_ShMeSo_trips_se, 
           ci_lower_95 = mean_num_ShMeSo_trips_low, ci_upper_95 = mean_num_ShMeSo_trips_upp, 
           coeff_of_var = mean_num_ShMeSo_trips_cv),
  
  # ShMeSo trips - all adults
  process_survey_result(srv_results_ShMeSo_trips_AllAdults, "num_ShMeSo_trips", "All adults") %>%
    rename(mean = mean_num_ShMeSo_trips, se = mean_num_ShMeSo_trips_se, 
           ci_lower_95 = mean_num_ShMeSo_trips_low, ci_upper_95 = mean_num_ShMeSo_trips_upp, 
           coeff_of_var = mean_num_ShMeSo_trips_cv),
  
  # HOME trips - by commute category
  process_survey_result(srv_results_trips_HOME, "num_trips_HOME", "By Commute Category") %>%
    rename(mean = mean_num_trips_HOME, se = mean_num_trips_HOME_se, 
           ci_lower_95 = mean_num_trips_HOME_low, ci_upper_95 = mean_num_trips_HOME_upp, 
           coeff_of_var = mean_num_trips_HOME_cv),
  
  # HOME trips - all adults
  process_survey_result(srv_results_trips_HOME_AllAdults, "num_trips_HOME", "All adults") %>%
    rename(mean = mean_num_trips_HOME, se = mean_num_trips_HOME_se, 
           ci_lower_95 = mean_num_trips_HOME_low, ci_upper_95 = mean_num_trips_HOME_upp, 
           coeff_of_var = mean_num_trips_HOME_cv),
  
  # WORK trips - by commute category
  process_survey_result(srv_results_trips_WORK, "num_trips_WORK", "By Commute Category") %>%
    rename(mean = mean_num_trips_WORK, se = mean_num_trips_WORK_se, 
           ci_lower_95 = mean_num_trips_WORK_low, ci_upper_95 = mean_num_trips_WORK_upp, 
           coeff_of_var = mean_num_trips_WORK_cv),
  
  # WORK trips - all adults
  process_survey_result(srv_results_trips_WORK_AllAdults, "num_trips_WORK", "All adults") %>%
    rename(mean = mean_num_trips_WORK, se = mean_num_trips_WORK_se, 
           ci_lower_95 = mean_num_trips_WORK_low, ci_upper_95 = mean_num_trips_WORK_upp, 
           coeff_of_var = mean_num_trips_WORK_cv),
  
  # SCHOOL trips - by commute category
  process_survey_result(srv_results_trips_SCHOOL, "num_trips_SCHOOL", "By Commute Category") %>%
    rename(mean = mean_num_trips_SCHOOL, se = mean_num_trips_SCHOOL_se, 
           ci_lower_95 = mean_num_trips_SCHOOL_low, ci_upper_95 = mean_num_trips_SCHOOL_upp, 
           coeff_of_var = mean_num_trips_SCHOOL_cv),
  
  # SCHOOL trips - all adults
  process_survey_result(srv_results_trips_SCHOOL_AllAdults, "num_trips_SCHOOL", "All adults") %>%
    rename(mean = mean_num_trips_SCHOOL, se = mean_num_trips_SCHOOL_se, 
           ci_lower_95 = mean_num_trips_SCHOOL_low, ci_upper_95 = mean_num_trips_SCHOOL_upp, 
           coeff_of_var = mean_num_trips_SCHOOL_cv),
  
  # ESCORT trips - by commute category
  process_survey_result(srv_results_trips_ESCORT, "num_trips_ESCORT", "By Commute Category") %>%
    rename(mean = mean_num_trips_ESCORT, se = mean_num_trips_ESCORT_se, 
           ci_lower_95 = mean_num_trips_ESCORT_low, ci_upper_95 = mean_num_trips_ESCORT_upp, 
           coeff_of_var = mean_num_trips_ESCORT_cv),
  
  # ESCORT trips - all adults
  process_survey_result(srv_results_trips_ESCORT_AllAdults, "num_trips_ESCORT", "All adults") %>%
    rename(mean = mean_num_trips_ESCORT, se = mean_num_trips_ESCORT_se, 
           ci_lower_95 = mean_num_trips_ESCORT_low, ci_upper_95 = mean_num_trips_ESCORT_upp, 
           coeff_of_var = mean_num_trips_ESCORT_cv),
  
  # PERS_BUS trips - by commute category
  process_survey_result(srv_results_trips_PERS_BUS, "num_trips_PERS_BUS", "By Commute Category") %>%
    rename(mean = mean_num_trips_PERS_BUS, se = mean_num_trips_PERS_BUS_se, 
           ci_lower_95 = mean_num_trips_PERS_BUS_low, ci_upper_95 = mean_num_trips_PERS_BUS_upp, 
           coeff_of_var = mean_num_trips_PERS_BUS_cv),
  
  # PERS_BUS trips - all adults
  process_survey_result(srv_results_trips_PERS_BUS_AllAdults, "num_trips_PERS_BUS", "All adults") %>%
    rename(mean = mean_num_trips_PERS_BUS, se = mean_num_trips_PERS_BUS_se, 
           ci_lower_95 = mean_num_trips_PERS_BUS_low, ci_upper_95 = mean_num_trips_PERS_BUS_upp, 
           coeff_of_var = mean_num_trips_PERS_BUS_cv),
  
  # SHOP trips - by commute category
  process_survey_result(srv_results_trips_SHOP, "num_trips_SHOP", "By Commute Category") %>%
    rename(mean = mean_num_trips_SHOP, se = mean_num_trips_SHOP_se, 
           ci_lower_95 = mean_num_trips_SHOP_low, ci_upper_95 = mean_num_trips_SHOP_upp, 
           coeff_of_var = mean_num_trips_SHOP_cv),
  
  # SHOP trips - all adults
  process_survey_result(srv_results_trips_SHOP_AllAdults, "num_trips_SHOP", "All adults") %>%
    rename(mean = mean_num_trips_SHOP, se = mean_num_trips_SHOP_se, 
           ci_lower_95 = mean_num_trips_SHOP_low, ci_upper_95 = mean_num_trips_SHOP_upp, 
           coeff_of_var = mean_num_trips_SHOP_cv),
  
  # MEAL trips - by commute category
  process_survey_result(srv_results_trips_MEAL, "num_trips_MEAL", "By Commute Category") %>%
    rename(mean = mean_num_trips_MEAL, se = mean_num_trips_MEAL_se, 
           ci_lower_95 = mean_num_trips_MEAL_low, ci_upper_95 = mean_num_trips_MEAL_upp, 
           coeff_of_var = mean_num_trips_MEAL_cv),
  
  # MEAL trips - all adults
  process_survey_result(srv_results_trips_MEAL_AllAdults, "num_trips_MEAL", "All adults") %>%
    rename(mean = mean_num_trips_MEAL, se = mean_num_trips_MEAL_se, 
           ci_lower_95 = mean_num_trips_MEAL_low, ci_upper_95 = mean_num_trips_MEAL_upp, 
           coeff_of_var = mean_num_trips_MEAL_cv),
  
  # SOCREC trips - by commute category
  process_survey_result(srv_results_trips_SOCREC, "num_trips_SOCREC", "By Commute Category") %>%
    rename(mean = mean_num_trips_SOCREC, se = mean_num_trips_SOCREC_se, 
           ci_lower_95 = mean_num_trips_SOCREC_low, ci_upper_95 = mean_num_trips_SOCREC_upp, 
           coeff_of_var = mean_num_trips_SOCREC_cv),
  
  # SOCREC trips - all adults
  process_survey_result(srv_results_trips_SOCREC_AllAdults, "num_trips_SOCREC", "All adults") %>%
    rename(mean = mean_num_trips_SOCREC, se = mean_num_trips_SOCREC_se, 
           ci_lower_95 = mean_num_trips_SOCREC_low, ci_upper_95 = mean_num_trips_SOCREC_upp, 
           coeff_of_var = mean_num_trips_SOCREC_cv),
  
  # OTHER trips - by commute category
  process_survey_result(srv_results_trips_OTHER, "num_trips_OTHER", "By Commute Category") %>%
    rename(mean = mean_num_trips_OTHER, se = mean_num_trips_OTHER_se, 
           ci_lower_95 = mean_num_trips_OTHER_low, ci_upper_95 = mean_num_trips_OTHER_upp, 
           coeff_of_var = mean_num_trips_OTHER_cv),
  
  # OTHER trips - all adults
  process_survey_result(srv_results_trips_OTHER_AllAdults, "num_trips_OTHER", "All adults") %>%
    rename(mean = mean_num_trips_OTHER, se = mean_num_trips_OTHER_se, 
           ci_lower_95 = mean_num_trips_OTHER_low, ci_upper_95 = mean_num_trips_OTHER_upp, 
           coeff_of_var = mean_num_trips_OTHER_cv),

  # Average trip length - by commute category
  process_survey_result(srv_results_avg_triplen, "avg_trip_length_miles", "By Commute Category") %>%
    rename(
      mean         = avg_trip_length,
      se           = avg_trip_length_se,
      ci_lower_95  = avg_trip_length_low,
      ci_upper_95  = avg_trip_length_upp,
      coeff_of_var = avg_trip_length_cv
    ),

  # Average trip length - all adults
  process_survey_result(srv_results_avg_triplen_AllAdults, "avg_trip_length_miles", "All adults") %>%
    rename(
      mean         = avg_trip_length,
      se           = avg_trip_length_se,
      ci_lower_95  = avg_trip_length_low,
      ci_upper_95  = avg_trip_length_upp,
      coeff_of_var = avg_trip_length_cv
    ),
  
  # ===== BY COUNTY (All adults by county) =====
  
  # Distance - by county
  process_survey_result(srv_results_dist_ByCounty, "personDay_dist_in_miles", "By County") %>%
    rename(mean = mean_dist, se = mean_dist_se, ci_lower_95 = mean_dist_low, 
           ci_upper_95 = mean_dist_upp, coeff_of_var = mean_dist_cv),
  
  # PbShMeSo distance - by county
  process_survey_result(srv_results_PbShMeSo_dist_ByCounty, "personDay_dist_PbShMeSo_miles", "By County") %>%
    rename(mean = mean_PbShMeSo_dist, se = mean_PbShMeSo_dist_se, 
           ci_lower_95 = mean_PbShMeSo_dist_low, ci_upper_95 = mean_PbShMeSo_dist_upp, 
           coeff_of_var = mean_PbShMeSo_dist_cv),
  
  # Trips - by county
  process_survey_result(srv_results_trips_ByCounty, "num_trips", "By County") %>%
    rename(mean = mean_num_trips, se = mean_num_trips_se, 
           ci_lower_95 = mean_num_trips_low, ci_upper_95 = mean_num_trips_upp, 
           coeff_of_var = mean_num_trips_cv),
  
  # PbShMeSo trips - by county
  process_survey_result(srv_results_PbShMeSo_trips_ByCounty, "num_PbShMeSo_trips", "By County") %>%
    rename(mean = mean_num_PbShMeSo_trips, se = mean_num_PbShMeSo_trips_se, 
           ci_lower_95 = mean_num_PbShMeSo_trips_low, ci_upper_95 = mean_num_PbShMeSo_trips_upp, 
           coeff_of_var = mean_num_PbShMeSo_trips_cv),
  
  # ShMeSo trips - by county
  process_survey_result(srv_results_ShMeSo_trips_ByCounty, "num_ShMeSo_trips", "By County") %>%
    rename(mean = mean_num_ShMeSo_trips, se = mean_num_ShMeSo_trips_se, 
           ci_lower_95 = mean_num_ShMeSo_trips_low, ci_upper_95 = mean_num_ShMeSo_trips_upp, 
           coeff_of_var = mean_num_ShMeSo_trips_cv),
  
  # HOME trips - by county
  process_survey_result(srv_results_trips_HOME_ByCounty, "num_trips_HOME", "By County") %>%
    rename(mean = mean_num_trips_HOME, se = mean_num_trips_HOME_se, 
           ci_lower_95 = mean_num_trips_HOME_low, ci_upper_95 = mean_num_trips_HOME_upp, 
           coeff_of_var = mean_num_trips_HOME_cv),
  
  # WORK trips - by county
  process_survey_result(srv_results_trips_WORK_ByCounty, "num_trips_WORK", "By County") %>%
    rename(mean = mean_num_trips_WORK, se = mean_num_trips_WORK_se, 
           ci_lower_95 = mean_num_trips_WORK_low, ci_upper_95 = mean_num_trips_WORK_upp, 
           coeff_of_var = mean_num_trips_WORK_cv),
  
  # SCHOOL trips - by county
  process_survey_result(srv_results_trips_SCHOOL_ByCounty, "num_trips_SCHOOL", "By County") %>%
    rename(mean = mean_num_trips_SCHOOL, se = mean_num_trips_SCHOOL_se, 
           ci_lower_95 = mean_num_trips_SCHOOL_low, ci_upper_95 = mean_num_trips_SCHOOL_upp, 
           coeff_of_var = mean_num_trips_SCHOOL_cv),
  
  # ESCORT trips - by county
  process_survey_result(srv_results_trips_ESCORT_ByCounty, "num_trips_ESCORT", "By County") %>%
    rename(mean = mean_num_trips_ESCORT, se = mean_num_trips_ESCORT_se, 
           ci_lower_95 = mean_num_trips_ESCORT_low, ci_upper_95 = mean_num_trips_ESCORT_upp, 
           coeff_of_var = mean_num_trips_ESCORT_cv),
  
  # PERS_BUS trips - by county
  process_survey_result(srv_results_trips_PERS_BUS_ByCounty, "num_trips_PERS_BUS", "By County") %>%
    rename(mean = mean_num_trips_PERS_BUS, se = mean_num_trips_PERS_BUS_se, 
           ci_lower_95 = mean_num_trips_PERS_BUS_low, ci_upper_95 = mean_num_trips_PERS_BUS_upp, 
           coeff_of_var = mean_num_trips_PERS_BUS_cv),
  
  # SHOP trips - by county
  process_survey_result(srv_results_trips_SHOP_ByCounty, "num_trips_SHOP", "By County") %>%
    rename(mean = mean_num_trips_SHOP, se = mean_num_trips_SHOP_se, 
           ci_lower_95 = mean_num_trips_SHOP_low, ci_upper_95 = mean_num_trips_SHOP_upp, 
           coeff_of_var = mean_num_trips_SHOP_cv),
  
  # MEAL trips - by county
  process_survey_result(srv_results_trips_MEAL_ByCounty, "num_trips_MEAL", "By County") %>%
    rename(mean = mean_num_trips_MEAL, se = mean_num_trips_MEAL_se, 
           ci_lower_95 = mean_num_trips_MEAL_low, ci_upper_95 = mean_num_trips_MEAL_upp, 
           coeff_of_var = mean_num_trips_MEAL_cv),
  
  # SOCREC trips - by county
  process_survey_result(srv_results_trips_SOCREC_ByCounty, "num_trips_SOCREC", "By County") %>%
    rename(mean = mean_num_trips_SOCREC, se = mean_num_trips_SOCREC_se, 
           ci_lower_95 = mean_num_trips_SOCREC_low, ci_upper_95 = mean_num_trips_SOCREC_upp, 
           coeff_of_var = mean_num_trips_SOCREC_cv),
  
  # OTHER trips - by county
  process_survey_result(srv_results_trips_OTHER_ByCounty, "num_trips_OTHER", "By County") %>%
    rename(mean = mean_num_trips_OTHER, se = mean_num_trips_OTHER_se, 
           ci_lower_95 = mean_num_trips_OTHER_low, ci_upper_95 = mean_num_trips_OTHER_upp, 
           coeff_of_var = mean_num_trips_OTHER_cv),
  
  # Average trip length - by county
  process_survey_result(srv_results_avg_triplen_ByCounty, "avg_trip_length_miles", "By County") %>%
    rename(
      mean         = avg_trip_length,
      se           = avg_trip_length_se,
      ci_lower_95  = avg_trip_length_low,
      ci_upper_95  = avg_trip_length_upp,
      coeff_of_var = avg_trip_length_cv
    ),
  
  # ===== BY COMMUTE CATEGORY AND COUNTY =====
  
  # Distance - by commute category and county
  process_survey_result(srv_results_dist_ByCommuteCatCounty, "personDay_dist_in_miles", "By Commute Category and County") %>%
    rename(mean = mean_dist, se = mean_dist_se, ci_lower_95 = mean_dist_low, 
           ci_upper_95 = mean_dist_upp, coeff_of_var = mean_dist_cv),
  
  # PbShMeSo distance - by commute category and county
  process_survey_result(srv_results_PbShMeSo_dist_ByCommuteCatCounty, "personDay_dist_PbShMeSo_miles", "By Commute Category and County") %>%
    rename(mean = mean_PbShMeSo_dist, se = mean_PbShMeSo_dist_se, 
           ci_lower_95 = mean_PbShMeSo_dist_low, ci_upper_95 = mean_PbShMeSo_dist_upp, 
           coeff_of_var = mean_PbShMeSo_dist_cv),
  
  # Trips - by commute category and county
  process_survey_result(srv_results_trips_ByCommuteCatCounty, "num_trips", "By Commute Category and County") %>%
    rename(mean = mean_num_trips, se = mean_num_trips_se, 
           ci_lower_95 = mean_num_trips_low, ci_upper_95 = mean_num_trips_upp, 
           coeff_of_var = mean_num_trips_cv),
  
  # PbShMeSo trips - by commute category and county
  process_survey_result(srv_results_PbShMeSo_trips_ByCommuteCatCounty, "num_PbShMeSo_trips", "By Commute Category and County") %>%
    rename(mean = mean_num_PbShMeSo_trips, se = mean_num_PbShMeSo_trips_se, 
           ci_lower_95 = mean_num_PbShMeSo_trips_low, ci_upper_95 = mean_num_PbShMeSo_trips_upp, 
           coeff_of_var = mean_num_PbShMeSo_trips_cv),
  
  # ShMeSo trips - by commute category and county
  process_survey_result(srv_results_ShMeSo_trips_ByCommuteCatCounty, "num_ShMeSo_trips", "By Commute Category and County") %>%
    rename(mean = mean_num_ShMeSo_trips, se = mean_num_ShMeSo_trips_se, 
           ci_lower_95 = mean_num_ShMeSo_trips_low, ci_upper_95 = mean_num_ShMeSo_trips_upp, 
           coeff_of_var = mean_num_ShMeSo_trips_cv),
  
  # HOME trips - by commute category and county
  process_survey_result(srv_results_trips_HOME_ByCommuteCatCounty, "num_trips_HOME", "By Commute Category and County") %>%
    rename(mean = mean_num_trips_HOME, se = mean_num_trips_HOME_se, 
           ci_lower_95 = mean_num_trips_HOME_low, ci_upper_95 = mean_num_trips_HOME_upp, 
           coeff_of_var = mean_num_trips_HOME_cv),
  
  # WORK trips - by commute category and county
  process_survey_result(srv_results_trips_WORK_ByCommuteCatCounty, "num_trips_WORK", "By Commute Category and County") %>%
    rename(mean = mean_num_trips_WORK, se = mean_num_trips_WORK_se, 
           ci_lower_95 = mean_num_trips_WORK_low, ci_upper_95 = mean_num_trips_WORK_upp, 
           coeff_of_var = mean_num_trips_WORK_cv),
  
  # SCHOOL trips - by commute category and county
  process_survey_result(srv_results_trips_SCHOOL_ByCommuteCatCounty, "num_trips_SCHOOL", "By Commute Category and County") %>%
    rename(mean = mean_num_trips_SCHOOL, se = mean_num_trips_SCHOOL_se, 
           ci_lower_95 = mean_num_trips_SCHOOL_low, ci_upper_95 = mean_num_trips_SCHOOL_upp, 
           coeff_of_var = mean_num_trips_SCHOOL_cv),
  
  # ESCORT trips - by commute category and county
  process_survey_result(srv_results_trips_ESCORT_ByCommuteCatCounty, "num_trips_ESCORT", "By Commute Category and County") %>%
    rename(mean = mean_num_trips_ESCORT, se = mean_num_trips_ESCORT_se, 
           ci_lower_95 = mean_num_trips_ESCORT_low, ci_upper_95 = mean_num_trips_ESCORT_upp, 
           coeff_of_var = mean_num_trips_ESCORT_cv),
  
  # PERS_BUS trips - by commute category and county
  process_survey_result(srv_results_trips_PERS_BUS_ByCommuteCatCounty, "num_trips_PERS_BUS", "By Commute Category and County") %>%
    rename(mean = mean_num_trips_PERS_BUS, se = mean_num_trips_PERS_BUS_se, 
           ci_lower_95 = mean_num_trips_PERS_BUS_low, ci_upper_95 = mean_num_trips_PERS_BUS_upp, 
           coeff_of_var = mean_num_trips_PERS_BUS_cv),
  
  # SHOP trips - by commute category and county
  process_survey_result(srv_results_trips_SHOP_ByCommuteCatCounty, "num_trips_SHOP", "By Commute Category and County") %>%
    rename(mean = mean_num_trips_SHOP, se = mean_num_trips_SHOP_se, 
           ci_lower_95 = mean_num_trips_SHOP_low, ci_upper_95 = mean_num_trips_SHOP_upp, 
           coeff_of_var = mean_num_trips_SHOP_cv),
  
  # MEAL trips - by commute category and county
  process_survey_result(srv_results_trips_MEAL_ByCommuteCatCounty, "num_trips_MEAL", "By Commute Category and County") %>%
    rename(mean = mean_num_trips_MEAL, se = mean_num_trips_MEAL_se, 
           ci_lower_95 = mean_num_trips_MEAL_low, ci_upper_95 = mean_num_trips_MEAL_upp, 
           coeff_of_var = mean_num_trips_MEAL_cv),
  
  # SOCREC trips - by commute category and county
  process_survey_result(srv_results_trips_SOCREC_ByCommuteCatCounty, "num_trips_SOCREC", "By Commute Category and County") %>%
    rename(mean = mean_num_trips_SOCREC, se = mean_num_trips_SOCREC_se, 
           ci_lower_95 = mean_num_trips_SOCREC_low, ci_upper_95 = mean_num_trips_SOCREC_upp, 
           coeff_of_var = mean_num_trips_SOCREC_cv),
  
  # OTHER trips - by commute category and county
  process_survey_result(srv_results_trips_OTHER_ByCommuteCatCounty, "num_trips_OTHER", "By Commute Category and County") %>%
    rename(mean = mean_num_trips_OTHER, se = mean_num_trips_OTHER_se, 
           ci_lower_95 = mean_num_trips_OTHER_low, ci_upper_95 = mean_num_trips_OTHER_upp, 
           coeff_of_var = mean_num_trips_OTHER_cv),
  
  # Average trip length - by commute category and county
  process_survey_result(srv_results_avg_triplen_ByCommuteCatCounty, "avg_trip_length_miles", "By Commute Category and County") %>%
    rename(
      mean         = avg_trip_length,
      se           = avg_trip_length_se,
      ci_lower_95  = avg_trip_length_low,
      ci_upper_95  = avg_trip_length_upp,
      coeff_of_var = avg_trip_length_cv
    )
)



# Combine all results into one table
comprehensive_summary <- bind_rows(summary_list)

# Calculate additional columns
comprehensive_summary <- comprehensive_summary %>%
  mutate(
    # Rename existing columns
    unweighted_count = n_unweighted,
    weighted_count = n_weighted,
    
    # Replace NA values with 0 for counts, just in case
    #unweighted_count = replace_na(unweighted_count, 0),
    #weighted_count = replace_na(weighted_count, 0),
    
    # Calculate CI width (95% CI range)
    ci_95 = ci_upper_95 - ci_lower_95,
    
    # Calculate poor estimate reliability flags
    cv_flag = coeff_of_var > 0.30,  # CV > 30%
    sample_size_flag = unweighted_count < 30,  # Minimum sample size
    # ci_width_flag = ci_95 > 0.40,  # CI width > 40pp # drop this flag because this script is not calculating proportions
    # extreme_values_flag = ci_lower_95 < 0 | ci_upper_95 > 1,  # drop this flag because this script is not calculating proportions
    
    # Overall poor reliability decision
    suppress = cv_flag | sample_size_flag,
    
    # Create consolidated estimate reliability flag
    estimate_reliability = case_when(
      cv_flag ~ "Poor (High CV >30%)",
      sample_size_flag ~ "Poor (Small sample n<30)",
      # ci_width_flag ~ "Poor (Wide CI >40pp)",
      #extreme_values_flag ~ "Poor (Invalid range)",
      TRUE ~ "Acceptable"
    )
  )



# Reorder columns 
comprehensive_summary <- comprehensive_summary %>%
  select(
    summary_col,
    summary_level,
    survey_cycle,
    commute_cat,
    home_county_label, 
    chart_label,
    mean,
    se,
    ci_95,
    ci_lower_95,
    ci_upper_95,
    coeff_of_var,
    estimate_reliability,
    unweighted_count,
    weighted_count
  ) %>%
  arrange(summary_col, summary_level, survey_cycle, commute_cat, home_county_label)

# Display the comprehensive summary table
print("\n=== Comprehensive Summary Table ===")
print(comprehensive_summary)

# Write comprehensive summary to CSV
output_summary_csv <- glue("{working_dir}/Summary_TripRate_TripDistance_{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")
write.csv(comprehensive_summary, file = output_summary_csv, row.names = FALSE)
print(glue("\nWrote comprehensive summary with {nrow(comprehensive_summary)} rows to {output_summary_csv}"))

# Close the log file
sink()