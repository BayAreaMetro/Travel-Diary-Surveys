# -------------------------
# This journey-to-work mode analysis a person-day level analysis
# The universe is all workers
#
# Note that we want to include "Work-from-home" as a mode
# 
# -------------------------


# Load required libraries
library(readr)
library(dplyr)
library(glue)
library(tidyr) # to use replace_na


# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis"

# Start a log file
log_file <- glue("{working_dir}/BATS_work_tour_mode_share_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

# -------------------------
# The unit of analysis is person-day level
# The universe is all workers
# -------------------------

# Run the script that create the person-day level dataset
source("E:/GitHub/Travel-Diary-Surveys/BATS_2019_2023/Create_PersonDay_df_with_demographic_and_strata_vars.R")
# alternatively, one can just read the output from the above process:
# PersonDays_2019_2023_df <- read_csv("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis/ProcessedPersonDays_2019_2023.csv")

# Include only adults
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  filter(age>=4)


# Universe is all workers (full-time, part-time, self-employed, volunteer)
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  filter(employment %in% c(1,2,3,6))

 
#  filter(employment_label %in% c("1. Employed full-time (paid)", 
#                                 "2. Employed part-time (paid)", 
#                                 "3. Self-employed", 
#                                 "6. Unpaid volunteer or intern")) 

# Drop if weight is 0 (to get correct unweighted count), although only have negligible impacts on the weighted shares, se, ci, cv calc (4 or 5 digits after the decimal)
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  filter(pdexpfac>0)

# Write PersonDays_2019_2023_df to csv for subsequent processes
#output_trips_csv <- glue("{working_dir}/PersonDays_2019_2023_Workers.csv")
#write.csv(ProcessedPersonDays_2019_2023_df, file = output_trips_csv, row.names = FALSE)
#print(glue("Wrote {nrow(ProcessedPersonDays_2019_2023_df)} rows to {output_trips_csv}"))


#-----------------------------------------
# process the tour file to get journey-to-work mode
# an alternative to the tour file approach is a trip file approach:
# keep only home->work trip (but this approach will miss those who do trip chained home->coffee->work; but it is necessary because I don't want to include the trip lunch->work)
# so using the tour file is better
# the process referenced below produces a person-day data frame (each row is uniquely identified by survey_cycle, hhno, pno and day): worktour_personday_2019_2023_df
#-----------------------------------------

# Run the script that create the person-day level dataset
# source("E:/GitHub/Travel-Diary-Surveys/BATS_2019_2023/Process_tour_file_to_get_JTW_mode.R")


# ------------------
# load the tour file
# ------------------- 

# Read 2023 tour file
# Suppress progress bar for cleaner log output
tour2023_df <- read_csv("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/TripLinking_20251209/03b-assign_day/wt-wkday_4day/tour.csv",
                                progress = FALSE) %>% 
  mutate(survey_cycle = 2023)

print(glue("tour2023_df:"))
print(glue("  Observations: {nrow(tour2023_df)}"))
print(glue("  Columns: {ncol(tour2023_df)}"))
cat("\n")

# Read 2019 tour file
# Note that the 2019 file is space delimited
tour2019_df <- read.table("E:/Box/Modeling and Surveys/Surveys/Travel Diary Survey/MPO Partner Household Travel Survey (Includes 2018_2019 TNC Survey)/Bay Area Travel Study 2018-2019/Data from 2018_2019 Survey/BATS2018_2019_SFCTA_processed/20200228/survey2018_tourx.dat",
                                  header = TRUE,
                                  sep = "") %>% 
  mutate(survey_cycle = 2019)


print(glue("tour2019_df:"))
print(glue("  Observations: {nrow(tour2019_df)}"))
print(glue("  Columns: {ncol(tour2019_df)}"))
cat("\n")

# Union the two dataframes
tour_2019_2023_df <- bind_rows(tour2023_df, tour2019_df)


# -----------------
# Processing
# -----------------

# keep only work tours
worktour_2019_2023_df <- tour_2019_2023_df %>%
  filter(pdpurp == 1)


# create a variable "num_work_tours"
worktour_2019_2023_df <- worktour_2019_2023_df %>%
  group_by(hhno, pno, day) %>%
  mutate(num_work_tours = sum(pdpurp == 1, na.rm = TRUE)) %>%
  ungroup()

table(worktour_2019_2023_df$survey_cycle, worktour_2019_2023_df$num_work_tours)
# note that some individuals have more than one tour (up to four, it seems!)


# Function to get the most common value (mode)
# In the case of a tie, the function will return whichever mode appears first in the unique() vector, which corresponds to whichever one appears first in the original data for that person-day.
get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  if(length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

# Collapse the work tour file to person-day level
worktour_personday_2019_2023_df <- worktour_2019_2023_df %>%
  group_by(survey_cycle, hhno, pno, day) %>%
  summarise(
    num_work_tours = n(),
    jtw_mode_from_tour_file = get_mode(tmodetp), # find the most common mode (i.e. the mode in a statistical sense)
    .groups = "drop"
  )



#join worktour_personday_2019_2023_df to ProcessedPersonDays_2019_2023_df
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  left_join(worktour_personday_2019_2023_df, 
            by = c("survey_cycle", "hhno", "pno", "day"))


#-----------------------------------------
# Returning to the Person-Day data
# Create six "journey to work mode categories" 
#-----------------------------------------

# list all the modes at first
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  mutate(
    JTW_cat_detailed = case_when(
      jtw_mode_from_tour_file == 0                                    ~ "0.OTHER",
      jtw_mode_from_tour_file == 1                                    ~ "1. WALK",
      jtw_mode_from_tour_file == 2                                    ~ "2. BIKE",
      jtw_mode_from_tour_file == 3                                    ~ "3. DA",
      jtw_mode_from_tour_file == 4                                    ~ "4. HOV2",
      jtw_mode_from_tour_file == 5                                    ~ "5. HOV3",
      jtw_mode_from_tour_file == 6                                    ~ "6. WALKTRAN",
      jtw_mode_from_tour_file == 7                                    ~ "7. DRIVETRAN",
      jtw_mode_from_tour_file == 8                                    ~ "8. SCHBUS",
      jtw_mode_from_tour_file == 9                                    ~ "9. TNC",
      is.na(jtw_mode_from_tour_file) & telecommute_time > 0           ~ "Worked from home",
      TRUE                                                            ~ "Not in any JTW category" 
    )
  )


# Group the mode variable
# drop TNC, School Bus and Other
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%#  mutate(
    JTW_cat = case_when(
      jtw_mode_from_tour_file == 1                                    ~ "4. WALK",
      jtw_mode_from_tour_file == 2                                    ~ "5. BIKE",
      jtw_mode_from_tour_file == 3                                    ~ "1. DA",
      jtw_mode_from_tour_file == 4                                    ~ "2. Drive with Others",
      jtw_mode_from_tour_file == 5                                    ~ "2. Drive with Others",
      jtw_mode_from_tour_file == 6                                    ~ "3. Transit",
      jtw_mode_from_tour_file == 7                                    ~ "3. Transit",
      is.na(jtw_mode_from_tour_file) & telecommute_time > 0           ~ "6. Worked from home",
      TRUE                                                            ~ "Not in any JTW category" 
    )
  )

#-----------------------------------------
# Calculate mode share by survey_cycle
#-----------------------------------------
mode_share_df <- ProcessedPersonDays_2019_2023_df %>%
  group_by(survey_cycle, JTW_cat) %>%
  summarise(
    unweighted_count = n(),
    weighted_count = sum(pdexpfac, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(survey_cycle) %>%
  mutate(
    unweighted_share = unweighted_count / sum(unweighted_count),
    weighted_share = weighted_count / sum(weighted_count)
  ) %>%
  ungroup()

# View the results
print(mode_share_df, n="Inf")

