# -----------
# Calculate work from home rate on survey day (use a measure comparable to SWAA's)
# Universe is all paid workers (full-time, part-time, self-employed). No volunteers.
# Unit is person-day
# -----------

# Load required libraries
library(readr)
library(dplyr)
library(glue)

# -------------------------
# Initial set up and read the person-day data frame
# -------------------------

# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis"


# Start a log file
log_file <- glue("{working_dir}/calculate_WFH_rates{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for calculating telework frequency shares: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

# Run the script that load the person level dataset
source("E:/GitHub/Travel-Diary-Surveys/BATS_2019_2023/Load_PersonDay_df_with_info_from_unlinked_trips.R")
# alternatively, one can just read the output from the above process

# --------------------------------------
# Calculate work from home rate
# --------------------------------------

PersonDays_2019_2023_df <- PersonDays_2019_2023_df %>%
  mutate(
    commute_cat = case_when(
      employment %in% c(1, 2, 3) & commuted_on_travel_day == 1                           ~ "1. Commuted",
      employment %in% c(1, 2, 3) & telecommute_time >= 360 & commuted_on_travel_day == 0 ~ "2. Telecommuted 6+ hours and not Commuted",
      employment %in% c(1, 2, 3) & telecommute_time > 0 & commuted_on_travel_day == 0    ~ "3. Telecommuted <6 hours and not Commuted",
      employment %in% c(1, 2, 3) & telecommute_time == 0 & commuted_on_travel_day == 0   ~ "4. Did not work",
      TRUE                                                                               ~ "5. Not a paid worker" 
    )
  )

commute_cat_summary <- PersonDays_2019_2023_df %>%
  group_by(survey_cycle, commute_cat) %>%
  summarise(
    unweighted = n(),
    weighted = sum(day_weight, na.rm = TRUE),
    .groups = "drop"
  )

wfh_rate <- commute_cat_summary %>%
  filter(commute_cat %in% c("1. Commuted", "2. Telecommuted 6+ hours and not Commuted")) %>%
  group_by(survey_cycle) %>%
  summarise(
    wfh_rate_unweighted = sum(unweighted[commute_cat == "2. Telecommuted 6+ hours and not Commuted"]) / 
                          sum(unweighted),
    wfh_rate_weighted = sum(weighted[commute_cat == "2. Telecommuted 6+ hours and not Commuted"]) / 
                        sum(weighted),
    .groups = "drop"
  )

commute_cat_summary
wfh_rate


sink()
