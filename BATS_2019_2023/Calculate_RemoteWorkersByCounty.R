# -------------------------
# This remote worker distribution analysis is a person-day level analysis
# The universe is all adults (18+) because the 2019 survey was adult-only
# updated the unverse to "all workers" or just "full time workers" to make it a bit more comparable to PUMS
# -------------------------


# Load required libraries
library(readr)
library(dplyr)
library(glue)
library(tidyr) # to use replace_na


# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis"

# Start a log file
log_file <- glue("{working_dir}/BATS_multi_year_RemoteWorkerLocation_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for % of remote workers by county: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

# -------------------------
# The unit of analysis is person-day level
# The universe is all adults (18+) because the 2019 survey was adult-only
# -------------------------

# Run the script that create the person-day level dataset
source("E:/GitHub/Travel-Diary-Surveys/BATS_2019_2023/Create_PersonDay_df_with_demographic_and_strata_vars.R")
# alternatively, one can just read the output from the above process:
# PersonDays_2019_2023_df <- read_csv("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023_Analysis/ProcessedPersonDays_2019_2023.csv")

# The universe is all adults (18+) because the 2019 survey was adult-only
# updated the unverse to "all workers" or just "full time workers" to make it a bit more comparable to PUMS
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  filter(age>=4) %>%
  filter(employment==1)  %>%
  filter(pdexpfac>0)

# or should it be all workers?
# filter(employment %in% c(1, 2, 3, 7, 8))    

# Write PersonDays_2019_2023_df to csv for subsequent processes
output_csv <- glue("{working_dir}/PersonDays_2019_2023_Adults.csv")
write.csv(ProcessedPersonDays_2019_2023_df, file = output_csv, row.names = FALSE)
print(glue("Wrote {nrow(ProcessedPersonDays_2019_2023_df)} rows to {output_trips_csv}"))

#-----------------------------------------
# Create 5 "commute categories" 
# - full time workers who commuted
# - full time workers who telecommuted (7+ hours)
# - full time workers who telecommuted (less than 7 hours)
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


#-----------------------------------------
# Group the counties
#-----------------------------------------
ProcessedPersonDays_2019_2023_df <- ProcessedPersonDays_2019_2023_df %>%
  mutate(home_county_label_grouped = case_when(
    home_county_label == "Alameda County"       ~ "Alameda",
    home_county_label == "Contra Costa County"  ~ "Contra Costa",
    home_county_label == "Marin County"         ~ "Marin, Sonoma, Napa, Solano",
    home_county_label == "Napa County"          ~ "Marin, Sonoma, Napa, Solano",
    home_county_label == "San Francisco County" ~ "San Francisco",
    home_county_label == "San Mateo County"     ~ "San Mateo",
    home_county_label == "Santa Clara County"   ~ "Santa Clara",
    home_county_label == "Solano County"        ~ "Marin, Sonoma, Napa, Solano",
    home_county_label == "Sonoma County"        ~ "Marin, Sonoma, Napa, Solano",
    TRUE ~ NA_character_  
  ))

#-----------------------------------------
# % of remote workers by county
#-----------------------------------------
ProcessedPersonDays_2019_2023_df %>%
  group_by(survey_cycle, home_county_label) %>%
  summarise(
    pct_commute_cat_2 = (
      sum(pdexpfac[commute_cat == "2. Telecommuted 7+ hours and not Commuted"]) /
      sum(pdexpfac)
    ) * 100
  )


#-----------------------------------------
# Add srvyr and build a survey design
#-----------------------------------------
library(srvyr)

# Get unique survey cycles
survey_cycles <- unique(ProcessedPersonDays_2019_2023_df$survey_cycle)

# Initialize empty list to store results
results_list <- list()

# Loop through each survey cycle
for(cycle in survey_cycles) {
  
  print(glue("\n--- Processing Survey Cycle: {cycle} ---"))
  
  # Filter data for current cycle
  cycle_data <- ProcessedPersonDays_2019_2023_df %>%
    filter(survey_cycle == cycle)
  
  # Create survey design object
  srv_design <- cycle_data %>%
    as_survey_design(
      ids     = hhno,
      weights = pdexpfac,
      strata  = stratification_var 
    )
  
  # Calculate results for this cycle
  results_cycle <- srv_design %>%
    group_by(home_county_label_grouped) %>%
    summarize(
      # Weighted count of total observations
      weighted_n_total = survey_total(
        vartype = "se"
      ),
      # Weighted count of telecommuters
      weighted_n_telecommute = survey_total(
        commute_cat == "2. Telecommuted 7+ hours and not Commuted",
        vartype = "se",
        na.rm = TRUE
      ),
      # Percentage with SE/CI/CV
      pct_commute_cat_2 = survey_mean(
        commute_cat == "2. Telecommuted 7+ hours and not Commuted",
        vartype = c("se", "ci", "cv"),
        deff = TRUE,
        level   = 0.95,
        na.rm   = TRUE
      ),
      # Unweighted counts
      unweighted_n_total = unweighted(n()),
      unweighted_n_telecommute = unweighted(sum(commute_cat == "2. Telecommuted 7+ hours and not Commuted", na.rm = TRUE))
    ) %>%
    mutate(survey_cycle = cycle)  # Add cycle identifier
  
  # Store results
  results_list[[cycle]] <- results_cycle
}

# Combine all results
results_commute_cat2 <- bind_rows(results_list)

# Reorder columns to put survey_cycle first
results_commute_cat2 <- results_commute_cat2 %>%
  select(survey_cycle, everything())

print(results_commute_cat2, width = Inf)

# Write to CSV
output_summary_csv <- glue("{working_dir}/Summary_PctRemoteWorkerByCounty_{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")
write.csv(results_commute_cat2, file = output_summary_csv, row.names = FALSE)
print(glue("\nWrote comprehensive summary with {nrow(results_commute_cat2)} rows to {output_summary_csv}"))

# Close the log file
sink()