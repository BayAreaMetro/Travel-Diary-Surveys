# --------------------------------------
# Self-Reported Journey-To-Work Mode Pre and Post-COVID
# --------------------------------------

# Run the script that create the person level dataset
source("E:/GitHub/Travel-Diary-Surveys/BATS-2023/Create_Person_df_with_StrataVars_and_Labels.R")


# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023"


# Start a log file
log_file <- glue("{working_dir}/Self-Reported_Journey-To-Work_Mode_Pre_and_Post-COVID_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for Self-Reported_Journey-To-Work_Mode_Pre_and_Post-COVID: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

# -------------------------------------
# Filter the dataset
# -------------------------------------
# drop "other" and "missing"
person_filtered_df <- person_df %>%
  filter(!work_mode_grouped %in% c("Other", "Missing Response"),
         !pre_covid_mode_grouped %in% c("Other", "Missing Response"))

# check the "universe" being analyzed
# are they all employed workers?
person_filtered_df %>%
     count(employment)

# -------------------------------------
# generate a 3x3 table
# -------------------------------------
pre_covid_mode_analysis_df <- person_filtered_df %>%
  group_by(pre_covid_mode_grouped, work_mode_grouped) %>%
  summarise(
    Weighted_Count = sum(person_weight_rmove_only, na.rm = TRUE),
    Count = n(),
    .groups = "drop"
  ) %>%
  rename(
    `Pre-pandemic` = pre_covid_mode_grouped,
    `2023` = work_mode_grouped
  )

print("Pre and post Pandemic Mode Share (3x3 table):")
print(pre_covid_mode_analysis_df)

# Write pre_covid_mode_analysis_df to CSV
output_summary_csv <- glue("{working_dir}/Self-Reported_Journey-To-Work_Mode_Pre_and_Post-COVID_{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")
write.csv(pre_covid_mode_analysis_df, file = output_summary_csv, row.names = FALSE)
print(glue("\nWrote Pre and post Pandemic Mode Share (3x3 table) {nrow(pre_covid_mode_analysis_df)} rows to {output_summary_csv}"))



# ------------------------------------
# calculate se", "ci", "cv", etc
# ------------------------------------
library(srvyr)

# Create survey design object
svy_design <- person_filtered_df %>%
  as_survey_design(
    ids = hh_id,           # Primary sampling unit
    strata = sample_segment, # Stratification variable
    weights = person_weight_rmove_only
  )

# Calculate pre-pandemic mode share
prepandemic_mode_share_df <- svy_design %>%
  group_by(pre_covid_mode_grouped) %>%
  summarise(
    weighted_share = survey_mean(vartype = c("se", "ci", "cv"))
  )

# Calculate 2023 mode share
postpandemic_mode_share_df <- svy_design %>%
  group_by(work_mode_grouped) %>%
  summarise(
    weighted_share = survey_mean(vartype = c("se", "ci", "cv"))
  )

# View results
print("Pre-Pandemic Mode Share:")
print(prepandemic_mode_share_df)

print("\n2023 Mode Share:")
print(postpandemic_mode_share_df)



# -------------------------------------
# Filter to keep only people who left transit
# -------------------------------------
who_left_df <- person_filtered_df[
  person_filtered_df$pre_covid_mode_grouped == "Transit" & 
  person_filtered_df$work_mode_grouped != "Transit", 
]

who_left_summary <- who_left_df %>%
  group_by(income_broad_label) %>%
  summarise(
    weighted_count = sum(person_weight_rmove_only, na.rm = TRUE),
    unweighted_count = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    percentage = weighted_count / sum(weighted_count) * 100
  ) %>%
  arrange(desc(weighted_count))

print(who_left_summary)


# ------------------------------------
# calculate se", "ci", "cv", etc for "who left"
# ------------------------------------

# Handle singleton PSUs (strata with only 1 cluster)
# Options: "certainty", "adjust", "average", "remove"
options(survey.lonely.psu = "adjust")

# Set up survey design for the who_left subset
who_left_svy <- who_left_df %>%
  as_survey_design(
    ids = hh_id,
    strata = sample_segment,
    weights = person_weight_rmove_only
  )

# Calculate weighted estimates with SE, CI, CV
who_left_summary_with_se <- who_left_svy %>%
  group_by(income_broad_label) %>%
  summarise(
    weighted_count = survey_total(),
    percentage = survey_mean(vartype = c("se", "ci", "cv")) * 100,
    unweighted_count = unweighted(n())
  ) %>%
  arrange(income_broad_label)
  
# See everything
print(who_left_summary_with_se, n = Inf, width = Inf)


# Close the log file
sink()