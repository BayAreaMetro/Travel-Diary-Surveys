library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

# Haversine distance calculation function
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000.0  # Earth radius in meters
  RAD <- pi / 180
  
  lat1r <- lat1 * RAD
  lon1r <- lon1 * RAD
  lat2r <- lat2 * RAD
  lon2r <- lon2 * RAD
  
  dlat <- lat2r - lat1r
  dlon <- lon2r - lon1r
  
  a <- sin(dlat/2)^2 + cos(lat1r) * cos(lat2r) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  
  return(R * c)
}

# Function to prepare survey trips data
prepare_survey_trips <- function(survey_trips) {
  survey_trips %>%
    mutate(
      # Create datetime columns for depart and arrive times
      depart_time = as.POSIXct(
        paste(depart_date, 
              sprintf("%02d:%02d:%02d", depart_hour, depart_minute, depart_seconds)),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      arrive_time = as.POSIXct(
        paste(arrive_date,
              sprintf("%02d:%02d:%02d", arrive_hour, arrive_minute, arrive_second)),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      # Create arrtm and deptm integer columns for joining
      deptm = as.integer(depart_hour * 100 + depart_minute),
      arrtm = as.integer(arrive_hour * 100 + arrive_minute),
      # day (aka dow), 1=Monday, 7=Sunday
      day = as.integer(wday(as.Date(depart_date, format = "%Y-%m-%d"))),
      # Create hhno and pno columns for joining
      hhno = hh_id,
      pno = person_num
    )
}

# Function to prepare linked trips data
prepare_linked_trips <- function(linked_trips) {
  linked_trips %>%
    mutate(
      ltrip_id = row_number() - 1,  # 0-indexed to match Python
      hhno = as.integer(hhno),
      day = as.integer(day)
    )
}

# Function to join survey trips with linked trips
join_survey_to_linked <- function(survey_trips_prep, linked_trips_prep) {
  lnk_int <- linked_trips_prep %>%
    select(ltrip_id, hhno, pno, day, 
           start_lnk = deptm, 
           end_lnk = arrtm)
  
  survey_trips_joined <- survey_trips_prep %>%
    inner_join(lnk_int, by = c("hhno", "pno", "day")) %>%
    filter(deptm >= start_lnk & deptm <= end_lnk)
  
  return(survey_trips_joined)
}

# Function to calculate path distances from location data
calculate_path_distances <- function(survey_locations, survey_trips_joined) {
  path_distance <- survey_locations %>%
    arrange(trip_id) %>%
    group_by(trip_id) %>%
    mutate(
      lat_prev = lag(lat),
      lon_prev = lag(lon)
    ) %>%
    mutate(
      segment_distance_m = ifelse(
        is.na(lat_prev) | is.na(lon_prev),
        0,
        haversine_distance(lat_prev, lon_prev, lat, lon)
      )
    ) %>%
    ungroup() %>%
    left_join(
      survey_trips_joined %>% select(trip_id, ltrip_id),
      by = "trip_id"
    ) %>%
    group_by(ltrip_id) %>%
    summarise(
      path_distance_meters = sum(segment_distance_m, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(path_distance)
}

# Main function to process survey data
process_survey_distances <- function(survey_dir, daysim_dir, year_label) {
  
  cat(sprintf("Processing %s survey data...\n", year_label))
  
  # Replace M: with network path
  daysim_dir <- gsub("M:", M_NETDIR, daysim_dir, fixed = TRUE)
  
  # Define file paths
  trip_path <- file.path(survey_dir, "trip.csv")
  location_path <- file.path(survey_dir, "location.csv")
  linked_trip_path <- file.path(daysim_dir, "trip.csv")
  
  # Load data
  cat("Loading data files...\n")
  survey_trips <- read_csv(trip_path, show_col_types = FALSE)
  survey_locations <- read_csv(location_path, show_col_types = FALSE)
  linked_trips <- read_csv(linked_trip_path, show_col_types = FALSE)
  
  # Prepare data
  cat("Preparing survey trips...\n")
  survey_trips_prep <- prepare_survey_trips(survey_trips)
  
  cat("Preparing linked trips...\n")
  linked_trips_prep <- prepare_linked_trips(linked_trips)
  
  # Join survey trips to linked trips
  cat("Joining survey trips to linked trips...\n")
  survey_trips_joined <- join_survey_to_linked(survey_trips_prep, linked_trips_prep)
  
  # Aggregate trip distances
  cat("Aggregating trip distances...\n")
  trip_distances <- survey_trips_joined %>%
    group_by(ltrip_id) %>%
    summarise(
      sum_distance_meters = sum(distance_meters, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate path distances from location points
  cat("Calculating path distances from location points...\n")
  path_distances <- calculate_path_distances(survey_locations, survey_trips_joined)
  
  # Combine results
  cat("Combining results...\n")
  result <- linked_trips_prep %>%
    left_join(trip_distances, by = "ltrip_id") %>%
    left_join(path_distances, by = "ltrip_id")
  
  cat(sprintf("Completed processing %s survey data.\n\n", year_label))
  
  return(result)
}

# Example usage for 2023 survey
survey_dir_2023 <- "E:/Box/Modeling and Surveys/Surveys/Travel Diary Survey/BATS_2023/MTC_RSG_Partner Repository/5.Deliverables/Task 10 - Weighting and Expansion Data Files/UnweightedDataset/"
daysim_dir_2023 <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/TripLinking_20251012/03b-assign_day/wt-wkday_4day/"

linked_trips_2023 <- process_survey_distances(survey_dir_2023, daysim_dir_2023, "2023")

# Example usage for 2019 survey (update paths as needed)
# survey_dir_2019 <- "E:/Box/Modeling and Surveys/Surveys/Travel Diary Survey/MPO Partner Household Travel Survey (Includes 2018_2019 TNC Survey)/Bay Area Travel Study 2018-2019/Data from 2018_2019 Survey/BATS2018_2019_SFCTA_processed/20200228/"
# daysim_dir_2019 <- "M:/Data/HomeInterview/Bay Area Travel Study 2019/Data/Processed/..."
# linked_trips_2019 <- process_survey_distances(survey_dir_2019, daysim_dir_2019, "2019")

# View results
print(head(linked_trips_2023))

# Compare sum_distance_meters vs path_distance_meters
linked_trips_2023 %>%
  select(ltrip_id, sum_distance_meters, path_distance_meters) %>%
  mutate(difference = path_distance_meters - sum_distance_meters) %>%
  summary()