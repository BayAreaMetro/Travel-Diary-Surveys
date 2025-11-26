# Daysim output trip file:
# M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/TripLinking_20251012/03b-assign_day/wt-wkday_4day/trip.csv
# E:/Box/Modeling and Surveys/Surveys/Travel Diary Survey/MPO Partner Household Travel Survey (Includes 2018_2019 TNC Survey)/Bay Area Travel Study 2018-2019/Data from 2018_2019 Survey/BATS2018_2019_SFCTA_processed/20200228/survey2018_tripx.dat

# The location.csv is in:
# E:\Box\Modeling and Surveys\Surveys\Travel Diary Survey\BATS_2023\MTC_RSG_Partner Repository\5.Deliverables\Task 10 - Weighting and Expansion Data Files\UnweightedDataset\location.csv
# E:\Box\Modeling and Surveys\Surveys\Travel Diary Survey\BATS_2023\MTC_RSG_Partner Repository\5.Deliverables\Task 10 - Weighting and Expansion Data Files\UnweightedDataset\trip.csv

# Load required libraries
library(data.table)
library(lubridate)
library(glue)

# Calculate haversine distance between two points
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000.0  # Earth radius in meters
  RAD <- pi / 180
  
  # Convert to radians
  lat1r <- lat1 * RAD
  lon1r <- lon1 * RAD
  lat2r <- lat2 * RAD
  lon2r <- lon2 * RAD
  
  # Calculate differences
  dlat <- lat2r - lat1r
  dlon <- lon2r - lon1r
  
  # Haversine formula
  a <- sin(dlat / 2)^2 + cos(lat1r) * cos(lat2r) * sin(dlon / 2)^2
  c <- 2 * asin(sqrt(a))
  
  return(R * c)
}

add_datetime_cols = function(trips_dt, day_dt) {

  # If table does not have _date columns
  if (!("depart_date" %in% names(trips_dt)) | !("arrive_date" %in% names(trips_dt))) {
    print("Adding depart_date and arrive_date columns to trips...")

    # Add depart_date and arrive_date to trips  
    trips_dt[
      day_dt,
      depart_date := i.travel_date,
      on = .(hh_id, person_id, day_num)
    ]
    
    # Check if trip spans midnight, if so, adjust arrive_date
    trips_dt[
      ,
      arrive_date := fifelse(
        (arrive_hour < depart_hour) | # trip crosses midnight, e.g., depart 23:30 arrive 00:15
          # edge case: very long trip that crosses midnight within the same hour, e.g., depart 23:30 (prev day) arrive 23:15 (next day)
          (arrive_hour == depart_hour & arrive_minute < depart_minute),
        as.Date(depart_date) + 1,
        as.Date(depart_date)
      )
    ]
  }

  # if no seconds columns, add dummy seconds columns
  if (!("depart_seconds" %in% names(trips_dt)) | !("arrive_seconds" %in% names(trips_dt))) {
    # Add dummy seconds columns
    trips_dt[
      ,
      `:=`(
        depart_seconds = 0,
        arrive_seconds = 0
      )
    ]
  }

  # if arrive/depart_second is pluralized incorrectly, rename
  if ("depart_seconds" %in% names(trips_dt)) {
    setnames(trips_dt, "depart_seconds", "depart_second")
  }

  if ("arrive_seconds" %in% names(trips_dt)) {
    setnames(trips_dt, "arrive_seconds", "arrive_second")
  }

  # Add _time and deptm and arrtm columns
  if (!("depart_time" %in% names(trips_dt)) | !("arrive_time" %in% names(trips_dt))) {
    print("Preparing survey trips data with deptm/arrtm. Datetime operations are slow....")
    trips_dt <- trips_dt[
      ,
      `:=`(
        # Create datetime columns for depart and arrive times
        depart_time = as.POSIXct(
          paste(depart_date, sprintf("%02d:%02d:%02d", depart_hour, depart_minute, depart_second)),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        arrive_time = as.POSIXct(
          paste(arrive_date, sprintf("%02d:%02d:%02d", arrive_hour, arrive_minute, arrive_second)),
          format = "%Y-%m-%d %H:%M:%S"
        )
      )
    ]
  }
  # Add deptm, arrtm, day, hhno, pno columns
  if (!("deptm" %in% names(trips_dt)) | !("arrtm" %in% names(trips_dt)) |
      !("day" %in% names(trips_dt)) | !("hhno" %in% names(trips_dt)) |
      !("pno" %in% names(trips_dt))) {
    print("Adding deptm, arrtm, day, hhno, pno columns to trips...")
    trips_dt <- trips_dt[
      ,
      `:=`(
        # Create arrtm and deptm integer columns for joining
        deptm = as.integer(depart_hour * 100 + depart_minute),
        arrtm = as.integer(arrive_hour * 100 + arrive_minute),
        # day (aka dow), 1=Monday, 2=Tuesday, 3=Wednesday, 4=Thursday, 5=Friday, 6=Saturday, 7=Sunday
        day = as.integer(wday(as.Date(depart_date, format = "%Y-%m-%d"))) - 1,
        # Create hhno (aka hh_id) and pno (aka person_num) columns for joining
        hhno = hh_id,
        pno = person_num
      )
    ]
  }
  
  return(trips_dt)
}

# Calculate trip distances from location points from linked trips
calc_path_distance <- function(linked_trips, survey_locations, survey_trips_joined) {
  
  # Convert to data.table if not already
  setDT(survey_locations)
  setDT(survey_trips_joined)
  setDT(linked_trips)
  
  # Step 5: Calculate path distance from location points
  # Sort by trip_id and calculate lagged values
  setorder(survey_locations, trip_id)
  survey_locations[
    ,
    `:=`(
      lat_prev = shift(lat, 1, type = "lag"),
      lon_prev = shift(lon, 1, type = "lag")
    ),
    by = trip_id
  ]
  
  # Calculate segment distances
  survey_locations[
    ,
    segment_distance_m := fifelse(
      is.na(lat_prev) | is.na(lon_prev),
      NA_real_,
      haversine_distance(lat_prev, lon_prev, lat, lon)
    )
  ]
  
  # Join with trip mapping and aggregate
  trip_mapping <- unique(survey_trips_joined[, .(trip_id, ltrip_id)])
  survey_locations_with_ltrip <- trip_mapping[survey_locations, on = "trip_id"]
  
  path_distance <- survey_locations_with_ltrip[
    ,
    .(path_distance_meters = sum(segment_distance_m, na.rm = TRUE)),
    by = ltrip_id
  ]
  
  # Step 6: Merge path distance back onto linked trips
  linked_trips <- path_distance[linked_trips, on = "ltrip_id"]
  
  return(linked_trips)
}

# Match unlinked trips to linked trips from Daysim output
match_unlinked_trips <- function(survey_trips, daysim_trips, time_tolerance = 0) {
  
  # Convert to data.table if not already
  setDT(survey_trips)
  setDT(daysim_trips)
    
  # Prepare linked trips interval data
  lnk_int <- daysim_trips[
    ,
    .(
      hhno, pno, day, ltrip_id,
      start_link = deptm - time_tolerance,
      end_link = arrtm + time_tolerance
    )
  ]
  
  # Match survey trips to linked trips using non-equi join
  # Survey trip must fall within linked trip time window
  print("Matching survey trips to linked trips using non-equi join...")
  survey_trips_joined <- survey_trips[
    lnk_int,
    on = .(hhno, pno, day, deptm >= start_link, arrtm <= end_link),
    nomatch = 0
  ]
  
  # Diagnostic: check match rates
  n_matched_linked_trips <- length(unique(survey_trips_joined$ltrip_id))
  n_matched_unlinked_trips <- nrow(survey_trips_joined)
  unlinked_rate = round(100 * n_matched_unlinked_trips / nrow(survey_trips), 2)
  linked_rate = round(100 * n_matched_linked_trips / nrow(daysim_trips), 2)
  print(glue("Total survey trips: {nrow(survey_trips)}"))
  print(glue("Total linked trips: {nrow(daysim_trips)}"))
  print(glue("Matched survey trips: {nrow(survey_trips_joined)}, Match rate: {unlinked_rate}%"))
  print(glue("Matched linked trips: {n_matched_linked_trips}, Match rate: {linked_rate}%"))
  
  return(survey_trips_joined)
}



# Main execution =================================
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023"


# Paths to data files
survey_2023_dir <- "E:/Box/Modeling and Surveys/Surveys/Travel Diary Survey/BATS_2023/MTC_RSG_Partner Repository/5.Deliverables/Task 10 - Weighting and Expansion Data Files/UnweightedDataset/"
survey_2019_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Data/Final Version with Imputations/Final Updated Dataset as of 10-18-2021"


# The combined daysim prepared linked trips file
LinkedTrips_2019_2023_df <- fread(glue("{working_dir}/LinkedTrips_2019_2023_withDist.csv"))

# Original survey trip and location files
survey_day2019 <- fread(file.path(survey_2019_dir, "day.tsv"))
survey_day2023 <- fread(file.path(survey_2023_dir, "day.csv"))

survey_trips2019 <- fread(file.path(survey_2019_dir, "trip.tsv"))
survey_trips2023 <- fread(file.path(survey_2023_dir, "trip.csv"))

# survey_locs2019 <- fread(file.path(survey_2019_dir, "location.tsv"))
# survey_locs2023 <- fread(file.path(survey_2023_dir, "location.csv"))


# Prepare linked trips data, add ltrip_id
# Create dummy linked trip ID for mapping from row numbers
LinkedTrips_2019_2023_df <- LinkedTrips_2019_2023_df[, ltrip_id := .I]

# Prep survey trip date time columns
survey_trips2019 <- add_datetime_cols(survey_trips2019, survey_day2019)
survey_trips2023 <- add_datetime_cols(survey_trips2023, survey_day2023)

# Convert 2019 distance from miles to meters for consistency
survey_trips2019[, distance_meters := distance * 1609.34]

# Combine the two survey years
survey_trips_2019_2023 <- rbindlist(list(
  survey_trips2019[, .(hhno, pno, day, deptm, arrtm, trip_id, distance_meters)],
  survey_trips2023[, .(hhno, pno, day, deptm, arrtm, trip_id, distance_meters)]
))

# Find unlinked trips
matched_survey_trips <- match_unlinked_trips(survey_trips_2019_2023, LinkedTrips_2019_2023_df, time_tolerance = 15)


# Calculate summed distances for unlinked trips
sum_distances <- matched_survey_trips[
  ,
  .(sum_distance_meters = sum(distance_meters, na.rm = TRUE)),
  by = ltrip_id
]

LinkedTrips_2019_2023_df[sum_distances, distance_meters := i.sum_distance_meters, on = "ltrip_id"]
LinkedTrips_2019_2023_df[, distance_miles := distance_meters / 1609.34]


# Export final linked trips with distances
fwrite(LinkedTrips_2019_2023_df, glue("{working_dir}/LinkedTrips_2019_2023_withDist.csv"))
print("Linked trips with distances saved.")




##########
# Diagnose why some trips are not matched
unmatched_survey_trips <- survey_trips_2019_2023[
  !matched_survey_trips,
  on = .(hhno, pno, day, deptm, arrtm, trip_id, distance_meters)
]

unmatched_survey_trips[
  hhno == 23803687 & pno == 1 & day == 4 & deptm >= 2045 & arrtm <= 2258,
]

LinkedTrips_2019_2023_df[
  hhno == 23803687  & pno == 1 & day == 4,
  .(hhno, pno, day, ltrip_id, deptm, arrtm)
]
