# Daysim output trip file:
# M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/TripLinking_20251012/03b-assign_day/wt-wkday_4day/trip.csv
# E:/Box/Modeling and Surveys/Surveys/Travel Diary Survey/MPO Partner Household Travel Survey (Includes 2018_2019 TNC Survey)/Bay Area Travel Study 2018-2019/Data from 2018_2019 Survey/BATS2018_2019_SFCTA_processed/20200228/survey2018_tripx.dat

# The location.csv is in:
# E:\Box\Modeling and Surveys\Surveys\Travel Diary Survey\BATS_2023\MTC_RSG_Partner Repository\5.Deliverables\Task 10 - Weighting and Expansion Data Files\UnweightedDataset\location.csv
# E:\Box\Modeling and Surveys\Surveys\Travel Diary Survey\BATS_2023\MTC_RSG_Partner Repository\5.Deliverables\Task 10 - Weighting and Expansion Data Files\UnweightedDataset\trip.csv

# Load required libraries
library(data.table)
library(lubridate)

# M: network path
M_NETDIR <- "//models.ad.mtc.ca.gov/data/models"

# Paths to data files
survey_dir <- "E:/Box/Modeling and Surveys/Surveys/Travel Diary Survey/BATS_2023/MTC_RSG_Partner Repository/5.Deliverables/Task 10 - Weighting and Expansion Data Files/UnweightedDataset/"
daysim_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/TripLinking_20251012/03b-assign_day/wt-wkday_4day/"

# Replace "M:/" with network path
daysim_dir <- gsub("M:", M_NETDIR, daysim_dir, fixed = TRUE)


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


# Match unlinked trips to linked trips from Daysim output
find_unlinked_trips <- function(survey_trips, daysim_trips) {
  
  # NOTE Key columns to map on:
  # linked_trips has hhno=hh_id, pno=person_num, day=travel_dow
  # deptm and arrtm as time converted to integer e.g., hmm with no leading zeros
  
  # Convert to data.table if not already
  setDT(survey_trips)
  setDT(daysim_trips)
  
  # Step 1: Prepare survey trips data
  print("Preparing survey trips data with deptm/arrtm. Datetime operations are slow....")
  survey_trips_prepped <- survey_trips[
    ,
    `:=`(
      # Create datetime columns for depart and arrive times
      depart_time = as.POSIXct(
        paste(depart_date, sprintf("%02d:%02d:%02d", depart_hour, depart_minute, depart_seconds)),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      arrive_time = as.POSIXct(
        paste(arrive_date, sprintf("%02d:%02d:%02d", arrive_hour, arrive_minute, arrive_second)),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      # Create arrtm and deptm integer columns for joining
      deptm = as.integer(depart_hour * 100 + depart_minute),
      arrtm = as.integer(arrive_hour * 100 + arrive_minute),
      # day (aka dow), 1=Monday, 7=Sunday
      day = as.integer(wday(as.Date(depart_date, format = "%Y-%m-%d"))),
      # Create hhno (aka hh_id) and pno (aka person_num) columns for joining
      hhno = hh_id,
      pno = person_num
    )
  ]
  
  # Step 2: Prepare linked trips data, add ltrip_id
  print("Adding linked trip IDs...")
  linked_trips <- daysim_trips[
    ,
    `:=`(
      # Create dummy linked trip ID for mapping from row numbers
      ltrip_id = .I - 1,  # 0-indexed to match Python
      # Cast hhno and day to appropriate types
      hhno = as.integer(hhno),
      day = as.integer(day)
    )
  ]
  
  # Prepare linked trips interval data
  lnk_int <- linked_trips[
    ,
    .(hhno, pno, day, ltrip_id, start_lnk = deptm, end_lnk = arrtm)
  ]
  
  # Step 3: Match survey trips to linked trips using non-equi join
  # This avoids cartesian product and ensures correct time range matching
  print("Matching survey trips to linked trips using non-equi join...")
  survey_trips_joined <- lnk_int[
    survey_trips_prepped[, .(hhno, pno, day, deptm, arrtm, trip_id)],
    on = .(hhno, pno, day, start_lnk <= deptm, end_lnk >= arrtm),
    nomatch = 0
  ]
  
  return(list(
    survey_trips_joined = survey_trips_joined,
    linked_trips = linked_trips
  ))
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


# Main execution

# Load survey trip and location data using data.table's fread for speed
survey_trips <- fread(file.path(survey_dir, "trip.csv"))
survey_locations <- fread(file.path(survey_dir, "location.csv"))
daysim_trips <- fread(file.path(daysim_dir, "trip.csv"))

# Find unlinked trips
result <- find_unlinked_trips(survey_trips, daysim_trips)
survey_trips_joined <- result$survey_trips_joined
linked_trips <- result$linked_trips

# Sum linked trip distances
sum_distances <- survey_trips_joined[
  ,
  .(sum_distance_meters = sum(distance_meters, na.rm = TRUE)),
  by = ltrip_id
]

linked_trips <- sum_distances[linked_trips, on = "ltrip_id"]

# Calculate path distances
linked_trips <- calc_path_distance(
  linked_trips,
  survey_locations,
  survey_trips_joined
)

