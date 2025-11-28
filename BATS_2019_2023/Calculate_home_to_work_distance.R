# -----------
# Calculate home-to-work distance
# -----------

# Load required libraries
library(crul)
library(readr)
library(dplyr)
library(glue)
library(sf)      # simple features, for spatial joins

# -------------------------
# Initial set up and read the person-level data frame
# -------------------------

# Set working directory
working_dir <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/BATS2019_2023"

# Start a log file
log_file <- glue("{working_dir}/calculate_home_to_work_distance{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
sink(log_file, append = TRUE, split = TRUE) 
print(glue("\n=== Log Entry for calculating home-to-work distances: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} ==="))
cat("\n") # print a clean blank line

# Run the script that load the person level dataset
source("E:/GitHub/Travel-Diary-Surveys/BATS_2019_2023/Load_Person_df_with_demographic_and_strata_vars.R")
# alternatively, one can just read the output from the above process

# check dataset for valid values of home and work location
person_2019_2023_df <- person_2019_2023_df %>%
  mutate(
    valid_home_latlon = !is.na(home_lon) & !is.na(home_lat),
    valid_work_latlon = !is.na(work_lon) & !is.na(work_lat)
  )

# check against the employment variable if valid_home_latlon and valid_work_latlon are both valid
person_2019_2023_df %>%
  filter(valid_home_latlon & valid_work_latlon) %>%
  count(employment_label)



# make sure the home and work locations are within the mega region

# Load geojson file for counties in the mega region
mega_region_counties_sf <- st_read(glue("{working_dir}/../../region_county_6062446197583704732.geojson"))

# Check if home and work points are within the mega region counties

# Create separate sf objects for home and work points
home_points_sf <- person_2019_2023_df %>%
  filter(valid_home_latlon) %>%
  st_as_sf(coords = c("home_lon", "home_lat"), crs = 4326)

work_points_sf <- person_2019_2023_df %>%
  filter(valid_work_latlon) %>%
  st_as_sf(coords = c("work_lon", "work_lat"), crs = 4326)

# Check if point intersects with ANY county in the region, force to logical vector (not sparse matrix)
home_in_region <- rowSums(st_intersects(home_points_sf, mega_region_counties_sf, sparse = FALSE)) > 0
work_in_region <- rowSums(st_intersects(work_points_sf, mega_region_counties_sf, sparse = FALSE)) > 0

# Add results back to person_2019_2023_df
person_2019_2023_df$HomeInMegaRegion <- 0
person_2019_2023_df$WorkInMegaRegion <- 0
person_2019_2023_df$HomeInMegaRegion[person_2019_2023_df$valid_home_latlon] <- as.integer(home_in_region)
person_2019_2023_df$WorkInMegaRegion[person_2019_2023_df$valid_work_latlon] <- as.integer(work_in_region)

# Create indicator for both home and work in mega region
person_2019_2023_df$HomeWork_In_MegaRegion <- as.integer(
  person_2019_2023_df$HomeInMegaRegion == 1 & person_2019_2023_df$WorkInMegaRegion == 1
)


# -------------------------

# keep only records with valid lat lon, valid employment status, and within the mega region
person_2019_2023_ForHWloc_df <- person_2019_2023_df %>%
  filter(valid_home_latlon & valid_work_latlon) %>%
  filter(employment_label %in% c("1. Employed full-time (paid)",
                                  "2. Employed part-time (paid)",
                                  "3. Self-employed",
                                  "6. Unpaid volunteer or intern")) %>%
filter(HomeWork_In_MegaRegion == 1)


# Print summary statistics to log
print(glue("Number of records in person_2019_2023_ForHWloc_df: {nrow(person_2019_2023_ForHWloc_df)}"))
print(glue("Sum of person_weight_rmove_only: {sum(person_2019_2023_ForHWloc_df$person_weight_rmove_only, na.rm = TRUE)}"))
cat("\n")

# -----------
# Calculate crow fly distance using Haversine formula
# This is the easist approch. Could use TAZ-based network distance if this line of investigation seems promising
# -----------
print("Calculating crow fly (straight line) distances using Haversine formula...")

# Haversine formula function
# Inputs: lon1, lat1, lon2, lat2 in decimal degrees
# Output: distance in miles
haversine_distance <- function(lon1, lat1, lon2, lat2) {
  # Earth's radius in miles
  R <- 3959
  
  # Convert degrees to radians
  lon1_rad <- lon1 * pi / 180
  lat1_rad <- lat1 * pi / 180
  lon2_rad <- lon2 * pi / 180
  lat2_rad <- lat2 * pi / 180
  
  # Haversine formula
  dlon <- lon2_rad - lon1_rad
  dlat <- lat2_rad - lat1_rad
  
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  distance <- R * c
  return(distance)
}

# Calculate crow fly distance for home-to-work location
person_2019_2023_ForHWloc_df <- person_2019_2023_ForHWloc_df %>%
  mutate(
    home_to_work_crowfly_miles = haversine_distance(home_lon, home_lat, work_lon, work_lat)
  )


# Get routed distance using OSRM server

# Using data.table for efficiency, tidy is too slow for this

# Sequential trip routing using Nick's OSRM server, returns distance in meters
route_coords <- function(o_lon, o_lat, d_lon, d_lat) {
  
  # Initialize result vector
  n <- length(o_lon)
  distance_meters <- rep(NA_real_, n)
  
  # Find valid (non-NA) rows
  valid_rows <- !is.na(o_lon) & !is.na(o_lat) & !is.na(d_lon) & !is.na(d_lat)
  
  if (sum(valid_rows) == 0) {
    return(distance_meters)
  }
  
  # Process valid rows
  valid_indices <- which(valid_rows)
  n_valid <- length(valid_indices)
  
  for (j in seq_along(valid_indices)) {
    i <- valid_indices[j]
    
    # Progress bar
    if (j %% 100 == 0) {
      print(glue("Routing trip {j} of {n_valid} valid trips (of {n} total trips)..."))
    }
    url <- glue::glue("http://router.nicholasfournier.com/route/v1/driving/{o_lon[i]},{o_lat[i]};{d_lon[i]},{d_lat[i]}?geometries=geojson")
    
    tryCatch({
      res <- httr::GET(url)
      if (res$status_code == 200) {
        res_content <- httr::content(res)
        distance_meters[i] <- res_content$routes[[1]]$distance
      }
    }, error = function(e) {
      # Keep as NA on error
    })
  }

  return(distance_meters)
}

# Asynchronous trip routing using Nick's OSRM server, returns distance in meters
route_coords_async <- function(o_lon, o_lat, d_lon, d_lat, batch_size = 1000) {
  
  # Initialize result vector
  n <- length(o_lon)
  distance_meters <- rep(NA_real_, n)
  
  # Find valid rows
  valid_rows <- !is.na(o_lon) & !is.na(o_lat) & !is.na(d_lon) & !is.na(d_lat)
  
  if (sum(valid_rows) == 0) {
    print("No valid rows found for routing, returning NA vector.")
    return(distance_meters)
  }
  
  valid_indices <- which(valid_rows)
  n_valid <- length(valid_indices)
  n_batches <- ceiling(n_valid / batch_size)
  
  # Process in batches
  for (batch in seq_len(n_batches)) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, n_valid)
    batch_indices <- valid_indices[start_idx:end_idx]
    
    print(glue("Processing batch {batch} of {n_batches} ({start_idx} to {end_idx} of {n_valid} valid trips)..."))
    
    # Create async client pool
    base_url = "http://router.nicholasfournier.com/route/v1/driving/"
    options = "?geometries=geojson"
    urls <- glue::glue("{base_url}{o_lon[batch_indices]},{o_lat[batch_indices]};{d_lon[batch_indices]},{d_lat[batch_indices]}{options}")
    
    # Create async requests
    cc <- Async$new(urls = urls)
    
    # Execute all requests concurrently
    res <- cc$get()
    
    # Parse results
    for (k in seq_along(batch_indices)) {
      i <- batch_indices[k]
      tryCatch({
        if (res[[k]]$status_code == 200) {
          content <- jsonlite::fromJSON(res[[k]]$parse("UTF-8"))
          distance_meters[i] <- content$routes$distance
        }
      }, error = function(e) {
        # Keep as NA on error
      })
    }
  }
  
  return(distance_meters)
}


# Routing home/work trips to get distances ================================
person_2019_2023_ForHWloc_df <- person_2019_2023_ForHWloc_df %>%
  mutate(
    home_to_work_miles = route_coords_async(home_lon, home_lat, work_lon, work_lat) / 1609.34
  )

# Routing home/school trips to get distances ================================
person_2019_2023_ForHWloc_df <- person_2019_2023_ForHWloc_df %>%
  mutate(
    home_to_school_miles = route_coords_async(home_lon, home_lat, school_lon, school_lat) / 1609.34
  )


### Statistical summaries

# Summary statistics
summary_home_to_work_miles <- person_2019_2023_ForHWloc_df %>%
  group_by(survey_cycle) %>%
  summarise(
    mean_distance = round(mean(home_to_work_miles, na.rm = TRUE), 2),
    median_distance = round(median(home_to_work_miles, na.rm = TRUE), 2),
    max_distance = round(max(home_to_work_miles, na.rm = TRUE), 2)
  )

cat("\n")
print("Home-to-work distance summary (unweighted):")
print(summary_home_to_work_miles)
cat("\n")

# Mean and median by survey_cycle and employment
summary_home_to_work_miles_ByEmployment <- person_2019_2023_ForHWloc_df %>%
  group_by(survey_cycle, employment_label) %>%
  summarise(
    n = n(),
    mean_miles = mean(home_to_work_miles, na.rm = TRUE),
    median_miles = median(home_to_work_miles, na.rm = TRUE),
    max_miles = max(home_to_work_miles, na.rm = TRUE),
    sd_miles = sd(home_to_work_miles, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n")
print("Home-to-work distance summary, by employment (unweighted):")
print(summary_home_to_work_miles_ByEmployment)
cat("\n")


# Mean and median by survey_cycle and income
summary_home_to_work_miles_ByIncome <- person_2019_2023_ForHWloc_df %>%
  group_by(survey_cycle, income_label) %>%
  summarise(
    n = n(),
    mean_miles = mean(home_to_work_miles, na.rm = TRUE),
    median_miles = median(home_to_work_miles, na.rm = TRUE),
    max_miles = max(home_to_work_miles, na.rm = TRUE),
    sd_miles = sd(home_to_work_miles, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n")
print("Home-to-work distance summary, by income (unweighted):")
print(summary_home_to_work_miles_ByIncome)
cat("\n")


# -------------------------
# Calculate mean, se, ci, cv etc
# -------------------------
library(srvyr)

# Create survey design object
srv_design <- person_2019_2023_ForHWloc_df %>%
  as_survey_design(
    weights = person_weight_rmove_only,
    strata = c(survey_cycle, stratification_var)
  )

# -------------------------
# By survey cycle
# -------------------------

summary_by_cycle <- srv_design %>%
  group_by(survey_cycle) %>%
  summarise(      
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_distance = survey_mean(home_to_work_miles, vartype = c("se", "ci", "cv")),
    median_distance = survey_median(home_to_work_miles, vartype = c("se", "ci", "cv")),
  )

cat("\n")
print("Home-to-work distance summary by survey cycle (from sryvr):")
print(summary_by_cycle, width = Inf)
cat("\n")


# -------------------------
# By income
# -------------------------

summary_by_income <- srv_design %>%
  group_by(survey_cycle, income_label) %>%
  summarise(
    n_unweighted = unweighted(n()),
    n_weighted = survey_total(),
    mean_distance = survey_mean(home_to_work_miles, vartype = c("se", "ci", "cv")),
    median_distance = survey_median(home_to_work_miles, vartype = c("se", "ci", "cv")),
    .groups = "drop"
  )


cat("\n")
print("Home-to-work distance summary by survey cycle and income (from sryvr):")
print(summary_by_income, width = Inf)
cat("\n")

sink() # to close the log file connection

