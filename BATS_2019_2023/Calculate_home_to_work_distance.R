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
source("./BATS_2019_2023/Load_Person_df_with_demographic_and_strata_vars.R")
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
  routes <- vector("list", n)
  
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
          routes[[i]] <- content$routes$geometry
        }
      }, error = function(e) {
        # Keep as NA on error
      })
    }
  }
  
  return(list(
    distance_meters = distance_meters,
    routes = routes
  ))
}


# Routing home/work trips to get distances ================================
print("Routing home-to-work trips to get distances...")
work_routes_data = route_coords_async(
  person_2019_2023_ForHWloc_df$home_lon,
  person_2019_2023_ForHWloc_df$home_lat,
  person_2019_2023_ForHWloc_df$work_lon,
  person_2019_2023_ForHWloc_df$work_lat,
  batch_size = 5000
)
person_2019_2023_ForHWloc_df <- person_2019_2023_ForHWloc_df %>%
  mutate(
    home_to_work_miles = work_routes_data$distance_meters / 1609.34,
    route = work_routes_data$routes
  )

# Compare crow fly vs routed distances

# XY scatter plot
library(ggplot2)
ggplot(person_2019_2023_ForHWloc_df, aes(x = home_to_work_crowfly_miles, y = home_to_work_miles)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Home-to-Work Distance: Crow Fly vs Routed",
    x = "Crow Fly Distance (miles)",
    y = "Routed Distance (miles)"
  ) +
  coord_fixed() +
  theme_minimal()
ggsave(glue("{working_dir}/home_to_work_crowfly_vs_routed_scatterplot.png"), width = 8, height = 8)

### Mapping a sample of routes


# Function to plot sample of routes from multiple survey cycles
plot_sample_routes <- function(person_df, sample_size = 100) {
  # Set seed for reproducibility
  set.seed(123)

  # Filter out records with 0 distance and 0 weight
  person_df <- person_df %>%
    filter(home_to_work_miles > 0 & !is.na(work_lon) & !is.na(work_lat) & person_weight_rmove_only > 0)
  
  n_2019 <- nrow(person_df %>% filter(survey_cycle == 2019))
  n_2023 <- nrow(person_df %>% filter(survey_cycle == 2023))

  # Check if the samples are balanced, stop if not
  if (n_2019 < sample_size | n_2023 < sample_size) {
    warning(glue("Sample size reduced due to insufficient records: 2019={n_2019}, 2023={n_2023}"))
    sample_size <- min(n_2019, n_2023, sample_size)
  }

  # Sample from each survey cycle
  sample_2019 <- person_df %>%
    filter(survey_cycle == 2019) %>%
    sample_n(min(sample_size, n()))
  
  sample_2023 <- person_df %>%
    filter(survey_cycle == 2023) %>%
    sample_n(min(sample_size, n()))
  

  # Function to convert routes to sf linestrings
  routes_to_sf <- function(sample_df) {
    routes_sf <- st_sfc(lapply(sample_df$route, function(geojson) {
      if (is.null(geojson)) {
        return(NULL)
      }
      st_linestring(do.call(rbind, geojson$coordinates))
    }), crs = 4326)
    
    return(routes_sf)
  }
  
  # Convert routes to sf objects
  routes_2019_sf <- routes_to_sf(sample_2019)
  routes_2023_sf <- routes_to_sf(sample_2023)
  
  # Create home location points
  home_2019_sf <- sample_2019 %>%
    st_as_sf(coords = c("home_lon", "home_lat"), crs = 4326) %>%
    mutate(survey_cycle = "2019")
  
  home_2023_sf <- sample_2023 %>%
    st_as_sf(coords = c("home_lon", "home_lat"), crs = 4326) %>%
    mutate(survey_cycle = "2023")
  
  home_combined <- rbind(
    home_2019_sf %>% select(survey_cycle, geometry),
    home_2023_sf %>% select(survey_cycle, geometry)
  )

  # Create work location points
  work_2019_sf <- sample_2019 %>%
    st_as_sf(coords = c("work_lon", "work_lat"), crs = 4326) %>%
    mutate(survey_cycle = "2019")
  
  work_2023_sf <- sample_2023 %>%
    st_as_sf(coords = c("work_lon", "work_lat"), crs = 4326) %>%
    mutate(survey_cycle = "2023")

  work_combined <- rbind(
    work_2019_sf %>% select(survey_cycle, geometry),
    work_2023_sf %>% select(survey_cycle, geometry)
  )
  
  # Create combined plot
  # Create data frames with survey cycle labels for legend
  routes_2019_df <- st_sf(geometry = routes_2019_sf, survey_cycle = "2019")
  routes_2023_df <- st_sf(geometry = routes_2023_sf, survey_cycle = "2023")
  routes_combined <- rbind(routes_2019_df, routes_2023_df)
  
  plot = ggplot() +
    geom_sf(data = routes_combined, aes(color = survey_cycle), alpha = 0.2) +
    geom_sf(data = home_combined, aes(color = survey_cycle), size = 1, shape = 19, alpha = 0.2) +
    geom_sf(data = work_combined, aes(color = survey_cycle), size = 1, shape = 5, alpha = 0.2) +
    scale_color_manual(values = c("2019" = "blue", "2023" = "red"), name = "Survey Cycle") +
    ggtitle(glue("Sample of {sample_size} Home-to-Work Routes and Home Locations by Year")) +
    theme_minimal()

  return(plot)
}

# Plot sample of routes and save to file
# Count of valid home-to-work records by survey cycle
person_2019_2023_ForHWloc_df %>% filter(
  home_to_work_miles > 0 & person_weight_rmove_only > 0
) %>% group_by(survey_cycle) %>% summarise(n = n())

map <- plot_sample_routes(person_2019_2023_ForHWloc_df, sample_size = 1000)
plot(map)
ggsave(glue("{working_dir}/sample_home_to_work_routes.png"), plot = map, width = 12, height = 12)


### Statistical summaries


# Summarize the count and distance weighted and unweighted by home county and survey cycle

# Summary statistics
home_to_work_miles_county <- person_2019_2023_ForHWloc_df %>%
  filter(
    home_to_work_crowfly_miles > 0 &
    person_weight_rmove_only > 0
  ) %>%
  group_by(survey_cycle, home_county_label) %>%
  summarise(
    n = n(),
    weighted_n = sum(person_weight_rmove_only, na.rm = TRUE),
    avg_crowfly = mean(home_to_work_crowfly_miles, na.rm = TRUE),
    avg_routed = mean(home_to_work_miles, na.rm = TRUE),
    avg_crowfly_wtd = weighted.mean(home_to_work_crowfly_miles, w = person_weight_rmove_only, na.rm = TRUE),
    avg_routed_wtd = weighted.mean(home_to_work_miles, w = person_weight_rmove_only, na.rm = TRUE)
  ) %>%
  # Transpose for better readability comparing 2019 vs 2023
  tidyr::pivot_wider(
    names_from = survey_cycle,
    values_from = c(n, weighted_n, avg_crowfly, avg_routed, avg_crowfly_wtd, avg_routed_wtd),
    names_sep = "_"
  )

home_to_work_miles_region <- person_2019_2023_ForHWloc_df %>%
  filter(
    home_to_work_crowfly_miles > 0 &
    person_weight_rmove_only > 0
  ) %>%
  group_by(survey_cycle) %>%
  summarise(
    n = n(),
    weighted_n = sum(person_weight_rmove_only, na.rm = TRUE),
    avg_crowfly = mean(home_to_work_crowfly_miles, na.rm = TRUE),
    avg_routed = mean(home_to_work_miles, na.rm = TRUE),
    avg_crowfly_wtd = weighted.mean(home_to_work_crowfly_miles, w = person_weight_rmove_only, na.rm = TRUE),
    avg_routed_wtd = weighted.mean(home_to_work_miles, w = person_weight_rmove_only, na.rm = TRUE)
  ) %>%
  # Transpose for better readability comparing 2019 vs 2023
  tidyr::pivot_wider(
    names_from = survey_cycle,
    values_from = c(n, weighted_n, avg_crowfly, avg_routed, avg_crowfly_wtd, avg_routed_wtd),
    names_sep = "_"
  )

# Combine county and region level summaries
home_to_work_miles <- bind_rows(
  home_to_work_miles_county %>%
    mutate(home_county_label = as.character(home_county_label)),
  home_to_work_miles_region %>%
    mutate(home_county_label = "Region")
) %>%
  # Calculate percent change
  mutate(
    pct_change_avg_routed = ifelse(
      avg_routed_2019 == 0, NA,
      (avg_routed_2023 - avg_routed_2019) / avg_routed_2019 * 100
    ),
    pct_change_avg_routed_wtd = ifelse(
      avg_routed_wtd_2019 == 0, NA,
      (avg_routed_wtd_2023 - avg_routed_wtd_2019) / avg_routed_wtd_2019 * 100
    )
  ) %>%
  # Calcuate share of total region
  mutate(
    share_of_region_2019 = n_2019 / sum(n_2019, na.rm = TRUE) * 100,
    share_of_region_2023 = n_2023 / sum(n_2023, na.rm = TRUE) * 100,
    share_of_region_wtd_2019 = weighted_n_2019 / sum(weighted_n_2019, na.rm = TRUE) * 100,
    share_of_region_wtd_2023 = weighted_n_2023 / sum(weighted_n_2023, na.rm = TRUE) * 100
  ) %>%
  # Rename columns for clarity
  rename(
    `(wtd) Work Distance 2019` = avg_routed_wtd_2019,
    `(wtd) Work Distance 2023` = avg_routed_wtd_2023,
    `(unwtd) Work Distance 2019` = avg_routed_2019,
    `(unwtd) Work Distance 2023` = avg_routed_2023,
    `Count 2019` = n_2019,
    `Count 2023` = n_2023,
    `(wtd) Count 2019` = weighted_n_2019,
    `(wtd) Count 2023` = weighted_n_2023,
    `%Change (unwtd)` = pct_change_avg_routed,
    `%Change (wtd)` = pct_change_avg_routed_wtd,
    # `Share of Region 2019 (unwtd)` = share_of_region_2019,
    # `Share of Region 2023 (unwtd)` = share_of_region_2023,
    # `Share of Region 2019 (wtd)` = share_of_region_wtd_2019,
    `Share of Region 2023 (wtd)` = share_of_region_wtd_2023
  )




cat("\n")
print("Home-to-work distance by home county (weighted):")
print(summary_home_to_work_miles)
cat("\n")


# Table of average distances by home county, weighted vs unweighted
home_to_work_miles %>%
  select(home_county_label, contains("Distance"), contains("%Change")) %>%
  # Calculate share of total region
  mutate(
    `Share of Region 2019 (wtd)` = `(wtd) Count 2019` / sum(`(wtd) Count 2019`, na.rm = TRUE) * 100,
    `Share of Region 2023 (wtd)` = `(wtd) Count 2023` / sum(`(wtd) Count 2023`, na.rm = TRUE) * 100
  ) %>%
  arrange(-`(wtd) Count 2023`) %>%
  print(width = Inf)

# Bar plot to visually show the changes
plot_home_to_work_distance <- ggplot(home_to_work_miles %>%
  mutate(home_county_label = factor(home_county_label,
    levels = c(setdiff(unique(home_county_label), "Region"), "Region"))),
  aes(x = reorder(home_county_label, ifelse(home_county_label == "Region", -Inf, `%Change (wtd)`)), 
      y = `%Change (wtd)`,
      width = `Share of Region 2023 (wtd)` / 50)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Percent Change in Average Home-to-Work Distance by Home County (Weighted)",
    subtitle = "Bar width scaled by share of region",
    x = "Home County",
    y = "Percent Change (%)"
  ) +
  theme_minimal()
ggsave(glue("{working_dir}/home_to_work_distance_percent_change_by_county.png"),
  plot = plot_home_to_work_distance, width = 8, height = 6)



# -------------------------
summary_home_to_work_miles_stats <- person_2019_2023_ForHWloc_df %>%
  group_by(survey_cycle) %>%
  summarise(
    mean_distance = round(mean(home_to_work_miles, na.rm = TRUE), 2),
    median_distance = round(median(home_to_work_miles, na.rm = TRUE), 2),
    max_distance = round(max(home_to_work_miles, na.rm = TRUE), 2)
  )

cat("\n")
print("Home-to-work distance summary (unweighted):")
print(summary_home_to_work_miles_stats)
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

# Pivot to compare 2019 vs 2023
cat("\n")
print("Home-to-work distance summary, by employment (2019 vs 2023):")
home_to_work_employment_pivot <- summary_home_to_work_miles_ByEmployment %>%
  select(survey_cycle, employment_label, mean_miles) %>%
  tidyr::pivot_wider(
    names_from = survey_cycle,
    values_from = c(mean_miles),
    names_sep = "_"
  ) %>%
  # Calculate change
  mutate(
    mean_miles_change = `2023` - `2019`,
    mean_miles_pct_change = ifelse(
       `2019` == 0, NA, 
      (`2023` - `2019`) / `2019` * 100)
  )
print(home_to_work_employment_pivot)
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


# Save the person_2019_2023_ForHWloc_df with calculated distances
saveRDS(person_2019_2023_ForHWloc_df, file = glue("{working_dir}/person_2019_2023_hws_distances.rds"))