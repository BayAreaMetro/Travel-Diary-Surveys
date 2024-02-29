# Install and load necessary packages
library(osmdata)
library(tidyverse)

# Define bounding box coordinates for California counties
bbox <- matrix(c(-123.0, 37.0, -121.5, 38.5), byrow = TRUE,
               nrow = 2, dimnames = list(c('x', 'y'), c('min', 'max')))
location <- bbox %>% opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary")) %>%
  osmdata_sf()

# Retrieve OSM data for the specified area
osm_data <- location %>% osmdata_sf() 

# Define your bounding box coordinates (min and max lat/lon)
# These values are the mins/maxes for the Bay Area
lon_min <- -123.0
lon_max <- -121.5
lat_min <- 37.0
lat_max <- 38.5

# Get the bounding box
bay_areax <- matrix(
  data = c(lon_min,lat_min,lon_max,lat_max),
  nrow = 2,
  ncol = 2,
  byrow = T,
  dimnames = list(c('x','y'),c("min","max"))
)

query <- bay_areax %>% 
  opq(timeout = 500000) %>% 
  add_osm_feature(
    key = 'highway',
    value = c('motorway','trunk','primary')
  )

sf_data <- osmdata_sf(query)

%>%
  opq(timeout = 50) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary")) %>%
  osmdata_sf()

# Oakland example

oakland_major <- getbb(place_name = "Oakland") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary")) %>%
  osmdata_sf()

# Create the plot object, using the osm_lines element of tucson_major
street_plot <- ggplot() +
  geom_sf(data = oakland_major$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 0.2)
# Print the plot
street_plot

37.6336763,-122.355881,37.8854257,-122.1144203

tucson_major <- getbb(c(-122.355881,37.6336763,-122.1144203,37.8854257)) %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary")) %>%
  osmdata_sf()
