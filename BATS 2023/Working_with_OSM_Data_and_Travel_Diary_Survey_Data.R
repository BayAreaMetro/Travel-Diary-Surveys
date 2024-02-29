library(osmdata)
library(ggplot2)
library(sf)

# Define function to get bounding box for a place
get_osm_bbox <- function(place_name) {
  place <- opq(place_name)
  place <- add_osm_feature(place, key = 'admin_level', value = '6')
  place <- add_osm_feature(place, key = 'boundary', value = 'administrative')
  place <- osmdata_sf(place)
  if (length(place$osm_multipolygons) == 0) {
    message("No data retrieved for ", place_name)
    return(NULL)
  }
  bbox <- st_bbox(place$osm_multipolygons)
  return(bbox)
}

# Get bounding box for Alameda County
alameda_bbox <- get_osm_bbox("Alameda County, California")
if (is.null(alameda_bbox)) {
  stop("Failed to retrieve bounding box for Alameda County")
}

# Get bounding box for Contra Costa County
contra_costa_bbox <- get_osm_bbox("Contra Costa County, California")
if (is.null(contra_costa_bbox)) {
  stop("Failed to retrieve bounding box for Contra Costa County")
}

# Combining bounding boxes
combined_bbox <- c(min(alameda_bbox$xmin, contra_costa_bbox$xmin, na.rm = TRUE),
                   min(alameda_bbox$ymin, contra_costa_bbox$ymin, na.rm = TRUE),
                   max(alameda_bbox$xmax, contra_costa_bbox$xmax, na.rm = TRUE),
                   max(alameda_bbox$ymax, contra_costa_bbox$ymax, na.rm = TRUE))

# Print combined bounding box
print(combined_bbox)
