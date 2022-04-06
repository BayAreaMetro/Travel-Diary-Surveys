# Analyses.R
# Interact the OSM freeway trip traces with TNC Survey data

# Libraries

library(tidyverse)
library(json64)
library(geojson)

# Locations

TNC_Survey_Paths_in <- "M:/Data/HomeInterview/TNC Survey/SFCTA Map Matching/TNC_Survey_Paths.RData"

# Bring in paths data

Paths <- load(TNC_Survey_Paths_in)

# Bring in json data
