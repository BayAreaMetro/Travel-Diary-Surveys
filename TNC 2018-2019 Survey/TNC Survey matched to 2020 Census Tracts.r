# TNC Survey matched to 2020 Census Tracts.r
# Remove point level data and match 2020 Census geographies

# Bring in libraries

library(tigris)
suppressMessages(library(tidyverse))

# Bring in files


# Make tracts call for 21-county region

bay_tracts <- tracts(
  state = "CA",
  county = c("Alameda","Contra Costa"),
  year = 2020
)
