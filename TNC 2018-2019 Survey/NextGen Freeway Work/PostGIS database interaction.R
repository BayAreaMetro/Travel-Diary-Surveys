library(RPostgreSQL)
library(rpostgis)
library(tidyverse)



password_in <- readLines("M:/Data/HomeInterview/TNC Survey/SFCTA Map Matching/Password.txt")
conn <- RPostgreSQL::dbConnect("PostgreSQL", host = "localhost",
                               dbname = "tncdata", user = "postgres", password = password_in)

# Check that PostGIS extension is installed

pgPostGIS(conn)

# Disconnect from PostgreSQL session

RPostgreSQL::dbDisconnect(conn)

