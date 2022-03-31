# TNC SFCTA Data Import and Conversion 022720.R
# Read in raw SFCTA data versions and write out R versions
# SFCTA processed the raw data into linked trips and chained tours
# SI

# Import Libraries

suppressMessages(library(tidyverse))

# Set up working directory

wd <- "M:/Data/HomeInterview/TNC Survey/Data/Final Version/SFCTA Processing/2_tour_extract/wt_wkday/"
setwd(wd)

# Bring in data

SFCTA_household    	<- read.csv(paste0(wd,"survey2018_hrecx.dat"),sep = ' ')
SFCTA_day           <- read.csv(paste0(wd,"survey2018_pdayx.dat"),sep = ' ')
SFCTA_person    	  <- read.csv(paste0(wd,"survey2018_precx.dat"),sep = ' ')
SFCTA_tour        	<- read.csv(paste0(wd,"survey2018_tourx.dat"),sep = ' ')
SFCTA_trip        	<- read.csv(paste0(wd,"survey2018_tripx.dat"),sep = ' ')

# Save out R versions

save(SFCTA_household, file = paste0(wd,"survey2018_hrecx.rdata"))
save(SFCTA_day, file = paste0(wd,"survey2018_pdayx.rdata"))
save(SFCTA_person, file = paste0(wd,"survey2018_precx.rdata"))
save(SFCTA_tour, file = paste0(wd,"survey2018_tourx.rdata"))
save(SFCTA_trip, file = paste0(wd,"survey2018_tripx.rdata"))


