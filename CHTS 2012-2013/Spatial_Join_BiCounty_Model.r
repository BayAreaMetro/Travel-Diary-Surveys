# Spatial_Join_BiCounty_Model.R
# Script to join CHTS Survey with Bi-County TAZ system
# SI
# June 21, 2022

# Set working directory

wd <- "C:/Users/sisrael/Box/Modeling and Surveys/Share Data/Protected Data/George Naylor at WSP"
setwd(wd)

# Import libraries

library(pacman)
p_load(sf,tidyverse,sp,rgdal,crsuggest)

# Set up input and output directories
CHTS_places_in  <- "M:/Data/HomeInterview/2010/Data from CHTS/_Full Data Release/CSV and XLS Versions/LookUp_PLACE.csv"
CHTS_hhs_in     <- "M:/Data/HomeInterview/2010/Data from CHTS/_Full Data Release/CSV and XLS Versions/LookUp_Home.csv"
CHTS_persons_in <- "M:/Data/HomeInterview/2010/Data from CHTS/_Full Data Release/CSV and XLS Versions/LookUp_Person.csv"
Shapefile       <- "M:/Data/GIS layers/BiCountyTAZ/2022_06_16/BiCountyModel_TAZs_20220616.shp"
  
# Bring in bicounty shapefile and select TAZs and geometry columns
   
TAZ_shape <- st_read(Shapefile) %>%
  select(BCM_TAZ,geometry) 

# Bring in place file, ensuring origin and destination lat/long format is numeric
CHTS_places_temp <- read.csv(CHTS_places_in, stringsAsFactors = FALSE) %>% 
  mutate_at(.,c("dXCORD","dYCORD","oXCORD","oYCORD"),~as.numeric(.)) %>% 
  select("SAMPN", "PERNO", "PLANO","oXCORD","oYCORD","dXCORD","dYCORD") 

CHTS_places_o <- CHTS_places_temp %>% 
  select(-dXCORD,-dYCORD) %>% 
  filter(!(is.na(oXCORD)),!(is.na(oYCORD))) %>% 
  st_as_sf(., coords = c("oXCORD", "oYCORD"), crs = 4326) %>% 
  st_transform(., crs=st_crs(TAZ_shape))

CHTS_places_d <- CHTS_places_temp %>% 
  select(-oXCORD,-oYCORD) %>% 
  filter(!(is.na(dXCORD)),!(is.na(dYCORD))) %>% 
  st_as_sf(., coords = c("dXCORD", "dYCORD"), crs = 4326) %>% 
  st_transform(., crs=st_crs(TAZ_shape))
  
# Spatially join origin and destination to shapefile
  
CHTS_places_o <- st_join(CHTS_places_o,TAZ_shape, join=st_within,left=TRUE)%>%
  rename(O_BCM_TAZ=BCM_TAZ) %>% 
  as.data.frame(.) %>% select(-geometry)

CHTS_places_d <- st_join(CHTS_places_d,TAZ_shape, join=st_within,left=TRUE)%>%
  rename(D_BCM_TAZ=BCM_TAZ) %>% 
  as.data.frame(.) %>% select(-geometry)

# Join origin and destination datasets and only include those with TAZ values greater than zero

CHTS_places_bicounty <- full_join(CHTS_places_o,CHTS_places_d,by=c("SAMPN", "PERNO", "PLANO")) %>% 
  filter(O_BCM_TAZ>0 | D_BCM_TAZ>0)

## Now HH file

CHTS_hhs <- read.csv(CHTS_hhs_in, stringsAsFactors = FALSE) %>% 
  mutate_at(.,c("HXCORD","HYCORD"),~as.numeric(.)) %>% 
  select("SAMPN", "HXCORD", "HYCORD", "HCTFIP") %>% 
  filter(!(is.na(HXCORD)),!(is.na(HYCORD)))

CHTS_hhs_trans <- CHTS_hhs %>% 
  st_as_sf(., coords = c("HXCORD", "HYCORD"), crs = 4326) %>% 
  st_transform(., crs=st_crs(TAZ_shape))

CHTS_hhs_final <- st_join(CHTS_hhs_trans,TAZ_shape, join=st_within,left=TRUE)%>%
  rename(H_BCM_TAZ=BCM_TAZ) %>% 
  as.data.frame(.) %>% select(-geometry) %>% 
  filter(H_BCM_TAZ>0)

## Now the person file for work and school

CHTS_person_temp <- read.csv(CHTS_persons_in, stringsAsFactors = FALSE) %>% 
  mutate_at(.,c("WXCORD", "WYCORD","SXCORD","SYCORD"),~as.numeric(.)) %>% 
  select("SAMPN", "PERNO", "WXCORD", "WYCORD","SXCORD","SYCORD")

CHTS_person_work <- CHTS_person_temp %>% 
  select(-SXCORD,-SYCORD) %>% 
  filter(!(is.na(WXCORD)),!(is.na(WYCORD))) %>% 
  st_as_sf(., coords = c("WXCORD", "WYCORD"), crs = 4326) %>% 
  st_transform(., crs=st_crs(TAZ_shape))

CHTS_person_school <- CHTS_person_temp %>% 
  select(-WXCORD,-WYCORD) %>% 
  filter(!(is.na(SXCORD)),!(is.na(SYCORD))) %>% 
  st_as_sf(., coords = c("SXCORD", "SYCORD"), crs = 4326) %>% 
  st_transform(., crs=st_crs(TAZ_shape))

CHTS_work <- st_join(CHTS_person_work,TAZ_shape, join=st_within,left=TRUE)%>%
  rename(Work_BCM_TAZ=BCM_TAZ) %>% 
  as.data.frame(.) %>% select(-geometry)

CHTS_school <- st_join(CHTS_person_school,TAZ_shape, join=st_within,left=TRUE)%>%
  rename(School_BCM_TAZ=BCM_TAZ) %>% 
  as.data.frame(.) %>% select(-geometry)

CHTS_person_bicounty <- full_join(CHTS_work,CHTS_school,by=c("SAMPN", "PERNO"))%>% 
  filter(Work_BCM_TAZ>0 | School_BCM_TAZ>0)

# Write out final CSV files

write.csv(CHTS_places_bicounty,file.path(wd,"CHTS_places_bicounty_TAZ.csv"),row.names = FALSE)
write.csv(CHTS_hhs_final,file.path(wd,"CHTS_hhs_bicounty_TAZ.csv"),row.names = FALSE)
write.csv(CHTS_person_bicounty,file.path(wd,"CHTS_person_locations_bicounty_TAZ.csv"),row.names = FALSE)


