# CHTS2012-2013 Build Bay Unlinked Trip File.r
# Script to build Bay Area unlinked trip file from CHTS places file
# SI
# May 4, 2020

# Import libraries

library(sf)
library(sp)
library(rgdal)
suppressMessages(library(tidyverse))

# Set up input and output directories. Weight file also includes the HHs to subset from state CHTS to Bay Area HHs for "typical travel"

wd <- "M:/Data/HomeInterview/2010/Data from CHTS/_Full Data Release/CSV and XLS Versions/"
setwd(wd)

Weight_location       <- "M:/Data/HomeInterview/2010/Data from CHTS/Processed by PB 021715/Household_typical_weekday_weight.csv"
Unlinked_location     <- "M:/Data/HomeInterview/2010/Data from CHTS/_Full Data Release/CSV and XLS Versions/CHTS_Place_Data.Rdata"
TAZ_location          <- "M:/Data/GIS layers/TM1_taz/taz1454-utm.shp"
Place_detail_location <- "M:/Data/HomeInterview/2010/Data from CHTS/_Full Data Release/CSV and XLS Versions/LookUp_PLACE.csv"
Skim_location         <- "//model2-d/Model2D-Share/Projects/2015_TM152_IPA_08/skims/hwyskm_csv/HWYSKMAM.csv"
Vehicle_location      <- "M:/Data/HomeInterview/2010/Data from CHTS/_Full Data Release/CSV and XLS Versions/Deliv_VEH.csv"


# Bring in CHTS unlinked trips and MTC TAZ1454 shapefile and select TAZ and geometry columns, geocode locations to TAZs
# Join wtih weight file to filter out from the full dataset only the Bay Area HHs traveling during "typical conditions"
# Bring in skim distance and vehicle files as well

weight <- read.csv(Weight_location,as.is = TRUE)

load(Unlinked_location) 

unlinked <- left_join(CHTS_unlinked,weight,by="SAMPN") %>% 
  filter(!is.na(TYP_WKDY_WGT)) %>% 
  select(SAMPN,PERNO,PLANO,MODE,TRIPNO,VEHNO,TOTTR,HHMEM,NONHH)

TAZ <- st_read(TAZ_location) %>%
  select(TAZ1454,geometry)

place_detail <- left_join(read.csv(Place_detail_location,as.is=TRUE),weight,by="SAMPN") %>% 
  filter(!is.na(TYP_WKDY_WGT)) %>% 
  select(SAMPN,PERNO,PLANO,dXCORD,dYCORD,oXCORD,oYCORD)

skim      <- read.csv(Skim_location,header=TRUE) %>% 
  select(orig,dest,DISTDA) %>% 
  rename(Origin_TAZ=orig,Destination_TAZ=dest)

vehicle   <- read.csv(Vehicle_location,header=TRUE) %>% 
  select(SAMPN,VEHNO,BODY,VEHT)

# Separate origin and destination into two files, remove missing data to make join later easier

place_origin <- place_detail %>%
  select(SAMPN,PERNO,PLANO,oXCORD,oYCORD) %>%
  filter(!is.na(oXCORD))

place_destination <- place_detail %>%
  select(SAMPN,PERNO,PLANO,dXCORD,dYCORD)%>%
  filter(!is.na(dXCORD))

# Assign projection for origin/destination and then convert projection into what's used in Bay Area - NAD83 / UTM zone 10N

place_origin_space <- st_as_sf(place_origin, coords = c("oXCORD", "oYCORD"), crs = 4326)
place_origin_space <- st_transform(place_origin_space,crs = 26910)

place_destination_space <- st_as_sf(place_destination, coords = c("dXCORD", "dYCORD"), crs = 4326)
place_destination_space <- st_transform(place_destination_space,crs = 26910)

# Convert TAZ shape to same project as origins, now NAD83 / UTM zone 10N

TAZ_shape <- st_transform(TAZ,crs = st_crs(place_origin_space))

# Spatially join origin and destination to shapefile
# Remove geometry columns from origin/destination for join back to datasets

origin <- as.data.frame(st_join(place_origin_space,TAZ_shape, join=st_within,left=TRUE))%>%
  rename(Origin_TAZ=TAZ1454) %>% 
  select(-geometry)

destination <- as.data.frame(st_join(place_destination_space,TAZ_shape, join=st_within,left=TRUE))%>%
  rename(Destination_TAZ=TAZ1454) %>% 
  select(-geometry)

# Join origin and destination TAZs to original place_detail file
# Join unlinked trips to the geocoded origins and destinations
# Filter out TRIPNO="NA", which is associated the first place of the day (the home location, before traveling)

place_detail <- left_join(place_detail,origin,by=c("SAMPN","PERNO","PLANO")) 
place_detail <- left_join(place_detail,destination, by=c("SAMPN","PERNO","PLANO"))
bay_unlinked <- left_join(unlinked,place_detail,by=c("SAMPN","PERNO","PLANO")) %>% 
  filter(!is.na(TRIPNO))

# Join skim file and vehicle file, append weight information

bay_unlinked <- left_join(bay_unlinked,skim,by=c("Origin_TAZ","Destination_TAZ"))
bay_unlinked <- left_join(bay_unlinked,vehicle,by=c("SAMPN","VEHNO"))
bay_unlinked <- left_join(bay_unlinked,weight,by="SAMPN") 

# Recode text values from the category codes into a new variable
# Rename some variables, and select out a lean final file for export 

bay_unlinked <- bay_unlinked %>% mutate(
  body_text=case_when(
  BODY==1  ~ "SEDAN (4-door)",
  BODY==2  ~ "SUV",
  BODY==3  ~ "PICK-UP TRUCK",
  BODY==4  ~ "COUPE (2-door)",
  BODY==5  ~ "CONVERTIBLE",  
  BODY==6  ~ "HATCHBACK",
  BODY==7  ~ "WAGON",
  BODY==8  ~ "MINIVAN",
  BODY==9  ~ "VAN", 
  BODY==10 ~ "OTHER KIND OF TRUCK", 
  BODY==11 ~ "RECREATIONAL VEHICLE",
  BODY==12 ~ "MOTORCYCLE", 
  BODY==13 ~ "MOPED/SCOOTER (e.g. VESPA)",
  BODY==97 ~ "OTHER, SPECIFY (WATERCRAFT, CROSSOVER, ETC)",
  BODY==98 ~ "Don't Know", 
  BODY==99 ~ "Refused"
  ),
  fuel_text=case_when(
    VEHT==1  ~ "Hybrid Vehicle",
    VEHT==2  ~ "Gasoline Only Vehicle",
    VEHT==3  ~ "Diesel Only Vehicle",
    VEHT==4  ~ "Plug In Hybrid Electric Vehicle",
    VEHT==5  ~ "CNG",
    VEHT==6  ~ "Electric Only",
    VEHT==7  ~ "OTHER",
    VEHT==9  ~ "Don't Know / Refused"
  ),
  mode_text=case_when(
  MODE==1  ~ "Walk",
  MODE==2  ~ "Bike",
  MODE==3  ~ "Wheelchair / Mobility Scooter",
  MODE==4  ~ "Other Non-Motorized", 
  MODE==5  ~ "Auto / Van / Truck Driver",
  MODE==6  ~ "Auto / Van / Truck Passenger",
  MODE==7  ~ "Carpool / Vanpool",
  MODE==8  ~ "Motorcycle / Scooter / Moped",
  MODE==9  ~ "Taxi / Hired Car / Limo",
  MODE==10 ~ "Rental Car / Vehicle",
  MODE==11 ~ "Private shuttle (SuperShuttle, employer, hotel, etc.)",
  MODE==12 ~ "Greyhound Bus",
  MODE==13 ~ "Plane",
  MODE==14 ~ "Other Private Transit",
  MODE==15 ~ "Local Bus, Rapid Bus", 
  MODE==16 ~ "Express Bus / Commuter Bus (AC Transbay, Golden Gate Transit, etc)", 
  MODE==17 ~ "Premium Bus", 
  MODE==18 ~ "School Bus", 
  MODE==19 ~ "Public Transit Shuttle (Emery Go Round, etc.)", 
  MODE==20 ~ "AirBART", 
  MODE==21 ~ "Dial-a-Ride / Paratransit (Access Services, etc.)", 
  MODE==22 ~ "Amtrak Bus", 
  MODE==23 ~ "Other Bus",
  MODE==24 ~ "BART",
  MODE==25 ~ "ACE, Amtrak, Caltrain",
  MODE==26 ~ "Muni Metro, VTA Light Rail", 
  MODE==27 ~ "Street Car / Cable Car",
  MODE==28 ~ "Other Rail",      
  MODE==29 ~ "Ferry / Boat"
  )
) %>% 
  rename(body_code=BODY,fuel_code=VEHT,skim_distance=DISTDA,weight=TYP_WKDY_WGT,mode_code=MODE,
         party_size=TOTTR,household_members=HHMEM,non_hh_members=NONHH) %>% 
  select(SAMPN,PERNO,PLANO,TRIPNO,mode_code,mode_text,VEHNO,body_code,body_text,fuel_code,fuel_text,
         Origin_TAZ,Destination_TAZ,skim_distance,party_size,household_members,non_hh_members,weight)

# Rename final data frame and save files in R format and CSV 

CHTS_Bay_Unlinked_Trips <- bay_unlinked
save(CHTS_Bay_Unlinked_Trips,file = "CHTS_Bay_Unlinked_Trips.Rdata")
write.csv(CHTS_Bay_Unlinked_Trips,"CHTS_Bay_Unlinked_Trips.csv",row.names = FALSE, quote = TRUE)



