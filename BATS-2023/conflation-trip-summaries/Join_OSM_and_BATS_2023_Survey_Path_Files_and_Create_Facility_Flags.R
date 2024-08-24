# Join_OSM_and_BATS_2023_Survey_Path_Files_and_Create_Facility_Flags.R
# Join OSM equivalency file with conflated trips and create flags for links on each facility
# Get rid of scientific notation, bring in library

options(scipen = 999)
library(pacman)
p_load (tidyverse,sf)

# Input segment equivalency directory and read file

USERPROFILE       <- gsub("////","/", Sys.getenv("USERPROFILE"))
BOX_dir           <- file.path(USERPROFILE, "Box", "Modeling and Surveys","Surveys","Travel Diary Survey","Biennial Travel Diary Survey")
segments_in       <- file.path(BOX_dir,"Data","2023","Survey Conflation","osmid_facility_equivalence_lookup.csv")
facility_segments <- read.csv(segments_in)

# Excerpt conflation file data frame from its geopackage

file_temp         <- "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset"
conflation_file   <- file.path(file_temp,"WeightedDataset_08092024/OSM_match_v2/tds_conflation_results.gpkg")
conflation_df     <- st_read(conflation_file,layer = "matched_path_gdf")
con_attr_df       <- st_drop_geometry(conflation_df) %>% 
  select(trip_id,osmid,ref,name,highway)  

"M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Full Weighted 2023 Dataset/WeightedDataset_08092024/OSM_match_v2/tds_conflation_results.gpkg"

# Bring in weighted trip file, keep only trip_id and weight, for use at end of this script

hh_in             <- file.path(file_temp,"WeightedDataset_08092024/hh.csv")
person_in         <- file.path(file_temp,"WeightedDataset_08092024/person.csv")
trip_in           <- file.path(file_temp,"WeightedDataset_08092024/trip.csv")
hh                <- read.csv(hh_in) %>% 
  select(hh_id,participation_group,hh_weight_rmove_only)
person            <- read.csv(person_in)
trip              <- read.csv(trip_in) %>%
  #left_join(.,hh,by="hh_id") %>% 
  select(trip_id,trip_weight_rmove_only)


# Join segments to paths
# Retain all paths with non-na freeway values
# Summarize links by trip ID and facility
# Pivot from long to wide to better understand trip and facility relationship

joined <- left_join(con_attr_df,facility_segments,by="osmid") %>% 
  filter(!(is.na(Facility)))

sum_links <- joined %>% 
  group_by(trip_id,Facility) %>% 
  summarize(total=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Facility,values_from = total,values_fill = 0) 

# Reorder variables using select function, to match order used in project
# Convert trip values into 0,1 by recoding values above 1 to 1
# This step is to create a flag (0,1) for whether a given trip used a particular facility
# sr37_80_to_101=1 if any component segments of 37 are utilized
# i680_80_to_580=1 if i680_80_to_580_portion=1 (which is full extent minus bm_bridge) and/or bm_bridge=1
# bata_bridges=1 if any of the seven BATA bridges=1 (not including gg_bridge)
# The below list includes records with a zero rmove_only weight as they may be useful for other applications (maybe?);
# Any analysis applying weights will zero out records accordingly

final <- sum_links %>% 
  mutate_at(vars(-trip_id),~if_else(.>=1,1,as.double(.))) %>% 
  mutate(sr37_80_to_101=if_else((sr37_80_to_mare==1 | sr37_mare_to_121==1 | sr37_121_to_101==1),1,0),
         i680_80_to_580=if_else((i680_80_to_580_portion==1 | bm_bridge==1),1,0),
         bata_bridges=if_else(if_any(c(bay_bridge,sm_bridge,dum_bridge,rsr_bridge,
                                       carq_bridge,bm_bridge,ant_bridge), ~ .x==1),1,0))%>% 
  select(trip_id,
         bay_bridge,
         sm_bridge,
         dum_bridge,
         rsr_bridge,
         carq_bridge,
         bm_bridge,
         ant_bridge,
         gg_bridge,
         bata_bridges,
         sr37_80_to_mare,
         sr37_mare_to_121,
         sr37_121_to_101,
         sr37_80_to_101,
         i880_baybridge_to_237,
         #i680_80_to_580_portion,      # Remove this as it's only a temporary variable
         i680_80_to_580,
         i680_580_to_101,
         sr4_80_to_160,
         i580_hayward_to_sanjoaquin,
         i580_hayward_to_baybridge,
         i80_13_to_580,
         i80_580_to_Carquinez,
         i80_680_to_12) 

# Export CSV of trips for joining with trip file

write.csv(final,file.path(BOX_dir,"Data","2023","Survey Conflation","BATS 2023 Facility Use Booleans.csv"),row.names = FALSE)

# Summarize number of trips by facility, unweighted and weighted, for trips with >0 weight, output CSV

trips_with_weights <- final %>%
  left_join(.,trip,by="trip_id") %>% 
  filter(trip_weight_rmove_only>0) 

trips_by_facility_sum <- trips_with_weights %>%  
  pivot_longer(.,c(-trip_id,-trip_weight_rmove_only),names_to = "Facility",values_to = "Unweighted") %>% 
  mutate(Weighted=Unweighted*trip_weight_rmove_only) %>% 
  select(-trip_weight_rmove_only) %>% 
  group_by(Facility) %>% 
  summarize(Total_Unweighted_Trips=sum(Unweighted),Total_Weighted_Trips=sum(Weighted)) 

write.csv(trips_by_facility_sum,file.path(BOX_dir,"Data","2023","Survey Conflation","BATS 2023 Survey Trips Per Facility.csv"),row.names = FALSE)

