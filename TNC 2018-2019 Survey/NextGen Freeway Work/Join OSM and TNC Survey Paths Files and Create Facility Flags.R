# Join OSM and TNC Survey Paths Files and Create Facility Flags.R
# Compile all the freeway files and create flags for links on each facility

library(tidyverse)

# Input OSM segments

dir1        <- "M:/Data/HomeInterview/TNC Survey/SFCTA Map Matching/NextGen Freeway Project"
segment_in  <- file.path(dir1,"TNC_Survey_OSM_Network")

# Individual segment files
# Append facility name associated with links

Al_SF_80_PlazaTo101 <- read.csv(file.path(segment_in,"Al_SF_80_PlazaTo101.csv")) %>% 
  mutate(facility_links="Al_SF_80_PlazaTo101")

SF_101_80ToSM <- read.csv(file.path(segment_in,"SF_101_80ToSM.csv")) %>% 
  mutate(facility_links="SF_101_80ToSM")

SF_280_StartToSM <- read.csv(file.path(segment_in,"SF_280_StartToSM.csv")) %>% 
  mutate(facility_links="SF_280_StartToSM")

SM_101_SFToSC <- read.csv(file.path(segment_in,"SM_101_SFToSC.csv")) %>% 
  mutate(facility_links="SM_101_SFToSC")

SM_280_SFToSC <- read.csv(file.path(segment_in,"SM_280_SFToSC.csv")) %>% 
  mutate(facility_links="SM_280_SFToSC")

SC_101_SMTo680 <- read.csv(file.path(segment_in,"SC_101_SMTo680.csv")) %>% 
  mutate(facility_links="SC_101_SMTo680")

SC_101_680ToGilroy <- read.csv(file.path(segment_in,"SC_101_680ToGilroy.csv")) %>% 
  mutate(facility_links="SC_101_680ToGilroy")

SC_237_101To880 <- read.csv(file.path(segment_in,"SC_237_101To880.csv")) %>% 
  mutate(facility_links="SC_237_101To880")

SC_280_SMTo101 <- read.csv(file.path(segment_in,"SC_280_SMTo101.csv")) %>% 
  mutate(facility_links="SC_280_SMTo101")

Al_SC_680_101To580 <- read.csv(file.path(segment_in,"Al_SC_680_101To580.csv")) %>% 
  mutate(facility_links="Al_SC_680_101To580")

Al_SC_880_101To238 <- read.csv(file.path(segment_in,"Al_SC_880_101To238.csv")) %>% 
  mutate(facility_links="Al_SC_880_101To238")

Al_880_238ToPlaza <- read.csv(file.path(segment_in,"Al_880_238ToPlaza.csv")) %>% 
  mutate(facility_links="Al_880_238ToPlaza")

Al_580_SanJoaquinTo238 <- read.csv(file.path(segment_in,"Al_580_SanJoaquinTo238.csv")) %>% 
  mutate(facility_links="Al_580_SanJoaquinTo238")

Al_580_238To80 <- read.csv(file.path(segment_in,"Al_580_238To80.csv")) %>% 
  mutate(facility_links="Al_580_238To80")

Al_80_580ToPlaza <- read.csv(file.path(segment_in,"Al_80_580ToPlaza.csv")) %>% 
  mutate(facility_links="Al_80_580ToPlaza")

Al_CC_80_4To580 <- read.csv(file.path(segment_in,"Al_CC_80_4To580.csv")) %>% 
  mutate(facility_links="Al_CC_80_4To580")

CC_Al_24_680To580 <- read.csv(file.path(segment_in,"CC_Al_24_680To580.csv")) %>% 
  mutate(facility_links="CC_Al_24_680To580")

CC_Al_680_4To580 <- read.csv(file.path(segment_in,"CC_Al_680_4To580.csv")) %>% 
  mutate(facility_links="CC_Al_680_4To580")

CC_4_80To680 <- read.csv(file.path(segment_in,"CC_4_80To680.csv")) %>% 
  mutate(facility_links="CC_4_80To680")

Sol_80_YoloToCarquinez <- read.csv(file.path(segment_in,"Sol_80_YoloToCarquinez.csv")) %>% 
  mutate(facility_links="Sol_80_YoloToCarquinez")

North_37_101To80 <- read.csv(file.path(segment_in,"North_37_101To80.csv")) %>% 
  mutate(facility_links="North_37_101To80")

Mar_Son_101_12To580 <- read.csv(file.path(segment_in,"Mar_Son_101_12To580.csv")) %>% 
  mutate(facility_links="Mar_Son_101_12To580")

# Bind all segments together into a single file to merge with paths file
# Receiving error messages when trying to bind them all in one step, so did it in increments

segment1 <- bind_rows(
  Al_SF_80_PlazaTo101,
  SF_101_80ToSM,
  SF_280_StartToSM,
  SM_101_SFToSC,
  SM_280_SFToSC,
  SC_101_SMTo680,
  SC_101_680ToGilroy,
  SC_237_101To880,
  SC_280_SMTo101,
  Al_SC_680_101To580,
  Al_SC_880_101To238,
  Al_880_238ToPlaza)

segment2 <- rbind(
  Al_580_SanJoaquinTo238,
  Al_580_238To80)

segment3 <- bind_rows(
  Al_80_580ToPlaza,
  Al_CC_80_4To580,
  CC_Al_24_680To580,
  CC_Al_680_4To580,
  CC_4_80To680,
  Sol_80_YoloToCarquinez,
  North_37_101To80,
  Mar_Son_101_12To580)

temp <- rbind(segment1,segment2)

all_segments <- rbind(temp,segment3)
