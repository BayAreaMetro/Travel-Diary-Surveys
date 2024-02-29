# Telecommute Distribution and WFH Rate.R
# Summarize TNC survey data telecommute
# SI

# Import Libraries

suppressMessages(library(tidyverse))
library(formattable)

# Set up working directory

wd <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019/Analysis"
setwd(wd)

# TNC survey files locations

temp1                  <- "M:/Data/HomeInterview/Bay Area Travel Study 2018-2019"
temp2                  <- file.path(temp1,"Data/Final Version with Imputations/Final Updated Dataset as of 10-18-2021")
file_location          <- file.path(temp2,"RSG_HTS_Oct2021_bayarea")
person_location        <- file.path(file_location,"person.tsv")
trip_location          <- file.path(file_location,"trip.tsv")
trip_linked_location   <- file.path(file_location,"trip_linked.tsv")
hh_location            <- file.path(file_location,"hh.tsv")
day_location           <- file.path(file_location,"day.tsv")

# Bring in files

person          <- read_tsv(person_location,col_names=TRUE)
trip            <- read_tsv(trip_location,col_names=TRUE)      
linked_trip     <- read_tsv(trip_linked_location,col_names=TRUE)
household       <- read_tsv(hh_location,col_names=TRUE)
day             <- read_tsv(day_location,col_names=TRUE)


# Summarize the number of number of work or work-related trips by person/day combination

work_trips <- linked_trip %>%
  mutate(work_trip_flag=0,
         work_trip_flag=if_else(d_purpose_category_imputed %in% c(2,3),1,work_trip_flag)) %>% 
  group_by(hh_id,person_num,day_num) %>% 
  summarize(num_work_trips=sum(work_trip_flag))

# Join work trips with day file and summarize telework distribution by people traveling to work and not
# Join person worker flag, filter to include workers and telework time>0
# Convert to percentiles (*100)
# Chart results for people making no work trips

person_joiner <- person %>% filter(is_active_participant==1) %>% 
  select(hh_id,person_num, worker, employment) 


joined <- left_join(day,work_trips,by=c("hh_id","person_num","day_num")) %>% 
  left_join(.,person_joiner,by=c("hh_id","person_num")) %>%
  mutate(
    telecommute=case_when(
      telework_time==0                                ~ "0_0 minutes",
      telework_time>0 & telework_time<60              ~ "1_1 to 59 minutes",
      telework_time>=60 & telework_time<120           ~ "2_60 to 119 minutes",
      telework_time>=120 & telework_time<180          ~ "3_120 to 179 minutes",
      telework_time>=180 & telework_time<240          ~ "4_180 to 239 minutes",
      telework_time>=240 & telework_time<300          ~ "5_240 to 299 minutes",
      telework_time>=300 & telework_time<360          ~ "6_300 to 359 minutes",
      telework_time>=360 & telework_time<420          ~ "7_360 to 419 minutes",
      telework_time>=420 & telework_time<480          ~ "8_420 to 479 minutes",
      telework_time>=480                              ~ "9_480+ minutes",
      TRUE                                            ~ "10_No value given")) %>% 
  mutate(num_work_trips=if_else(is.na(num_work_trips),0,num_work_trips)) %>%        # Assigns zero work trips to people who made no trips at all
  filter(worker==1)                                                                 # Filter for workers only (worker==1 or employment %in% c(1,2,3))

zero_work_trips <- joined %>% 
  filter(num_work_trips==0, telework_time>0) %>%    
  group_by(telecommute) %>% 
  summarize(total=sum(daywt_alladult_wkday)) %>% 
  ungroup() %>% 
  mutate(share=100*round(total/sum(total),3))

ggplot(zero_work_trips, aes(x = telecommute,y=share)) +
  geom_col(fill = "blue", color = "black", alpha = 0.7) +
  geom_text(aes(label = formattable::digits(share,digits=1), vjust = -0.5)) +
  labs(title = "Telecommute time for zero work trips, worker==1, telecommute time>0",
       x = "Telecommute Bins",
       y = "Share (Percentage)")

# Share work at home
# If telework time is missing, change to zero

work_location <- joined %>% 
  mutate(telework_time=if_else(is.na(telework_time),0,telework_time)) %>% 
  mutate(
    worked_in_office       =if_else(num_work_trips>0,daywt_alladult_wkday,0),
    worked_from_home_LT240 =if_else(num_work_trips==0 & (telework_time>=1 & telework_time<240),daywt_alladult_wkday,0),
    worked_from_home_240p  =if_else(num_work_trips==0 & telework_time>=240,daywt_alladult_wkday,0),
    not_worked             =if_else(num_work_trips==0 & telework_time==0,daywt_alladult_wkday,0),
    total_worked           =worked_in_office+worked_from_home_LT240+worked_from_home_240p,
    total_workers          =total_worked+not_worked
  ) 

final <- work_location %>% 
  summarize(
    worked_in_office=sum(worked_in_office),
    worked_from_home_LT240=sum(worked_from_home_LT240),
    worked_from_home_240p=sum(worked_from_home_240p),
    not_worked=sum(not_worked),
    total_worked=sum(total_worked),
    total_workers=sum(total_workers)
  ) %>% 
  mutate(
    share_worked_in_office       =worked_in_office/total_worked,
    share_worked_from_home_LT240 =worked_from_home_LT240/total_worked,
    share_worked_from_home_240p  =worked_from_home_240p/total_worked)

# Look only at full-time workers

work_location2 <- joined %>% 
  filter(employment==1) %>% 
  mutate(telework_time=if_else(is.na(telework_time),0,telework_time)) %>% 
  mutate(
    worked_in_office  =if_else(num_work_trips>0,daywt_alladult_wkday,0),
    worked_from_home  =if_else(num_work_trips==0 & telework_time>=1,daywt_alladult_wkday,0),
    not_worked        =if_else(num_work_trips==0 & telework_time==0,daywt_alladult_wkday,0),
    total_worked      =worked_in_office+worked_from_home,
    total_workers     =total_worked+not_worked
  ) 

final2 <- work_location2 %>% 
  summarize(
    worked_in_office=sum(worked_in_office),
    worked_from_home=sum(worked_from_home),
    not_worked=sum(not_worked),
    total_worked=sum(total_worked),
    total_workers=sum(total_workers)
  ) %>% 
  mutate(
    share_worked_in_office=worked_in_office/total_worked,
    share_worked_from_home=worked_from_home/total_worked)

# Non-imputed version

work_trips3 <- linked_trip %>%
  mutate(work_trip_flag=0,
         work_trip_flag=if_else(d_purpose_category %in% c(2,3),1,work_trip_flag)) %>% 
  group_by(hh_id,person_num,day_num) %>% 
  summarize(num_work_trips=sum(work_trip_flag))


joined3 <- left_join(day,work_trips3,by=c("hh_id","person_num","day_num")) %>% 
  left_join(.,person_joiner,by=c("hh_id","person_num")) %>%
  mutate(
    telecommute=case_when(
      telework_time==0                                ~ "0_0 minutes",
      telework_time>0 & telework_time<60              ~ "1_1 to 59 minutes",
      telework_time>=60 & telework_time<120           ~ "2_60 to 119 minutes",
      telework_time>=120 & telework_time<180          ~ "3_120 to 179 minutes",
      telework_time>=180 & telework_time<240          ~ "4_180 to 239 minutes",
      telework_time>=240 & telework_time<300          ~ "5_240 to 299 minutes",
      telework_time>=300 & telework_time<360          ~ "6_300 to 359 minutes",
      telework_time>=360 & telework_time<420          ~ "7_360 to 419 minutes",
      telework_time>=420 & telework_time<480          ~ "8_420 to 479 minutes",
      telework_time>=480                              ~ "9_480+ minutes",
      TRUE                                            ~ "10_No value given")) %>% 
  mutate(num_work_trips=if_else(is.na(num_work_trips),0,num_work_trips)) %>%        # Assigns zero work trips to people who made no trips at all
  filter(worker==1)

work_location3 <- joined3 %>% 
  filter(employment %in% c(1,3)) %>% 
  mutate(telework_time=if_else(is.na(telework_time),0,telework_time)) %>% 
  mutate(
    worked_in_office  =if_else(num_work_trips>0,daywt_alladult_wkday,0),
    worked_from_home  =if_else(num_work_trips==0 & telework_time>=1,daywt_alladult_wkday,0),
    not_worked        =if_else(num_work_trips==0 & telework_time==0,daywt_alladult_wkday,0),
    total_worked      =worked_in_office+worked_from_home,
    total_workers     =total_worked+not_worked
  ) 

final3 <- work_location3 %>% 
  summarize(
    worked_in_office=sum(worked_in_office),
    worked_from_home=sum(worked_from_home),
    not_worked=sum(not_worked),
    total_worked=sum(total_worked),
    total_workers=sum(total_workers)
  ) %>% 
  mutate(
    share_worked_in_office=worked_in_office/total_worked,
    share_worked_from_home=worked_from_home/total_worked)

View(final3)

