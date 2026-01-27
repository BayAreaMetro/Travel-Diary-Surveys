import pandas as pd
import os
from os.path import join as _join
import numpy as np
import yaml
import geopandas as gpd
import re
import scipy
import seaborn as sns
import subprocess
import functools
import openmatrix as omx
from . import utilities
from .utilities import map_point_to_reference
from .utilities import apply_transformations

class SurveyReader:
    def __init__(self, config):
        # If config is a path, load the YAML
        if isinstance(config, str):
            with open(config, 'r') as f:
                self.config = yaml.safe_load(f)
        else:
            self.config = config
        self.data = None
        self.trips = None
        self.person = None
        self.hh = None
        self.tours = None
        self.day = None



    
    def read_data(self):
        # Implement logic to read survey data from the specified file
        if 'trip_file' in self.config:
            self.trips = pd.read_csv(_join(self.config['survey_dir'], self.config['trip_file']))
        else:
            raise ValueError("trip_file must be specified in the configuration.")
        if 'person_file' in self.config:
            self.person = pd.read_csv(_join(self.config['survey_dir'], self.config['person_file']))
        else:
            print("Warning: person_file not specified in config; person data will not be loaded.")
        if 'household_file' in self.config:
            self.hh = pd.read_csv(_join(self.config['survey_dir'], self.config['household_file']))
        else:
            print("Warning: household_file not specified in config; household data will not be loaded.")
        if 'day_file' in self.config:
            self.day = pd.read_csv(_join(self.config['survey_dir'], self.config['day_file']))
        else:
            print("Warning: day_file not specified in config; day data will not be loaded.")
        if 'tours_file' in self.config:
            self.tours = pd.read_csv(_join(self.config['survey_dir'], self.config['tours_file']))
        else:
            print("Warning: tours_file not specified in config; tours data will not be loaded.")
        return

    def load(self):
        print("SurveyReader.load() called")
        try:
            read_data = self.config['read_processed_data']
        except Exception as e:
            print(f"Exception reading config['read_processed_data']: {e}")
            read_data = False
        print(f"read_data = {read_data}")
        if read_data:
            print("Calling read_processed_data()...")
            self.read_processed_data()
        else:
            print("Calling read_data()...")
            self.read_data()
            print("Calling link_trips()...")
            self.link_trips()
            print("Calling summarize_trip_modes()...")
            self.summarize_trip_modes()
            print("Calling preprocess_data()...")
            self.preprocess_data()
            print("Calling save_processed_data()...")
            self.save_processed_data()

    def preprocess_data(self):
        """Preprocess survey data according to the configuration settings. First add descriptive labels from the codebook. Geocode if config is set to True.
        apply data transformation as specified in preprocessor configurations."""
        #add descriptive labels to categorical variables
        if 'codebook' in self.config:
            labels = pd.read_excel(_join(self.config['survey_dir'],self.config['codebook']),sheet_name = self.config['codebook_sheet'])
            #FIXME: hardcoded column names in codebook; make them configurable?
            grouped_labels = labels.groupby('variable')
            value_maps ={var: dict(zip(group['value'], group['label']))
                for var, group in grouped_labels}

            for col, value_map in value_maps.items():
                if col in self.trips.columns:
                    self.trips[col] = self.trips[col].map(value_map)
                if self.person is not None:
                    if col in self.person.columns:
                        self.person[col] = self.person[col].map(value_map)
                if self.hh is not None:
                    if col in self.hh.columns:
                        self.hh[col] = self.hh[col].map(value_map)
                if self.day is not None:
                    if col in self.day.columns:
                        self.day[col] = self.day[col].map(value_map)

        #map survey locations to model TAZs
        trip_len = len(self.trips)
        if self.config['geocode'] == True:
            taz = gpd.read_file(self.config['taz_shapefile']).to_crs('EPSG:4326')
        #geocode origins and destinations of trips
        #FIXME: These functions use hardcoded variable names; make them configurable
        if self.config['geocode'] == True:
            self.trips = map_point_to_reference(self.trips, point_field_name='OTAZ', point_lat_col='o_lat', point_lon_col='o_lon',reference_field = 'TAZ',reference_gdf = taz)
            self.trips = map_point_to_reference(self.trips, point_field_name='DTAZ', point_lat_col='d_lat', point_lon_col='d_lon',reference_field = 'TAZ',reference_gdf = taz)
        trip_xform = self.config.get('trip_data_xform')
        if trip_xform is not None:
            self.trips = apply_transformations(self.trips, trip_xform, self.config)
        else:
            print("Warning: No trip data transformations applied, or trip data not available.")

        person_xform = self.config.get('person_data_xform')
        if person_xform is not None:
            self.person = apply_transformations(self.person, person_xform, self.config)
        else:
            print("Warning: No person data transformations applied, or person data not available.")

        hh_xform = self.config.get('hh_data_xform')
        if hh_xform is not None:
            self.hh = apply_transformations(self.hh, hh_xform, self.config)
        else:
            print("Warning: No household data transformations applied, or household data not available.")

        tour_xform = self.config.get('tour_data_xform')
        if tour_xform is not None and self.tours is not None:
            self.tours = apply_transformations(self.tours, tour_xform, self.config)
        else:
            print("Warning: No tour data transformations applied, or tours data not available.")

        day_xform = self.config.get('day_data_xform')
        if day_xform is not None and self.day is not None:
            self.day = apply_transformations(self.day, day_xform, self.config)
        else:
            print("Warning: No day data transformations applied, or day data not available.")
               #append land use data information
             
        
        
        
        if self.config['geocode'] == True and not self.config['obs']:
            #geocode home locations
            self.hh = map_point_to_reference(self.hh, point_field_name='HHTAZ', point_lat_col='home_lat', point_lon_col='home_lon',reference_field = 'TAZ', reference_gdf = taz)

            
            if len(self.trips) != trip_len:
                raise ValueError(f"Geocoding home loc failed: expected {trip_len} rows, got {len(self.trips)}")
            #geocode work locations
            self.person = map_point_to_reference(self.person, point_field_name='WorkTAZ', point_lat_col='work_lat', point_lon_col='work_lon',reference_field = 'TAZ', reference_gdf = taz)
            
            #geocode school locations
            self.person = map_point_to_reference(self.person, point_field_name='SchoolTAZ', point_lat_col='school_lat', point_lon_col='school_lon',reference_field = 'TAZ', reference_gdf = taz)
            

            if len(self.trips) != trip_len:
                raise ValueError(f"Geocoding school loc failed: expected {trip_len} rows, got {len(self.trips)}")
        return

    def link_trips(self, weight_cols=['trip_weight','trip_weight_rmove_only']):
        print("Entered link_trips()")
        """Link trips based on the logic provided in the original notebook create by Yun Ma https://github.com/vta/BATS-2023/blob/main/trip_linkage/0608/script/build_linked_trips_main_type_weight_version_vta_yma_060825.ipynb.
        # approach:
        # 1. Sort the leg-based trip table by person_id and trip_num.
        # 2. Loop by person_id and when the first destination purpose is not ‘change mode’, a link trip is separated;
        # 3. Moreover, when the trip_day was changed, a link trip is separated as well. In this case, the dest_purpose was updated from the original misimputed “change-mode” to the real reported purpose.
        # 3. Use the loop’s first leg’s origin-purpose, depart-time, o-lat, o-lon as the linked trip’s origin’s attributes;
        # 4. Use the loop’s last leg’s dest-purpose, arrive-time, d-lat, d-lon as the linked trip’s destination’s attributes;
        # 5. The mode type with the most priority among all legs was picked as the linked trip’s mode type. The prioirity of mode_type can be customized in the correspondence table.
        # 6. The mode_x with the most priority among all legs was picked as the linked trip’s mode_x. The prioirity of mode_x can be customized in the correspondence table.
        # 7. The transit access/egress with the most priority among all legs was picked as the linked trip’s transit access/egress. The prioirity of transit access/egress can be customized in the correspondence table.
        # 8. The weight of the linked trip used the the leg of main mode_type's weight."""
        # Read correspondence tables
        lookup_o_purpose_reported = pd.read_csv(self.config['lookup_o_purpose_reported'])
        lookup_d_purpose_reported = pd.read_csv(self.config['lookup_d_purpose_reported'])
        lookup_mode = pd.read_csv(self.config['lookup_mode'])
        lookup_mode_type = pd.read_csv(self.config['lookup_mode_type'])
        lookup_transit_access = pd.read_csv(self.config['lookup_transit_access'])
        lookup_transit_egress = pd.read_csv(self.config['lookup_transit_egress'])

        # Read raw dataset
        trip = self.trips.copy()
        # Lookup attributes
        trip = trip.merge(lookup_mode[['mode_1', 'mode_1_priority']], how='left')
        trip = trip.merge(lookup_mode[['mode_2', 'mode_2_priority']], how='left')
        trip = trip.merge(lookup_mode[['mode_3', 'mode_3_priority']], how='left')
        trip = trip.merge(lookup_mode[['mode_4', 'mode_4_priority']], how='left')
        trip = trip.merge(lookup_mode_type[['mode_type', 'mode_type_priority']], how='left')
        trip = trip.merge(lookup_transit_access[['transit_access', 'transit_access_priority']], how='left')
        trip = trip.merge(lookup_transit_egress[['transit_egress', 'transit_egress_priority']], how='left')
        trip = trip.merge(lookup_d_purpose_reported[['d_purpose_reported', 'd_purpose_reported_priority']], how='left')
        trip = trip.merge(lookup_o_purpose_reported[['o_purpose_reported', 'o_purpose_reported_priority']], how='left')

        # Modify attributes
        trip['depart_datetime'] = pd.to_datetime(trip['depart_date'].astype(str) + ' ' +
                                                trip['depart_hour'].astype(str) + ':' +
                                                trip['depart_minute'].astype(str) + ':' +
                                                trip['depart_seconds'].astype(str), format='%Y-%m-%d %H:%M:%S')
        trip['arrive_datetime'] = pd.to_datetime(trip['arrive_date'].astype(str) + ' ' +
                                                trip['arrive_hour'].astype(str) + ':' +
                                                trip['arrive_minute'].astype(str) + ':' +
                                                trip['arrive_second'].astype(str), format='%Y-%m-%d %H:%M:%S')

        trip['num_travelers'] = np.where(trip['num_travelers'] == 995, 1, trip['num_travelers'])
        trip['managed_lane_use'] = np.where(trip['managed_lane_use'] == 995, 2, trip['managed_lane_use'])

        # Split trips into two parts
        trip_s0 = trip[(trip['o_purpose_category'] != 11) & (trip['d_purpose_category'] != 11)]
        trip_s1 = trip[(trip['o_purpose_category'] == 11) | (trip['d_purpose_category'] == 11)]

        # 3.0 define key variables 

        C_id = ['hh_id','person_id','trip_id','person_num','trip_num','day_num']
        # Ensure travel_dow is preserved if present
        if 'travel_dow' in trip.columns:
            C_id.append('travel_dow')
        C_flag = ['link_day','link_num','leg_num','leg_delete']
        C_purp = ['o_purpose_category','d_purpose_category','o_purpose','d_purpose','o_purpose_reported_priority','d_purpose_reported_priority']
        C_time = ['depart_datetime','arrive_datetime']
        C_mode = ['mode_type_priority','mode_1_priority','mode_2_priority','mode_3_priority','mode_4_priority']
        C_access = ['transit_access_priority','transit_egress_priority']
        C_od =   ['o_lat','o_lon','d_lat','d_lon','o_county', 'd_county','o_state', 'd_state','o_puma_2022', 'd_puma_2022']
        C_wt = weight_cols + ['num_travelers','managed_lane_use','driver']
        # C_wt =   ['trip_weight','trip_weight_rmove_only','num_travelers','managed_lane_use','driver']
        C_other =  ['distance_miles','duration_minutes','dwell_mins']
        C_last = ['d_purpose_category','d_purpose','arrive_datetime','d_lat','d_lon','d_county','d_state','d_puma_2022']
        C_ppr =  ['o_purpose_reported_priority','d_purpose_reported_priority']
        C_transit = ['transit_access_lat','transit_access_lon','transit_egress_lat','transit_egress_lon']

        C_pri = C_ppr + C_mode + C_access
        C_key = C_purp + C_mode + C_access + C_time + C_od + C_wt + C_other + C_transit

        C_key_link = ["link_" + key for key in C_key]
        C_last_link = ["link_" + key for key in C_last]
        C_pri_link = ["link_" + key for key in C_pri] 
        C_wt_link = ["link_" + key for key in C_wt]
        C_other_sum = ["sum_" + key for key in C_other]

        dictKey = dict(zip(C_key,C_key_link))
        dictLast = dict(zip(C_last,C_last_link))
        dictPri = dict(zip(C_pri,C_pri_link))
        dictWt = dict(zip(C_wt,C_wt_link)) 
        dictSum = dict(zip(C_other,C_other_sum)) # Process trip_s1

        trip_s1.fillna(0, inplace=True)
        trip_s1=trip_s1.sort_values(by=['hh_id','person_id','trip_num'])
        
        # Initialize transit access/egress columns
        trip_s1['transit_access_lat'] = np.nan
        trip_s1['transit_access_lon'] = np.nan
        trip_s1['transit_egress_lat'] = np.nan
        trip_s1['transit_egress_lon'] = np.nan
        
        # Output the linked trips
        for item in C_key_link:
            trip_s1.loc[:,item] = 0.0
        for item in C_flag:
            trip_s1.loc[:,item] = 0

        i = 0
        link_pid = 0
        link_day = 0
        link_num = 0

        # loop
        while i < len(trip_s1):
            
            person_id = trip_s1.iloc[i,trip_s1.columns.get_loc('person_id')]
            day_num = trip_s1.iloc[i,trip_s1.columns.get_loc('day_num')] 
            link_day = day_num
            
            if (person_id == link_pid):
                link_num += 1
            else:
                link_num = 1
                link_pid = person_id 

            # initial for all trips including independent trips with one leg only    
            for item in dictKey:    
                trip_s1.iloc[i,trip_s1.columns.get_loc(dictKey[item])] = trip_s1.iloc[i,trip_s1.columns.get_loc(item)]  
            trip_s1.iloc[i,trip_s1.columns.get_loc('link_num')] = link_num
            trip_s1.iloc[i,trip_s1.columns.get_loc('link_day')] = link_day
            
            # Initialize transit access/egress for this trip
            transit_access_set = False
            transit_egress_lat = np.nan
            transit_egress_lon = np.nan
            
            # Check if current trip is transit for access point
            if trip_s1.iloc[i,trip_s1.columns.get_loc('mode_type')] == 13:
                trip_s1.iloc[i,trip_s1.columns.get_loc('link_transit_access_lat')] = trip_s1.iloc[i,trip_s1.columns.get_loc('o_lat')]
                trip_s1.iloc[i,trip_s1.columns.get_loc('link_transit_access_lon')] = trip_s1.iloc[i,trip_s1.columns.get_loc('o_lon')]
                transit_access_set = True
                # Also set egress initially (will be updated if more transit legs found)
                transit_egress_lat = trip_s1.iloc[i,trip_s1.columns.get_loc('d_lat')]
                transit_egress_lon = trip_s1.iloc[i,trip_s1.columns.get_loc('d_lon')]
            
            if (trip_s1.iloc[i,trip_s1.columns.get_loc('d_purpose_category')] !=11):        
                i += 1
                continue
            
            j = 1
            for item in C_other:
                exec('sum_'+item + '=' + "trip_s1.iloc[i,trip_s1.columns.get_loc('" + item + "')]")
                                
            while ((trip_s1.iloc[(i+j),trip_s1.columns.get_loc('person_id')] == person_id) & \
                (trip_s1.iloc[(i+j),trip_s1.columns.get_loc('day_num')] == day_num)):

                # use last leg's 'd_purpose_category','d_purpose'... as real purpose
                for item in dictLast:
                    trip_s1.iloc[i,trip_s1.columns.get_loc(dictLast[item])] = trip_s1.iloc[(i+j),trip_s1.columns.get_loc(item)]
                
                # Check for transit access point (first transit trip in sequence)
                if (trip_s1.iloc[(i+j),trip_s1.columns.get_loc('mode_type')] == 13) and not transit_access_set:
                    trip_s1.iloc[i,trip_s1.columns.get_loc('link_transit_access_lat')] = trip_s1.iloc[(i+j),trip_s1.columns.get_loc('o_lat')]
                    trip_s1.iloc[i,trip_s1.columns.get_loc('link_transit_access_lon')] = trip_s1.iloc[(i+j),trip_s1.columns.get_loc('o_lon')]
                    transit_access_set = True
                
                # Update transit egress point (last transit trip in sequence)
                if trip_s1.iloc[(i+j),trip_s1.columns.get_loc('mode_type')] == 13:
                    transit_egress_lat = trip_s1.iloc[(i+j),trip_s1.columns.get_loc('d_lat')]
                    transit_egress_lon = trip_s1.iloc[(i+j),trip_s1.columns.get_loc('d_lon')]
                
                # use leg of priority
                for item in dictPri:
                    if trip_s1.iloc[(i+j),trip_s1.columns.get_loc(item)] < trip_s1.iloc[i,trip_s1.columns.get_loc(dictPri[item])]:
                        trip_s1.iloc[i,trip_s1.columns.get_loc(dictPri[item])] = trip_s1.iloc[(i+j),trip_s1.columns.get_loc(item)]            
                    
                # use leg of main mode_type for trip_weight
                for item in dictWt:
                    if trip_s1['mode_type_priority'].iloc[i+j] == trip_s1['link_mode_type_priority'].iloc[i]:
                        trip_s1.iloc[i,trip_s1.columns.get_loc(dictWt[item])] = trip_s1.iloc[(i+j),trip_s1.columns.get_loc(item)]
                    
                #sum_other
                for item in C_other:
                    exec('sum_'+item + '=' + 'sum_' + item  + " + trip_s1.iloc[(i+j),trip_s1.columns.get_loc('" + item + "')]")
                    exec("trip_s1.iloc[i,trip_s1.columns.get_loc('link_" + item + "')] = sum_" + item )
            
                # add flags
                trip_s1.iloc[(i+j),trip_s1.columns.get_loc('leg_delete')] = 1
                trip_s1.iloc[(i+j),trip_s1.columns.get_loc('link_num')] = link_num
                trip_s1.iloc[(i+j),trip_s1.columns.get_loc('leg_num')] = j
                trip_s1.iloc[(i+j),trip_s1.columns.get_loc('link_day')] = link_day
                
                if trip_s1.iloc[(i+j),trip_s1.columns.get_loc('d_purpose_category')] != 11:            
                    j += 1
                    break
                j += 1   
                continue
            
            # Set final transit egress point after processing all legs
            if not pd.isna(transit_egress_lat):
                trip_s1.iloc[i,trip_s1.columns.get_loc('link_transit_egress_lat')] = transit_egress_lat
                trip_s1.iloc[i,trip_s1.columns.get_loc('link_transit_egress_lon')] = transit_egress_lon

            i = i + j
            continue

            # 3.3. post-process

        # 3.3.1 link_mode_type_priority -> link_mode_type
        trip_s1_m = trip_s1.merge(lookup_mode_type[['link_mode_type_priority','link_mode_type']],how='left') 
        trip_s1_m = trip_s1_m.merge(lookup_mode[['link_mode_1_priority','link_mode_1']],how='left') 
        trip_s1_m = trip_s1_m.merge(lookup_mode[['link_mode_2_priority','link_mode_2']],how='left')
        trip_s1_m = trip_s1_m.merge(lookup_mode[['link_mode_3_priority','link_mode_3']],how='left')
        trip_s1_m = trip_s1_m.merge(lookup_mode[['link_mode_4_priority','link_mode_4']],how='left')

        # 3.3.2 link_transit_access_priority -> link_transit_access
        trip_s1_m = trip_s1_m.merge(lookup_transit_access[['link_transit_access_priority','link_transit_access']],how='left') 
        trip_s1_m = trip_s1_m.merge(lookup_transit_egress[['link_transit_egress_priority','link_transit_egress']],how='left')

        # 3.3.3 modify link_x_purpose, link_x_purpose_category
        trip_s1_m=trip_s1_m.merge(lookup_o_purpose_reported[['link_o_purpose_reported_priority',
                                                            'link_o_purpose_category_new',
                                                            'link_o_purpose_new',
                                                            'link_o_purpose_reported']],how='left')   
        trip_s1_m=trip_s1_m.merge(lookup_d_purpose_reported[['link_d_purpose_reported_priority',
                                                            'link_d_purpose_category_new',
                                                            'link_d_purpose_new',
                                                            'link_d_purpose_reported']],how='left')

        # if link_od_purpose_category == 11, use link_od_purpose_new based on link_od_purpose_reported_priority as new real purpose
        trip_s1_m['link_o_purpose_category'] = np.where(trip_s1_m['link_o_purpose_category'] == 11,
                                            trip_s1_m['link_o_purpose_category_new'],
                                            trip_s1_m['link_o_purpose_category'])
        trip_s1_m['link_d_purpose_category'] = np.where(trip_s1_m['link_d_purpose_category'] == 11,
                                            trip_s1_m['link_d_purpose_category_new'],
                                            trip_s1_m['link_d_purpose_category'])

        trip_s1_m['link_o_purpose'] = np.where(trip_s1_m['link_o_purpose'] == 60,
                                            trip_s1_m['link_o_purpose_new'],
                                            trip_s1_m['link_o_purpose'])
        trip_s1_m['link_d_purpose'] = np.where(trip_s1_m['link_d_purpose'] == 60,
                                            trip_s1_m['link_d_purpose_new'],
                                            trip_s1_m['link_d_purpose'])
        # 4. output trip_s1

        # define output columns
        C_purp = ['o_purpose_category','o_purpose','o_purpose_reported','d_purpose_category','d_purpose','d_purpose_reported']
        C_mode = ['mode_type','mode_1','mode_2','mode_3','mode_4']
        C_access = ['transit_access','transit_egress']
        C_transit = ['transit_access_lat','transit_access_lon','transit_egress_lat','transit_egress_lon']
        C_filter_unlink = C_time + C_purp + C_mode + C_access + C_wt + C_od + C_other + C_transit
        C_filter_link = ["link_" + key for key in C_filter_unlink]
        C_filter_link_unlink = dict(zip(C_filter_link,C_filter_unlink))
        C_filter = C_id + C_filter_link + C_flag

        # linked trip_s1 with all legs and flags
        trip_s1 = trip_s1_m.filter(items=C_filter)

        # create distinct trip_s1_linked, by removing ['leg_delete'] == 1
        trip_s1_linked = trip_s1[trip_s1['leg_delete'] == 0]
        trip_s1_linked.loc[:,'link_flag'] = "Y"

        # rename attriubtes by removing "link_"
        trip_s1_linked_rename = trip_s1_linked.copy()
        trip_s1_linked_rename = trip_s1_linked_rename.rename(columns=C_filter_link_unlink)

        # 5.0 process trip_s0 by filter columns
        trip_s0_filter = trip_s0.filter(items=trip_s1_linked_rename.columns.tolist())
        trip_s0_filter.loc[:,'link_flag'] = "N"
        trip_s0_filter.loc[:,'link_num'] = 0
        trip_s0_filter.loc[:,'leg_num'] = 999
        trip_s0_filter.loc[:,'leg_delete'] = "NA"
        
        # Initialize transit access/egress columns for non-linked trips
        trip_s0_filter.loc[:,'transit_access_lat'] = np.nan
        trip_s0_filter.loc[:,'transit_access_lon'] = np.nan
        trip_s0_filter.loc[:,'transit_egress_lat'] = np.nan
        trip_s0_filter.loc[:,'transit_egress_lon'] = np.nan
        
        # For single transit trips, set access/egress points
        transit_mask = trip_s0_filter['mode_type'] == 13
        trip_s0_filter.loc[transit_mask, 'transit_access_lat'] = trip_s0_filter.loc[transit_mask, 'o_lat']
        trip_s0_filter.loc[transit_mask, 'transit_access_lon'] = trip_s0_filter.loc[transit_mask, 'o_lon']
        trip_s0_filter.loc[transit_mask, 'transit_egress_lat'] = trip_s0_filter.loc[transit_mask, 'd_lat']
        trip_s0_filter.loc[transit_mask, 'transit_egress_lon'] = trip_s0_filter.loc[transit_mask, 'd_lon']

        # 6.0 append trip_s1 linked trips with trip_s0 filter
        trip_s01 = pd.concat([trip_s1_linked_rename,trip_s0_filter])
        self.trips = trip_s01
        return  # Return the processed trips for further use or testing

    def summarize_trip_modes(self, weight_cols=['trip_weight','trip_weight_rmove_only']):
        """ logic to summarize linked trips by mode for model calibration/estimation. Originally created by Yun Ma. https://github.com/vta/BATS-2023/blob/main/trip_convert/script/convert_n_sum_bats_trips_for_model_mode_choice_calibration_yma_0608.ipynb"""
        trip = self.trips
        pp = self.person
        hh = self.hh

        lookup_age = pd.read_csv(self.config['lookup_age'])
        lookup_income = pd.read_csv(self.config['lookup_income'])

        lookup_d_purpose_adj = pd.read_csv(self.config['lookup_d_purpose_adj'])
        lookup_d_purpose_adj_combined = pd.read_csv(self.config['lookup_d_purpose_adj_combined'])

        lookup_mode_type = pd.read_csv(self.config['lookup_mode_type_2'])
        lookup_mode = pd.read_csv(self.config['lookup_mode_2'])

        lookup_access = pd.read_csv(self.config['lookup_access'])
        lookup_access_submode = pd.read_csv(self.config['lookup_access_submode'])
        lookup_other_3 = pd.read_csv(self.config['lookup_other_3']) 
        
        ### 1. filter/merge

        C_id = ['hh_id','person_id','trip_id']
        C_pp = ['d_purpose_category','d_purpose','d_purpose_reported','o_purpose_category','o_purpose','o_purpose_reported']
        C_md = ['mode_type', 'mode_1', 'mode_2', 'mode_3', 'mode_4']
        C_ac = ['transit_access']
        C_num= ['num_travelers','managed_lane_use']
        C_wt = weight_cols #['trip_weight', 'trip_weight_rmove_only']
        C_sel = C_id + C_pp + C_md + C_ac + C_num + C_wt
        C_int = C_md + C_ac + C_num 
        # trip = trip.filter(items=C_sel)
        trip[C_int] = trip[C_int].astype(int)

        trip = trip.merge(hh[['hh_id','income_broad']],how = 'left')
        trip = trip.merge(lookup_income[['income_broad','income_vta','income_vta_des']],how = 'left')
        trip = trip.merge(pp[['person_id','age']],how = 'left')

        trip['num_traveler'] = np.where(trip['num_travelers']>=3,3,trip['num_travelers'])
        trip['managed_lane_use'] = np.where(trip['managed_lane_use']==995,2,trip['managed_lane_use'])
        trip['managed_lane_use'] = np.where(trip['driver']==2,2,trip['managed_lane_use'])       # only drivers are asked MLU question
        
        trip['mode_type_sub'] = np.where(trip['mode_type']==8,(trip['mode_type'].astype('str') + '_' + \
                                                            trip['num_traveler'].astype('str') + '_' + \
                                                            trip['managed_lane_use'].astype('str')),\
                                                            trip['mode_type'].astype('str')+ '_')
        
        ### 2. process mode/access 
        # mode
        trip = trip.merge(lookup_mode[['mode_1','mode_1_priority']],how = 'left')
        trip = trip.merge(lookup_mode[['mode_2','mode_2_priority']],how = 'left')
        trip = trip.merge(lookup_mode[['mode_3','mode_3_priority']],how = 'left')
        trip = trip.merge(lookup_mode[['mode_4','mode_4_priority']],how = 'left')
        trip['mode_priority'] = trip[['mode_1_priority', 'mode_2_priority', 'mode_3_priority','mode_4_priority']].min(axis=1)

        trip = trip.merge(lookup_mode_type[['mode_type_sub','mode_type_sub_des','mode_type_model','mode_type_model_des']],how = 'left')
        trip = trip.merge(lookup_mode[['mode_priority','mode_type_sub_new','mode_type_sub_new_des','mode_type_model_new','mode_type_model_new_des']],how = 'left')

        trip['mode_type_sub'] =  np.where(trip['mode_type'].isin({12,13}),trip['mode_type_sub_new'],trip['mode_type_sub'])
        trip['mode_type_sub_des']=np.where(trip['mode_type'].isin({12,13}),trip['mode_type_sub_new_des'],trip['mode_type_sub_des'])

        trip['mode_type_model']=np.where(trip['mode_type'].isin({12,13}),trip['mode_type_model_new'],trip['mode_type_model'])
        trip['mode_type_model_des']=np.where(trip['mode_type'].isin({12,13}),trip['mode_type_model_new_des'],trip['mode_type_model_des'])

        # access and egress
        trip = trip.merge(lookup_access[['transit_access','transit_access_model','transit_access_model_des']],how = 'left')
        trip = trip.merge(lookup_access.rename(columns = {col: col.replace('access','egress') for col in lookup_access.columns})[['transit_egress','transit_egress_model','transit_egress_model_des']],how = 'left')
        trip['access_submode'] = trip['transit_access_model'].astype('str') + "_" + trip['mode_type_model'].astype('str')
        # trip['egress_submode'] = trip['transit_egress_model'].astype('str') + "_" + trip['mode_type_model'].astype('str')
        trip = trip.merge(lookup_access_submode[['access_submode','mode_type_model_3','mode_type_model_3_des']],how = 'left')
        # trip = trip.merge(lookup_access_submode.rename(columns = {col.replace('access','egress'): col for col in lookup_access_submode.columns})[['egress_submode','mode_type_model_3','mode_type_model_3_des']],how = 'left', suffixes = ['_access', '_egress'])
        trip['mode_type_model']=np.where(trip['mode_type'].isin({12,13}),trip['mode_type_model_3'],trip['mode_type_model'])
        trip['mode_type_model_des']=np.where(trip['mode_type'].isin({12,13}),trip['mode_type_model_3_des'],trip['mode_type_model_des'])

        ### 3. process purpose
        trip['d_purpose_adj'] = trip['d_purpose']
        trip['homeBased'] = 0

        # modify d_purpose of escort
        trip['flag_o_school'] = np.where(trip['o_purpose_category'].isin({4,5}),1,0)
        trip['flag_d_escort'] = np.where(trip['d_purpose_category'].isin({6}),1,0)
        trip['d_purpose_adj'] = np.where(((trip['flag_o_school'] == 1)&(trip['flag_d_escort']==1)),\
                                        trip['o_purpose'],trip['d_purpose_adj'])
        # modify d_purpose of home
        trip['flag_o_home'] =  np.where(trip['o_purpose'].isin({1}),1,0)
        trip['flag_d_home'] =  np.where(trip['d_purpose'].isin({1}),1,0)
        trip['homeBased'] = np.where(((trip['flag_o_home'] == 1) | (trip['flag_d_home']==1)),1,0)
        trip['d_purpose_adj'] = np.where(((trip['flag_d_home'] == 1) & (trip['flag_o_home']!=1)),\
                                        trip['o_purpose'],trip['d_purpose_adj'])
        trip['d_purpose_adj'] = np.where(((trip['homeBased'] == 1) & \
                                        (trip['d_purpose_adj'].isin({1})) & \
                                        (~trip['d_purpose_reported'].isin({-1,1,995}))),\
                                        trip['d_purpose_reported'],trip['d_purpose_adj'])
        trip['d_purpose_adj'] = np.where(((trip['homeBased'] == 1) & \
                                        (trip['d_purpose_adj'].isin({1})) & \
                                        (~trip['o_purpose_reported'].isnull()) & \
                                        (~trip['o_purpose_reported'].isin({-1,1,995}))),\
                                            trip['o_purpose_reported'],trip['d_purpose_adj'])

        # convert to model purpose
        trip = trip.merge(lookup_d_purpose_adj[['d_purpose_adj','d_purpose_adj_des',\
                                                'd_purpose_adj_category','d_purpose_adj_category_des',\
                                                'd_purpose_adj_model','d_purpose_adj_model_des',\
                                                'd_purpose_adj_submodel','d_purpose_adj_submodel_des']],how='left')
        # split k-12
        trip = trip.merge(lookup_age[['age','d_purpose_adj_model_new','d_purpose_adj_model_new_des',\
                                            'd_purpose_adj_submodel_new','d_purpose_adj_submodel_new_des',]],how='left')
        trip['d_purpose_adj_model']=np.where((trip['d_purpose_adj_model'].isin({7})),\
                                            trip['d_purpose_adj_model_new'],trip['d_purpose_adj_model'])
        trip['d_purpose_adj_submodel']=np.where((trip['d_purpose_adj_submodel'].isin({'7_1','7_2','7_3','7_4'})),\
                                                trip['d_purpose_adj_submodel_new'],trip['d_purpose_adj_submodel'])
        trip['d_purpose_adj_model_des']=np.where((trip['d_purpose_adj_model_des'].isin({'K-12 school'})),\
                                                trip['d_purpose_adj_model_new_des'],trip['d_purpose_adj_model_des'])
        trip['d_purpose_adj_submodel_des']=np.where((trip['d_purpose_adj_submodel_des'].isin({'K-12 school_attend K-12 school',\
                                                                                            'K-12 school_attend school/class',\
                                                                                            'K-12 school_Attend vocational education class',\
                                                                                            'K-12 school_Attend other education-related activity'})),\
                                                                                            trip['d_purpose_adj_submodel_new_des'],\
                                                                                            trip['d_purpose_adj_submodel_des'])
        # split income
        trip['d_purpose_adj_model_home_inc'] = trip['homeBased'].astype('str') + "_" + \
                                            trip['income_vta'].astype('str') + "_" + \
                                            trip['d_purpose_adj_model'].astype('str') + "_" +\
                                            trip['d_purpose_adj_model_des']
        trip=trip.merge(lookup_d_purpose_adj_combined[['d_purpose_adj_model_home_inc','d_purpose_adj_vta','d_purpose_adj_vta_des']],how='left')
        self.trips = trip.drop(['age','income_broad'],axis=1)
        return

    
    def append_skim_values(self):
        """Directly use model outputs from passed in model directory in yaml file. Use Cube voyager to convert skims
        to omx format if not already done. Use parquet to compress omx files (reduces read time by 8mins). Use skim
          map to attach correct skim by mode"""
        if len(self.trips[self.config['choice_col']].unique()) == 1:
            raise ValueError("Only one choice in the data. Please check your data.")
        
        #add skim data and other data transformations
        merged_skims = utilities.import_all_skims(self.config)
        self.trips = self.trips.merge(merged_skims, on=['OTAZ', 'DTAZ'], how='left')
        if len(self.trips[self.config['choice_col']].unique()) == 1:
            raise ValueError("2. Only one choice in the data. Please check your data.")
        skim_map = pd.read_csv(self.config['skim_map'])

        
        # If depart_datetime is not already datetime type:
        self.trips['depart_datetime'] = pd.to_datetime(self.trips['depart_datetime'])
        #self.trips = self.trips[~(self.trips.OTAZ.isna())]
        #self.trips = self.trips[~(self.trips.DTAZ.isna())]
        # Extract the hour
        self.trips['depart_hour'] = self.trips['depart_datetime'].dt.hour
        self.trips['period'] = 'offpeak'
        self.trips.loc[self.trips['depart_hour'].isin(self.config['peak_hours']), 'period'] = 'peak'

        self.trips = self.trips.merge(skim_map, on = ['mode_type_model_des','period'], how = 'left')
        for attr in ['dist','time', 'cost', 'wait', 'xfer', 'walk']:
            col_name = f'skim_{attr}'
            conversion_col = f'conversion_{attr}'
            if conversion_col in self.trips.columns:
                self.trips[col_name] = self.trips.apply(
                    lambda row: eval(f"row[row[attr]] {row[conversion_col]}") if pd.notnull(row[attr]) and row[attr] in self.trips.columns and pd.notnull(row[conversion_col]) and str(row[conversion_col]).strip() != ''
                    else row[row[attr]] if pd.notnull(row[attr]) and row[attr] in self.trips.columns else np.nan,
                    axis=1
                )
            else:
                self.trips[col_name] = self.trips.apply(
                    lambda row: row[row[attr]] if pd.notnull(row[attr]) and row[attr] in self.trips.columns else np.nan,
                    axis=1
                )

        return


    # Additional summarization methods can be added here as needed
    def save_processed_data(self):
        """Save the processed trips and person data to CSV files."""
        if not os.path.exists(_join(self.config['survey_dir'], 'processed')):
            os.makedirs(_join(self.config['survey_dir'], 'processed'))
        self.trips.to_csv(_join(self.config['survey_dir'], 'processed', 'linked_trips.csv'), index=False)
        self.person.to_csv(_join(self.config['survey_dir'], 'processed', 'person.csv'), index=False)
        self.hh.to_csv(_join(self.config['survey_dir'], 'processed', 'households.csv'), index=False)
        if self.day is not None:
            self.day.to_csv(_join(self.config['survey_dir'], 'processed', 'day.csv'), index=False)
        if self.tours is not None:
            self.tours.to_csv(_join(self.config['survey_dir'], 'processed', 'tours.csv'), index=False)
    
    def read_processed_data(self):
        """Read already processed survey data from the specified directory."""
        print("Reading processed survey data..."   )
        self.trips = pd.read_csv(_join(self.config['survey_dir'], 'processed', 'linked_trips.csv'))
        self.person = pd.read_csv(_join(self.config['survey_dir'], 'processed', 'person.csv'))
        self.hh = pd.read_csv(_join(self.config['survey_dir'], 'processed', 'households.csv'))
        if os.path.exists(_join(self.config['survey_dir'], 'processed', 'tours.csv')):
            self.tours = pd.read_csv(_join(self.config['survey_dir'], 'processed', 'tours.csv'))
        if os.path.exists(_join(self.config['survey_dir'], 'processed', 'day.csv')):
            self.day = pd.read_csv(_join(self.config['survey_dir'], 'processed', 'day.csv'))

    