
# ************************************************************************
# This script prepares the data for BATA bridge usage analysis
# 
# The goal is to show the proportion of low-income households that cross BATA bridges 0 times, 1 time, 2 times, etc., on a typical weekday, 
# compared to the proportion of non-low-income households that cross BATA bridges 0 times, 1 time, 2 times, etc.
#
# Asana task: https://app.asana.com/0/12291104512646/1208217111737177/f
#
# ************************************************************************

import os
import pandas as pd
import numpy as np

import logging
from datetime import datetime

# ************************************************************************
# Specify file paths
# ************************************************************************

BATS_DIR = r"M:\Data\HomeInterview\Bay Area Travel Study 2023\Data\Full Weighted 2023 Dataset\WeightedDataset_09112024"
HH_CSV = os.path.join(BATS_DIR, "hh.csv")
TRIP_CSV = os.path.join(BATS_DIR, "trip.csv")

USERPROFILE = os.getenv("USERPROFILE")
CONFLATION_DIR = os.path.join(USERPROFILE, "Box", "Modeling and Surveys", "Surveys", "Travel Diary Survey", "Biennial Travel Diary Survey", "Data", "2023", "Survey Conflation")
FACILITY_BOOL_CSV = os.path.join(CONFLATION_DIR, "BATS 2023 Facility Use Booleans Toll.csv")

HH_POVERTY_CSV = os.path.join("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data","Full Weighted 2023 Dataset","WeightedDataset_09112024", "derived_variables","BATShh_ImputedIncomeValues.csv")

#OUTPUT_DIR = os.path.join("E:/temp", "BridgeTollAnalysis_Edrive")
OUTPUT_DIR = os.path.join("M:/Data", "HomeInterview", "Bay Area Travel Study 2023", "Data", "Full Weighted 2023 Dataset", "WeightedDataset_09112024", "Requests", "BATA_bridge_usage")

# ************************************************************************
# Set up logging
# ************************************************************************

# Generate a timestamp for the log file name
timestamp = datetime.now().strftime('%Y-%m-%d_%H-%M-%S')

# Define the log file path
log_file = os.path.join(OUTPUT_DIR, f"BataBridgeUsage_{timestamp}.log")

# Configure logging to write to both console and the log file with timestamps
logging.basicConfig(
    level=logging.INFO,  # Set the logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL)
    format='%(asctime)s - %(levelname)s - %(message)s',  # Include timestamps in the log format
    datefmt='%Y-%m-%d %H:%M:%S',  # Set the timestamp format
    handlers=[
        logging.FileHandler(log_file), # Directs log messages to a specified log file
        logging.StreamHandler()        # Directs log messages to the console (standard output)
    ]
)

logging.info("BATS version used:")
logging.info(f"BATS_DIR               : {BATS_DIR}")
logging.info("")  # This prints a blank line
logging.info("")  # This prints a blank line
logging.info("")  # This prints a blank line

# ************************************************************************
# Data prep
# ************************************************************************

# Read the CSV files into pandas DataFrames
hh_df = pd.read_csv(HH_CSV)
trip_df = pd.read_csv(TRIP_CSV)
facility_bool_df = pd.read_csv(FACILITY_BOOL_CSV)

hh_poverty_df=pd.read_csv(HH_POVERTY_CSV)
# Don't need all the columns
#hh_poverty_filtered_df = hh_poverty_df[['hhInc_continuous', 'poverty_status', 'hh_id']]

# Print out the number of rows in the files read
logging.info("")  # This prints a blank line
logging.info("----------------------------------------------")
logging.info("Print out the number of rows in the files read")
logging.info("----------------------------------------------")
logging.info(f"Number of rows in hh_df           : {len(hh_df)}")
logging.info(f"Number of rows in hh_poverty_df   : {len(hh_poverty_df)}")
logging.info(f"Number of rows in trip_df         : {len(trip_df)}")
logging.info(f"Number of rows in facility_bool_df: {len(facility_bool_df)}")
logging.info("")  # This prints a blank line

# Join the trips with the facility booleans
tripFacility_df = pd.merge(trip_df, facility_bool_df, on='trip_id', how='outer', indicator=True)

# Have a look at the merge indicator
merge_indicator_counts = tripFacility_df['_merge'].value_counts()
logging.info("")  # This prints a blank line
logging.info("----------------------------------------------")
logging.info("After joining the trips with the facility booleans,")
logging.info("check the merge indicator in tripFacility_df")
logging.info("----------------------------------------------")
logging.info(merge_indicator_counts)
logging.info("")  # This prints a blank line
logging.info("Note 1: We would expect that the numbers in 'both' to be the same as the number of rows in facility_bool_df.")
logging.info("Note 2: We would expect that the numbers in 'right_only' to be zero.")
logging.info("")  # This prints a blank line

# Done with the merge indicator. Drop it.
tripFacility_df = tripFacility_df.drop(columns=['_merge'])

# Join the trips + facility booleans to households
hhTripFacility_df = pd.merge(hh_df, tripFacility_df, on='hh_id', how='outer', indicator=True)

# Have a look at the merge indicator
merge_indicator_counts = hhTripFacility_df['_merge'].value_counts()
logging.info("----------------------------------------------")
logging.info("Check the merge indicator in hhTripFacility_df")
logging.info("----------------------------------------------")
logging.info(merge_indicator_counts)
logging.info("")  # This prints a blank line
logging.info("Note 1: We would expect that the number in 'both' to be the same as the number of rows in trip_df i.e. every trip is associated with a household")
logging.info("Note 2: If 'left_only' is non-zero, it indicates that this number of households made zero trips during the survey period.")
logging.info("")  # This prints a blank line

# Done with the merge indicator. Drop it.
hhTripFacility_df = hhTripFacility_df.drop(columns=['_merge'])

# Write the file to csv for checking
# output_test1 = os.path.join(OUTPUT_DIR, 'hhTripFacility.csv')
# hhTripFacility_df.to_csv(output_test1, index=False)

# ************************************************************************
# Add poverty status
# ************************************************************************

# join the trips + facility booleans + households + poverty status
TripFacilityPoverty_df = pd.merge(hhTripFacility_df, hh_poverty_df, on='hh_id', how='outer', indicator=True)

# Have a look at the merge indicator
merge_indicator_counts = TripFacilityPoverty_df['_merge'].value_counts()
logging.info("----------------------------------------------")
logging.info("Check the merge indicator in TripFacilityPoverty_df")
logging.info("----------------------------------------------")
logging.info(merge_indicator_counts)
logging.info("")  # This prints a blank line
logging.info("Note 1: 'both' in this merge should be the sum of 'both' and 'left_only' in the last table")
logging.info("")  # This prints a blank line

# Done with the merge indicator. Drop it.
TripFacilityPoverty_df = TripFacilityPoverty_df.drop(columns=['_merge'])

# Write the file to csv for checking
# output_test2 = os.path.join(OUTPUT_DIR, 'TripFacilityPoverty.csv')
# TripFacilityPoverty_df.to_csv(output_test2, index=False)

# ************************************************************************************************************
# Note that the data represents person-trip
# But for this analysis we are interested in vehicle-trips
# Address this issue by assiging one of the person-trip to be "responsible for paying"
# ************************************************************************************************************

# start from some tabulations
driver_tabulation = TripFacilityPoverty_df['driver'].value_counts()
logging.info("----------------------------------------------")
logging.info("Tabulate the 'driver' variable")
logging.info("Note that the value_labels for 'driver' are:")
logging.info("driver	1	Driver")
logging.info("driver	2	Passenger")
logging.info("driver	3	Both (switched drivers during trip)")
logging.info("driver	995	Missing Response")
logging.info("----------------------------------------------")
logging.info(driver_tabulation)
logging.info("")  # This prints a blank line


CopiedFromProxy_tabulation = TripFacilityPoverty_df['copied_from_proxy'].value_counts()
logging.info("----------------------------------------------")
logging.info("Tabulate the 'copied_from_proxy' variable")
logging.info("Note that the value_labels for 'copied_from_proxy' are:")
logging.info("copied_from_proxy	0	No")
logging.info("copied_from_proxy	1	Yes")
logging.info("# 995	Missing Response")
logging.info("----------------------------------------------")
logging.info(CopiedFromProxy_tabulation)
logging.info("")  # This prints a blank line

# Create a variable 'ResponsibleForPaying' and initialize it to 1
TripFacilityPoverty_df['ResponsibleForPaying'] = 1

ResponsibleForPaying_tabulation = TripFacilityPoverty_df['ResponsibleForPaying'].value_counts()
logging.info("----------------------------------------------")
logging.info("Initially, assume everyone pay")
logging.info("----------------------------------------------")
logging.info(ResponsibleForPaying_tabulation)
logging.info("")  # This prints a blank line

# Set ResponsibleForPaying = 1 where driver != 2
TripFacilityPoverty_df['ResponsibleForPaying'] = np.where(TripFacilityPoverty_df['driver'] != 2, 1, 0)

ResponsibleForPaying_tabulation = TripFacilityPoverty_df['ResponsibleForPaying'].value_counts()
logging.info("----------------------------------------------")
logging.info("After applying the assumption that passengers don't pay")
logging.info("----------------------------------------------")
logging.info(ResponsibleForPaying_tabulation)
logging.info("")  # This prints a blank line

# Set ResponsibleForPaying = 0 where copied_from_proxy = 1
TripFacilityPoverty_df['ResponsibleForPaying'] = np.where(TripFacilityPoverty_df['copied_from_proxy'] == 1, 0, TripFacilityPoverty_df['ResponsibleForPaying'])

ResponsibleForPaying_tabulation = TripFacilityPoverty_df['ResponsibleForPaying'].value_counts()
logging.info("----------------------------------------------")
logging.info("After applying the assumption that trips copied from proxy don't pay")
logging.info("----------------------------------------------")
logging.info(ResponsibleForPaying_tabulation)
logging.info("")  # This prints a blank line

# Now create a "VehTrip_index_1" for deduplication (to deal with the ambigious cases in 'driver' e.g. when it's 3 or 995)
# first create a string listing all the household members who are part of the trip
TripFacilityPoverty_df['hh_member_string'] = (
    'hh_member_' + 
    TripFacilityPoverty_df[['hh_member_1', 'hh_member_2', 'hh_member_3', 'hh_member_4',  
                             'hh_member_5', 'hh_member_6', 'hh_member_7', 'hh_member_8']]
    .astype(str)  # Convert to strings
    .agg(''.join, axis=1)
)

TripFacilityPoverty_df['VehTrip_index1'] = (
    TripFacilityPoverty_df['hh_id'].astype(str) + '_' +
    TripFacilityPoverty_df['depart_date'].astype(str) + '_' +
    TripFacilityPoverty_df['depart_hour'].astype(str) + '_' +
    TripFacilityPoverty_df['depart_minute'].astype(str) + '_' +
    TripFacilityPoverty_df['mode_1'].astype(str) + '_' +
    TripFacilityPoverty_df['num_hh_travelers'].astype(str) + '_' +
    TripFacilityPoverty_df['hh_member_string'].astype(str)
)
# todo: somehow can't convert hh_member_1, hh_member_2, etc to integers (ignore this for now)

# Modify ResponsibleForPaying based on 'VehTrip_index1'
TripFacilityPoverty_df['ResponsibleForPaying'] = TripFacilityPoverty_df.groupby('VehTrip_index1')['person_num'].transform(
    lambda x: np.where(x == x.min(), TripFacilityPoverty_df.loc[x.index, 'ResponsibleForPaying'], 0)
)

ResponsibleForPaying_tabulation = TripFacilityPoverty_df['ResponsibleForPaying'].value_counts()
logging.info("----------------------------------------------")
logging.info("After applying the 'VehTrip_index1' processing (the index includes depart_minute)")
logging.info("----------------------------------------------")
logging.info(ResponsibleForPaying_tabulation)
logging.info("")  # This prints a blank line

# Write the file to csv for checking the VehTripIndex
#output_intermediate2 = os.path.join(OUTPUT_DIR, 'TripFacilityPoverty_wVehTripIndex.csv')
#TripFacilityPoverty_df.to_csv(output_intermediate2, index=False)

# not sure if it's reasonable to assume that the trip records between hh members will be matched to the minute 
# create a "VehTrip_index2
TripFacilityPoverty_df['VehTrip_index2'] = (
    TripFacilityPoverty_df['hh_id'].astype(str) + '_' +
    TripFacilityPoverty_df['depart_date'].astype(str) + '_' +
    TripFacilityPoverty_df['depart_hour'].astype(str) + '_' +
    TripFacilityPoverty_df['mode_1'].astype(str) + '_' +
    TripFacilityPoverty_df['num_hh_travelers'].astype(str) + '_' +
    TripFacilityPoverty_df['hh_member_string'].astype(str)
)

# Modify ResponsibleForPaying based on 'VehTrip_index'
TripFacilityPoverty_df['ResponsibleForPaying'] = TripFacilityPoverty_df.groupby('VehTrip_index2')['person_num'].transform(
    lambda x: np.where(x == x.min(), TripFacilityPoverty_df.loc[x.index, 'ResponsibleForPaying'], 0)
)

ResponsibleForPaying_tabulation = TripFacilityPoverty_df['ResponsibleForPaying'].value_counts()
logging.info("----------------------------------------------")
logging.info("After applying the 'VehTrip_index2' processing (the index does not include depart_minute)")
logging.info("----------------------------------------------")
logging.info(ResponsibleForPaying_tabulation)
logging.info("")  # This prints a blank line

# ************************************************************************
# Summarize
# ************************************************************************

# Summarize bridge usage
# Note that BATS 2023 Facility Use Booleans.csv have directions
# Bay Bridge (San Francisco-Oakland): Tolls are collected only in the westbound direction (towards San Francisco).
# Richmond-San Rafael Bridge: Tolls are collected only in the westbound direction (towards San Rafael).
# San Mateo-Hayward Bridge: Tolls are collected only in the westbound direction (towards San Mateo).
# Dumbarton Bridge: Tolls are collected only in the westbound direction (towards the Peninsula).
# Carquinez Bridge: Tolls are collected only in the southbound direction (towards Vallejo).
# Benicia-Martinez Bridge: Tolls are collected only in the southbound direction (towards Benicia).

TripFacilityPoverty_df['count_BATAtoll'] = (
    TripFacilityPoverty_df['bay_bridge_toll'] + 
    TripFacilityPoverty_df['sm_bridge_toll'] + 
    TripFacilityPoverty_df['dum_bridge_toll'] + 
    TripFacilityPoverty_df['rsr_bridge_toll'] + 
    TripFacilityPoverty_df['carq_bridge_toll'] + 
    TripFacilityPoverty_df['bm_bridge_toll']
)

TripFacilityPoverty_df['num_BATAtoll'] = TripFacilityPoverty_df['count_BATAtoll'] * TripFacilityPoverty_df['ResponsibleForPaying']


# Group by 'hh_id' and aggregate
TripFacilityPoverty_groupedbyhh_df  = TripFacilityPoverty_df.groupby('hh_id').agg(
    num_BATAtoll=('num_BATAtoll', 'sum'),
    poverty_status=('poverty_status', 'first'),
    hh_weight_rmove_only=('hh_weight_rmove_only', 'first')
).reset_index()

num_unique_hh_id = TripFacilityPoverty_groupedbyhh_df['hh_id'].nunique()
logging.info(f"Number of unique hh_id after converting the trip table to a household table: {num_unique_hh_id}")
logging.info("")  # This prints a blank line

# ----
# Check outliers
# ----
# Filter rows where num_BATAtoll is greater than 10
outliers_df = TripFacilityPoverty_groupedbyhh_df[TripFacilityPoverty_groupedbyhh_df['num_BATAtoll'] > 10]

logging.info("----------------------------------------------")
logging.info("Check outliers (where num_BATAtoll is greater than 10)")
logging.info("----------------------------------------------")
# Print hh_id along with num_BATAtoll
for index, row in outliers_df.iterrows():
    logging.info(f"hh_id: {row['hh_id']}, num_BATAtoll: {row['num_BATAtoll']}")
logging.info("")  # This prints a blank line

# Group by poverty_status and get the frequency distribution of num_BATAtoll UNWEIGHTED

# ----
# make sure the frequency distribution has all combinations
# -----
# Sample unique values of poverty_status
poverty_status_unique = TripFacilityPoverty_groupedbyhh_df['poverty_status'].unique()

# Get the maximum value of num_BATAtoll
#max_num_BATAtoll = TripFacilityPoverty_groupedbyhh_df['num_BATAtoll'].max()
max_num_BATAtoll = int(TripFacilityPoverty_groupedbyhh_df['num_BATAtoll'].max())

# Create a range of values from 0 to max_num_BATAtoll
num_BATAtoll_range = range(0, max_num_BATAtoll + 1)

# Create all combinations of poverty_status and num_BATAtoll using MultiIndex
all_combinations = pd.MultiIndex.from_product(
    [poverty_status_unique, num_BATAtoll_range], 
    names=['poverty_status', 'num_BATAtoll']
)

#---

num_BATAtoll_distribution_by_poverty_UNweighted_df = (
    TripFacilityPoverty_groupedbyhh_df.groupby('poverty_status')['num_BATAtoll']
    .value_counts()
    .sort_index()
    .reset_index(name='frequency')
    .set_index(['poverty_status', 'num_BATAtoll'])  # Set multi-index for reindexing
    .reindex(all_combinations, fill_value=0)  # Reindex with all combinations, fill missing with 0
    .reset_index()  # Reset index for a clean DataFrame
)

logging.info("----------------------------------------------")
logging.info("Print out unweighted frequency distribution")
logging.info("----------------------------------------------")
logging.info(num_BATAtoll_distribution_by_poverty_UNweighted_df)
logging.info("")  # This prints a blank line



# Create a weighted frequency distribution
#num_BATAtoll_distribution_by_poverty_weighted_df = (
#    TripFacilityPoverty_groupedbyhh_df .groupby(['poverty_status', 'num_BATAtoll'])['hh_weight_rmove_only']
#    .sum()
#    .reset_index(name='weighted_frequency')
#)

num_BATAtoll_distribution_by_poverty_weighted_df = (
    TripFacilityPoverty_groupedbyhh_df.groupby(['poverty_status', 'num_BATAtoll'])['hh_weight_rmove_only']
    .sum()
    .reset_index(name='weighted_frequency')
    .set_index(['poverty_status', 'num_BATAtoll'])  # Set multi-index for reindexing
    .reindex(all_combinations, fill_value=0)  # Reindex with all combinations, fill missing with 0
    .reset_index()  # Reset index for a clean DataFrame
)

logging.info("----------------------------------------------")
logging.info("Print out weighted frequency distribution")
logging.info("----------------------------------------------")
logging.info(num_BATAtoll_distribution_by_poverty_weighted_df)
logging.info("")  # This prints a blank line

# Write the unweighted distribution to csv (for visualization in Excel or Tableau)
#output1 = os.path.join(OUTPUT_DIR, 'num_BATAtoll_distribution_by_poverty_UNweighted.csv')
#num_BATAtoll_distribution_by_poverty_UNweighted_df.to_csv(output1, index=False)

# Write the weighted distribution to csv (for visualization in Excel or Tableau)
#output2 = os.path.join(OUTPUT_DIR, 'num_BATAtoll_distribution_by_poverty_weighted.csv')
#num_BATAtoll_distribution_by_poverty_weighted_df.to_csv(output2, index=False)

# Join the unweighted and weighted DataFrames on 'poverty_status' and 'num_BATAtoll'
num_BATAtoll_distribution_byPoverty_df = pd.merge(
    num_BATAtoll_distribution_by_poverty_UNweighted_df,
    num_BATAtoll_distribution_by_poverty_weighted_df,
    on=['poverty_status', 'num_BATAtoll'],
    how='outer'  
)

# Make sure 'weighted_frequency' is numeric
num_BATAtoll_distribution_byPoverty_df['weighted_frequency'] = pd.to_numeric(num_BATAtoll_distribution_byPoverty_df['weighted_frequency'], errors='coerce')


# Print the unweighted and weighted distribution
logging.info("----------------------------------------------")
logging.info("Print out combined unweighted and weighted frequency distribution")
logging.info("----------------------------------------------")
logging.info(num_BATAtoll_distribution_byPoverty_df)
logging.info("")  # This prints a blank line


# Write the unweighted distribution to csv (for visualization in Excel or Tableau)
OUTPUT_FILE = os.path.join(OUTPUT_DIR, 'num_BATAtoll_distribution_byPoverty.csv')
num_BATAtoll_distribution_byPoverty_df.to_csv(OUTPUT_FILE, index=False)


# next steps: do MoE calcs

# I think the MoE calc was the reason why I think this is a scripting exercise 