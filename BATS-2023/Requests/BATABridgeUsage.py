import os
import pandas as pd

# ************************************************************************
# Specify file paths
# ************************************************************************

BATS_DIR = r"M:\Data\HomeInterview\Bay Area Travel Study 2023\Data\Full Weighted 2023 Dataset\WeightedDataset_09112024"
HH_CSV = os.path.join(BATS_DIR, "hh.csv")
TRIP_CSV = os.path.join(BATS_DIR, "trip.csv")

USERPROFILE = os.getenv("USERPROFILE")
CONFLATION_DIR = os.path.join(USERPROFILE, "Box", "Modeling and Surveys", "Surveys", "Travel Diary Survey", "Biennial Travel Diary Survey", "Data", "2023", "Survey Conflation")
FACILITY_BOOL_CSV = os.path.join(CONFLATION_DIR, "BATS 2023 Facility Use Booleans Toll.csv")

#HH_POVERTY_CSV = os.path.join(USERPROFILE, "Documents", "temp","BridgeTollAnalysis","BATShh_incomeImputed.csv")
HH_POVERTY_CSV = os.path.join("M:/Data/HomeInterview/Bay Area Travel Study 2023/Data","Full Weighted 2023 Dataset","WeightedDataset_09112024", "Processed","BATShh_incomeImputed.csv")


OUTPUT_DIR = os.path.join(USERPROFILE, "Documents", "temp","BridgeTollAnalysis")

# ************************************************************************
# Data prep
# ************************************************************************

# Read the CSV files into pandas DataFrames
hh_df = pd.read_csv(HH_CSV)
trip_df = pd.read_csv(TRIP_CSV)
facility_bool_df = pd.read_csv(FACILITY_BOOL_CSV)

hh_poverty_df=pd.read_csv(HH_POVERTY_CSV)
# Don't need all the columns
hh_poverty_filtered_df = hh_poverty_df[['hhInc_continuous', 'poverty_status', 'hh_id']]

# Print out the number of rows in the files read
print()  # This prints a blank line
print("----------------------------------------------")
print("Print out the number of rows in the files read")
print("----------------------------------------------")
print(f"Number of rows in hh_df           : {len(hh_df)}")
print(f"Number of rows in hh_poverty_df   : {len(hh_poverty_df)}")
print(f"Number of rows in trip_df         : {len(trip_df)}")
print(f"Number of rows in facility_bool_df: {len(facility_bool_df)}")
print()  # This prints a blank line

# Join the trips with the facility booleans
tripFacility_df = pd.merge(trip_df, facility_bool_df, on='trip_id', how='outer', indicator=True)

# Have a look at the merge indicator
merge_indicator_counts = tripFacility_df['_merge'].value_counts()
print()  # This prints a blank line
print("----------------------------------------------")
print("After joining the trips with the facility booleans,")
print("check the merge indicator in tripFacility_df")
print("----------------------------------------------")
print(merge_indicator_counts)
print()  # This prints a blank line
print("Note 1: We would expect that the numbers in 'both' to be the same as the number of rows in facility_bool_df.")
print("Note 2: We would expect that the numbers in 'right_only' to be zero.")
print()  # This prints a blank line

# Done with the merge indicator. Drop it.
tripFacility_df = tripFacility_df.drop(columns=['_merge'])

# Join the trips + facility booleans to households
hhTripFacility_df = pd.merge(hh_df, tripFacility_df, on='hh_id', how='outer', indicator=True)

# Have a look at the merge indicator
merge_indicator_counts = hhTripFacility_df['_merge'].value_counts()
print("----------------------------------------------")
print("Check the merge indicator in hhTripFacility_df")
print("----------------------------------------------")
print(merge_indicator_counts)
print()  # This prints a blank line
print("Note 1: We would expect that the number in 'both' to be the same as the number of rows in trip_df i.e. every trip is associated with a household")
print("Note 2: If 'left_only' is non-zero, it indicates that this number of households made zero trips during the survey period.")
print()  # This prints a blank line

# Done with the merge indicator. Drop it.
hhTripFacility_df = hhTripFacility_df.drop(columns=['_merge'])

# Write the file to csv for checking
output_test1 = os.path.join(OUTPUT_DIR, 'hhTripFacility.csv')
hhTripFacility_df.to_csv(output_test1, index=False)

# ************************************************************************
# Add poverty status
# ************************************************************************

# join the trips + facility booleans + households + poverty status
TripFacilityhhPoverty_df = pd.merge(hhTripFacility_df, hh_poverty_filtered_df, on='hh_id', how='outer', indicator=True)

# Write the file to csv for checking
output_test2 = os.path.join(OUTPUT_DIR, 'TripFacilityhhPoverty.csv')
TripFacilityhhPoverty_df.to_csv(output_test2, index=False)

# ************************************************************************
# Summarize
# ************************************************************************
# Note that BATS 2023 Facility Use Booleans.csv have directions
# Bay Bridge (San Francisco-Oakland): Tolls are collected only in the westbound direction (towards San Francisco).
# Richmond-San Rafael Bridge: Tolls are collected only in the westbound direction (towards San Rafael).
# San Mateo-Hayward Bridge: Tolls are collected only in the westbound direction (towards San Mateo).
# Dumbarton Bridge: Tolls are collected only in the westbound direction (towards the Peninsula).
# Carquinez Bridge: Tolls are collected only in the southbound direction (towards Vallejo).
# Benicia-Martinez Bridge: Tolls are collected only in the southbound direction (towards Benicia).

TripFacilityhhPoverty_df['num_BATAtoll'] = (
    TripFacilityhhPoverty_df['bay_bridge_toll'] + 
    TripFacilityhhPoverty_df['sm_bridge_toll'] + 
    TripFacilityhhPoverty_df['dum_bridge_toll'] + 
    TripFacilityhhPoverty_df['rsr_bridge_toll'] + 
    TripFacilityhhPoverty_df['carq_bridge_toll'] + 
    TripFacilityhhPoverty_df['bm_bridge_toll']
)


# Group by 'hh_id' and aggregate
TripFacilityhhPoverty_grouped_df = TripFacilityhhPoverty_df.groupby('hh_id').agg(
    num_BATAtoll=('num_BATAtoll', 'sum'),
    poverty_status=('poverty_status', 'first'),
    hh_weight_rmove_only=('hh_weight_rmove_only', 'first')
).reset_index()

# Group by poverty_status and get the frequency distribution of num_BATAtoll UNWEIGHTED
num_BATAtoll_distribution_by_poverty_UNweighted_df = (
    TripFacilityhhPoverty_grouped_df.groupby('poverty_status')['num_BATAtoll']
    .value_counts()
    .sort_index()
    .reset_index(name='frequency')
)

print("----------------------------------------------")
print("Print out unweighted frequency distribution")
print("----------------------------------------------")
#print(num_BATAtoll_distribution_by_poverty_weighted_df)
print(num_BATAtoll_distribution_by_poverty_UNweighted_df)
print()  # This prints a blank line

# Create a weighted frequency distribution
num_BATAtoll_distribution_by_poverty_weighted_df = (
    TripFacilityhhPoverty_grouped_df.groupby(['poverty_status', 'num_BATAtoll'])['hh_weight_rmove_only']
    .sum()
    .reset_index(name='weighted_frequency')
)

print("----------------------------------------------")
print("Print out weighted frequency distribution")
print("----------------------------------------------")
print(num_BATAtoll_distribution_by_poverty_weighted_df)
print()  # This prints a blank line

# next steps: do MoE calcs

# I think the MoE calc was the reason why I think this is a scripting exercise 