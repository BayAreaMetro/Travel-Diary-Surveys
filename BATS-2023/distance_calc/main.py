# For expediency, I used crow-fly distances in the BATS summaries for the upcoming presentations. Ideally, we’d calculate trip distances directly from the location points instead.

# We’d like to do this for the linked trips:

# Daysim output trip file:
# M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/TripLinking_20251012/03b-assign_day/wt-wkday_4day/trip.csv
# E:/Box/Modeling and Surveys/Surveys/Travel Diary Survey/MPO Partner Household Travel Survey (Includes 2018_2019 TNC Survey)/Bay Area Travel Study 2018-2019/Data from 2018_2019 Survey/BATS2018_2019_SFCTA_processed/20200228/survey2018_tripx.dat

# The location.csv is in:
# E:\Box\Modeling and Surveys\Surveys\Travel Diary Survey\BATS_2023\MTC_RSG_Partner Repository\5.Deliverables\Task 10 - Weighting and Expansion Data Files\UnweightedDataset\location.csv
# E:\Box\Modeling and Surveys\Surveys\Travel Diary Survey\BATS_2023\MTC_RSG_Partner Repository\5.Deliverables\Task 10 - Weighting and Expansion Data Files\UnweightedDataset\trip.csv

# Load in data
import math
from pathlib import Path

import polars as pl

# M: network path
M_NETDIR = r"\\models.ad.mtc.ca.gov\data\models"

# Paths to data files
survey_dir = "E:/Box/Modeling and Surveys/Surveys/Travel Diary Survey/BATS_2023/MTC_RSG_Partner Repository/5.Deliverables/Task 10 - Weighting and Expansion Data Files/UnweightedDataset/"
daysim_dir = "M:/Data/HomeInterview/Bay Area Travel Study 2023/Data/Processed/TripLinking_20251012/03b-assign_day/wt-wkday_4day/"


# Replace "M:\" with network path
daysim_dir = daysim_dir.replace("M:", M_NETDIR)


trip_path = Path(survey_dir) / "trip.csv"
location_path = Path(survey_dir) / "location.csv"
linked_trip_path = Path(daysim_dir) / "trip.csv"

# Check if files exist
# Load survey trip and location data
survey_trips = pl.read_csv(trip_path)
survey_locations = pl.read_csv(location_path)
linked_trips = pl.read_csv(linked_trip_path)


# Locations has trip_id, collect_time, and lat/lon

# NOTE Key columns to map on:
# linked_trips has hhno=hh_id, pno=person_num, day=travel_dow
# deptm and arrtm as time converted to integer e.g., hmm with no leading zeros

# Prepare survey trips data
survey_trips = (
    survey_trips.with_columns(
        # Create datetime columns for depart and arrive times
        pl.concat_str([
            pl.col("depart_date"),
            pl.lit(" "),
            pl.col("depart_hour").cast(pl.Utf8).str.zfill(2),
            pl.lit(":"),
            pl.col("depart_minute").cast(pl.Utf8).str.zfill(2),
            pl.lit(":"),
            pl.col("depart_seconds").cast(pl.Utf8).str.zfill(2),
        ]).str.strptime(pl.Datetime, format="%Y-%m-%d %H:%M:%S").alias("depart_time"),
        pl.concat_str([
            pl.col("arrive_date"),
            pl.lit(" "),
            pl.col("arrive_hour").cast(pl.Utf8).str.zfill(2),
            pl.lit(":"),
            pl.col("arrive_minute").cast(pl.Utf8).str.zfill(2),
            pl.lit(":"),
            pl.col("arrive_second").cast(pl.Utf8).str.zfill(2),
        ]).str.strptime(pl.Datetime, format="%Y-%m-%d %H:%M:%S").alias("arrive_time"),
        # Create arrtm and deptm integer columns for joining
        (pl.col("depart_hour").cast(pl.Int16) * 100 + pl.col("depart_minute").cast(pl.Int16)).alias("deptm"),
        (pl.col("arrive_hour").cast(pl.Int16) * 100 + pl.col("arrive_minute").cast(pl.Int16)).alias("arrtm"),
        # day (aka dow), 1=Monday, 7=Sunday
        pl.col("depart_date").str.strptime(pl.Date, format="%Y-%m-%d").dt.weekday().alias("day").cast(pl.Int8),
        # Create hno (aka hh_id) and pno (aka person_num) columns for joining
        pl.col("hh_id").alias("hhno"),
        pl.col("person_num").alias("pno"),
    )
)        


# Prepare linked trips data
linked_trips = (
    linked_trips.with_columns(
        # Create dummy linked trip ID for mapping from nrows
        pl.arange(0, pl.len()).alias("ltrip_id"),
        # Cast hhno and day to i8
        pl.col("hhno").cast(pl.Int64),
        pl.col("day").cast(pl.Int8),
    )
)

# Step 1: Filtered join ltrip_id onto survey_trips using time window
# Join on hhno, pno, day and then asof on deptm/arrtm
lnk_int = linked_trips.with_columns([
    pl.col("deptm").alias("start_lnk"),
    pl.col("arrtm").alias("end_lnk"),
])

survey_trips_joined = (
    survey_trips
    .join(
        lnk_int,
        on=["hhno", "pno", "day"],
        how="inner"
    )
    .filter(
        (pl.col("deptm") >= pl.col("start_lnk")) &
        (pl.col("deptm") <= pl.col("end_lnk"))
    )
)

# Check that all linked trips matched to survey trips
linked_trips.filter(~pl.col("ltrip_id").is_in(survey_trips_joined["ltrip_id"].implode())).select(
    ["ltrip_id", "hhno", "pno", "day", "deptm", "arrtm"]
)

# Step 2: Sum up trip distance_meters from survey_trips onto ltrip_id
linked_trips = (
    linked_trips.join(
        (
            survey_trips_joined
            .group_by("ltrip_id")
            .agg(
                pl.sum("distance_meters").alias("sum_distance_meters")
            )
        ),
        on="ltrip_id",
        how="left"
    )
)


# Calculate distances for each trip using location points

def haversine_expr(lat1, lon1, lat2, lon2):
    R = 6371000.0
    RAD = math.pi / 180

    """Return a Polars expression computing haversine distance in meters."""
    lat1r = (pl.col(lat1) * RAD)
    lon1r = (pl.col(lon1) * RAD)
    lat2r = (pl.col(lat2) * RAD)
    lon2r = (pl.col(lon2) * RAD)

    dlat = lat2r - lat1r
    dlon = lon2r - lon1r

    return 2 * R * (
        (
            (dlat / 2).sin().pow(2)
            + lat1r.cos() * lat2r.cos()
            * (dlon / 2).sin().pow(2)
        ).sqrt().arcsin()
    )

path_distance = (
    survey_locations
    .sort(["trip_id"])      # or ["trip_id", "timestamp"]
    .with_columns([
        pl.col("lat").shift().over("trip_id").alias("lat_prev"),
        pl.col("lon").shift().over("trip_id").alias("lon_prev"),
    ])
    .with_columns([
        haversine_expr("lat_prev", "lon_prev", "lat", "lon")
        .alias("segment_distance_m")
    ])
    .join(
        survey_trips_joined.select(["trip_id", "ltrip_id"]),
        on="trip_id",
        how="left"
    )
    .group_by("ltrip_id")
    .agg(
        pl.col("segment_distance_m")
            .fill_null(0)
            .sum()
            .alias("path_distance_meters")
    )
)

# Join path_distance back onto linked_trips to compare
linked_trips = (
    linked_trips.join(
        path_distance,
        on="ltrip_id",
        how="left"
    )
)

