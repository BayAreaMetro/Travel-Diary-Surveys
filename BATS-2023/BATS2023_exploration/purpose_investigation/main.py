"""Main script for purpose investigation in BATS 2023 dataset."""
import logging
from pathlib import Path

import plotly.express as px
import polars as pl

from linker import LinkerTinker

logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO)

# Not really sure where everything should live but all this is pretty sinful
DATA_DIR = Path("E:\\Box\\Modeling and Surveys\\Surveys\\Travel Diary Survey")
DATA_DIR = DATA_DIR / "BATS_2023\\MTC_RSG_Partner Repository\\5.Deliverables"
DATA_DIR = DATA_DIR / "Task 10 - Weighting and Expansion Data Files"
DATA_DIR = DATA_DIR / "WeightedDataset_12112024"
TRIP_PATH = DATA_DIR / "trip.csv"



CHANGE_MODE_CODE: int = 11  # Purpose code for 'change_mode'
TRANSIT_MODES: list[str] = [12, 13, 14]


# Some debugging...
if __name__ == "__main__":
    # Load data
    trips_df = pl.read_csv(TRIP_PATH)

    # Much wow RSG...
    trips_df = trips_df.rename({"arrive_second": "arrive_seconds"})

    # Instantiate BATS Handler
    BATS = LinkerTinker(
        trips_df,
        change_mode_code=CHANGE_MODE_CODE,
        transit_mode_codes=TRANSIT_MODES,
    )

    # Link the trips
    BATS.link_trips()

    # Get linked trips dataframe
    linked_trips_df = BATS.linked_trips

    # Mode Mapping for readability
    MODE_TYPE_MAP = {
        1: "Walk",
        2: "Bike",
        3: "Bikeshare",
        4: "Scootershare",
        5: "Taxi",
        6: "TNC",
        7: "Other",
        8: "Car",
        9: "Carshare",
        10: "School bus",
        11: "Shuttle/vanpool",
        12: "Ferry",
        13: "Transit",
        14: "Long distance passenger",
        995: "Missing Response",
    }

    # Get school trips by mode -------
    school_mode = (
        linked_trips_df
        .filter(
            pl.col("d_purpose_category").is_in([3,4]) &
            (pl.col("linked_trip_weight") > 0), # Drop incomplete trips
            )
        .group_by("mode_type").agg([
            pl.len().alias("count"),
            pl.col("linked_trip_weight").sum().alias("weighted_count"),
        ])
        .with_columns(
            pl.col("mode_type").cast(pl.Utf8).replace(MODE_TYPE_MAP).alias("mode"),
        )
        .sort("count", descending=True)
        .drop("mode_type")
        .with_columns([
            (pl.col("count") / pl.col("count").sum() * 100).alias("share"),
            (pl.col("weighted_count") / pl.col("weighted_count").sum() * 100)
            .alias("weighted_share"),
        ])
    )

    # Reshape for plotting
    school_mode_long = school_mode.unpivot(
        index=["mode", "count", "weighted_count"],
        on=["share", "weighted_share"],
        variable_name="type",
        value_name="percentage",
    ).with_columns([
        pl.when(pl.col("type") == "share")
            .then(pl.col("count"))
            .otherwise(pl.col("weighted_count").round(0).cast(pl.Int32))
            .alias("text_value"),
        pl.col("type")
        .replace({"share": "Unweighted", "weighted_share": "Weighted"}),
    ])

    # Interative plotly plot
    fig = px.bar(
        school_mode_long,
        x="mode",
        y="percentage",
        color="type",
        barmode="group",
        labels={"mode": "Mode Type", "percentage": "Share (%)", "type": ""},
        text="text_value",
        title="School Trips by Mode",
    )
    fig.update_traces(texttemplate="%{text:.3~s}", textposition="outside")
    fig.show()
