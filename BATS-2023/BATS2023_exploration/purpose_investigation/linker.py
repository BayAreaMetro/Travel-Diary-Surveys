"""Trip Linker Module."""
import logging

import pandera.polars as pa
import polars as pl

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# Dta Models -------------------------------------------------------------------
# Minimal data schema for trip linking
class TripModel(pa.DataFrameModel):
    """Trip data model for validation."""

    trip_id: pl.Int64 = pa.Field(ge=1)
    day_id: pl.Int64 = pa.Field(ge=1)
    person_id: pl.Int64 = pa.Field(ge=1)
    hh_id: pl.Int64 = pa.Field(ge=1)
    depart_date: pl.Utf8
    depart_hour: pl.Int64 = pa.Field(ge=0, le=23)
    depart_minute: pl.Int64 = pa.Field(ge=0, le=59)
    depart_seconds: pl.Int64 = pa.Field(ge=0, le=59)
    arrive_date: pl.Utf8
    arrive_hour: pl.Int64 = pa.Field(ge=0, le=23)
    arrive_minute: pl.Int64 = pa.Field(ge=0, le=59)
    arrive_seconds: pl.Int64 = pa.Field(ge=0, le=59)
    o_purpose_category: pl.Int64
    d_purpose_category: pl.Int64
    mode_type: pl.Int64

    # Optional (column may be absent), but not nullable
    depart_time: pl.Datetime | None = pa.Field(ge=0)
    arrive_time: pl.Datetime | None = pa.Field(ge=0)


# Subclassing allows you to extend TripModel cleanly
class LinkedTripModel(TripModel):
    """Linked Trip data model for validation."""

    trip_id: None
    linked_trip_id: pl.Int64 = pa.Field(ge=1, nullable=True)

# Helper Functions -------------------------------------------------------------
def datetime_from_parts(
    date: pl.Series,
    hour: pl.Series,
    minute: pl.Series,
    second: pl.Series,
) -> pl.Series:
    """Construct datetime from date and time parts."""
    return pl.concat_str([
        date,
        pl.lit("T"),
        hour.cast(pl.Utf8).str.pad_start(2, "0"),
        pl.lit(":"),
        minute.cast(pl.Utf8).str.pad_start(2, "0"),
        pl.lit(":"),
        second.cast(pl.Utf8).str.pad_start(2, "0"),
    ]).str.to_datetime()



# Trip Linker Class ------------------------------------------------------------
class LinkerTinker:
    """Class to link trips based on purpose and mode in time sequence."""

    change_mode_code: int
    transit_mode_codes: list[int]

    trips: pl.DataFrame
    linked_trips: pl.DataFrame

    def __init__(
        self,
        trips_df: pl.DataFrame,
        change_mode_code: int,
        transit_mode_codes: list[int],
    ) -> None:
        """Initialize LinkerTinker with trip data."""
        self.trips = TripModel.validate(trips_df)
        self.change_mode_code = change_mode_code
        self.transit_mode_codes = transit_mode_codes

        logger.info("LinkerTinker initialized.")

    def link_trips(self) -> None:
        """Link trips based on purpose and mode in time sequence."""
        self.add_time_columns()
        self.link_trip_ids()
        self.populate_linked_trips()
        logger.info("Trip linking completed.")

    def add_time_columns(self) -> None:
        """Add datetime columns for departure and arrival times if missing."""
        logger.info("Adding datetime columns...")
        if "depart_time" not in self.trips.columns:
            logger.info("Constructing depart_time...")
            self.trips = self.trips.with_columns([
                datetime_from_parts(
                    pl.col("depart_date"),
                    pl.col("depart_hour"),
                    pl.col("depart_minute"),
                    pl.col("depart_seconds"),
                ).alias("depart_time"),
            ])
        if "arrive_time" not in self.trips.columns:
            logger.info("Constructing arrive_time...")
            self.trips = self.trips.with_columns([
                datetime_from_parts(
                    pl.col("arrive_date"),
                    pl.col("arrive_hour"),
                    pl.col("arrive_minute"),
                    pl.col("arrive_seconds"),
                ).alias("arrive_time"),
            ])
        # Revalidate after adding datetime columns
        self.trips = TripModel.validate(self.trips)

    def link_trip_ids(self) -> None:
        """Link trips based on purpose and mode in time sequence.

        Logic:
        For each person's day of trips:
         - Sort trips by departure time
         - If previous trip's destination purpose is 'change_mode',
           continue the linked trip; else start a new linked trip.
         - Assign linked trip IDs accordingly.
         Linked trip IDs are made globally unique by combining day_id and
         local linked trip index.
        """
        logger.info("Linking trip IDs...")
        # Step 1: Sort trips by day and departure time
        linked_trips = self.trips.sort(["day_id", "depart_time"])

        # Step 2: Get previous trip purpose category within the same person-day
        linked_trips = linked_trips.with_columns([
            pl.col("d_purpose_category")
            .shift(fill_value=None)
            .over("day_id")
            .alias("prev_purpose"),
        ])

        # Step 3: Is new linked trips when prev is not change_mode or missing
        linked_trips = linked_trips.with_columns([
            (
                (pl.col("prev_purpose") != self.change_mode_code) |
                pl.col("prev_purpose").is_null()
            )
            .cast(pl.Int32)
            .alias("new_trip_flag"),
        ])

        # Step 4: Assign linked trip IDs using cumulative sum
        linked_trips = linked_trips.with_columns([
            pl.col("new_trip_flag")
            .cum_sum()
            .over("day_id")
            .alias("linked_trip_id"),
        ])

        # Step 5: Make linked_trip_id globally unique across days
        linked_trips = linked_trips.with_columns([
            (
                pl.col("day_id").cast(pl.Utf8) +
                pl.col("linked_trip_id").cast(pl.Utf8).str.pad_start(2, "0")
            )
            .cast(pl.Int64)
            .alias("linked_trip_id"),
        ])

        # Step 6: Clean up temporary columns
        linked_trips = linked_trips.drop(["prev_purpose", "new_trip_flag"])

        # Join linked trip IDs back to the original trip data
        self.trip = self.trips.join(
            linked_trips.select(["trip_id", "linked_trip_id"]),
            on="trip_id",
            how="left",
        )

    def populate_linked_trips(self) -> None:
        """Aggregate linked trips into single records, summarizing key info.

        Logic:
        For each linked trip:
         - keep the first trips depart_* and o_* fields
        - keep the last trips arrive_* and d_* fields
        - Mode is based on hierarchy. Simple case:
        If:
            transit is involved in any trip segment, use transit mode.
        Else:
            Use mode of longest duration trip segment.
        """
        logger.info("Populating linked trips...")

        # Calculate trip durations and sort trips accordingly
        trips_sorted = (
            self.trip
            .with_columns([
                (pl.col("arrive_time") - pl.col("depart_time"))
                .alias("trip_duration"),
            ])
            # pre-sort globally so longest trip per linked_trip_id is first
            .sort(["linked_trip_id", "trip_duration"], descending=[False, True])
        )

        # Aggregate trip segments by linked_trip_id
        self.linked_trips = (
            trips_sorted.group_by(
                ["linked_trip_id", "day_id", "person_id", "hh_id"]
            )
            .agg([
                # Departure information (from first trip segment)
                pl.first("depart_date"),
                pl.first("depart_hour"),
                pl.first("depart_minute"),
                pl.first("depart_seconds"),
                pl.first("depart_time"),
                pl.first("o_purpose_category"),

                # Arrival information (from last trip segment)
                pl.last("arrive_date"),
                pl.last("arrive_hour"),
                pl.last("arrive_minute"),
                pl.last("arrive_seconds"),
                pl.last("arrive_time"),
                pl.last("d_purpose_category"),

                # Mode selection: transit mode if present
                pl.col("mode_type")
                    .filter(pl.col("mode_type").is_in(self.transit_mode_codes))
                    .first()
                    .alias("mode_transit"),
                # Mode selection: longest non-transit mode otherwise
                # (because we sorted globally, .first() will give longest trip)
                pl.col("mode_type")
                    .filter(~pl.col("mode_type").is_in(self.transit_mode_codes))
                    .first()
                    .alias("mode_non_transit"),
                # Linked trip weight (mean of segment weights)
                pl.col("trip_weight").mean().alias("linked_trip_weight"),
            ])
            # Determine final mode: prioritize transit, fall back to longest
            .with_columns([
                pl.when(pl.col("mode_transit").is_not_null())
                .then(pl.col("mode_transit"))
                .otherwise(pl.col("mode_non_transit"))
                .alias("mode_type"),
            ])
            # Clean up temporary mode columns
            .drop(["mode_transit", "mode_non_transit"])
        )

        # Validate final linked trips
        self.linked_trips = LinkedTripModel.validate(self.linked_trips)

