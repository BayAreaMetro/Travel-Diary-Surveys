"""Completeness analysis.

Builds completeness flags for households, persons, days, trips, person-days and
household-days, and summarises which fields contribute to incompleteness.
"""
import sys
from datetime import datetime
from pathlib import Path
import polars as pl

pl.Config.set_tbl_rows(-1)
pl.Config.set_fmt_str_lengths(80)

# ---------- Config ----------
BASE = Path(r"E:\Box\Modeling and Surveys\Surveys\Travel Diary Survey\BATS_2023"
            r"\Versioned_Data\PreWeight_PreLink_MonToSun_20250610")   # read-only
HH_FILE = BASE / "hh.csv"
PERSON_FILE = BASE / "person.csv"
DAY_FILE = BASE / "day.csv"
TRIP_FILE = BASE / "trip.csv"

OUT_DIR = Path(r"E:\BATS_no_travel_analysis")   # parent output directory
# Each run writes to its own time-stamped subfolder, e.g. completeness_20261103_055900
RUN_DIR = OUT_DIR / f"completeness_{datetime.now():%Y%m%d_%H%M%S}"
RUN_DIR.mkdir(parents=True, exist_ok=True)


# Everything printed to the console is also written to run_log.txt in the run folder.
class Tee:
    def __init__(self, console, log):
        self.console, self.log = console, log

    def write(self, text):
        self.console.write(text)
        self.log.write(text)

    def flush(self):
        self.console.flush()
        self.log.flush()

    def __getattr__(self, name):   # e.g. isatty, encoding: defer to the console
        return getattr(self.console, name)


_log_file = open(RUN_DIR / "run_log.txt", "w", encoding="utf-8")
sys.stdout = Tee(sys.__stdout__, _log_file)

JOIN_KEY = "person_id"
TRIPS_COL = "num_trips"

STATUS_TRIPS_COMPLETE = "At least 1 trip, complete responses"
STATUS_TRIPS_INCOMPLETE = "At least 1 trip, but incomplete"
STATUS_NO_TRAVEL = "Zero trips, reason given"


def require(df, cols, name):
    """Fail early with a clear message if expected columns are missing."""
    missing = [c for c in cols if c not in df.columns]
    assert not missing, f"{name} is missing columns: {missing}"


def report(label, n, total):
    print(f"{label}: {n:,} of {total:,}")


# ---------- Load ----------
hh = pl.read_csv(HH_FILE, infer_schema_length=None)
person = pl.read_csv(PERSON_FILE, infer_schema_length=None)
day = pl.read_csv(DAY_FILE, infer_schema_length=None)
trip = pl.read_csv(TRIP_FILE, infer_schema_length=None)

# ---------- Completeness: household and person ----------
require(hh, ["hh_id", "num_vehicles", "num_adults", "num_kids", "num_people",
             "num_workers", "home_lat", "home_lon"], "hh.csv")
require(person, [JOIN_KEY, "hh_id", "employment", "job_type", "telework_freq",
                 "work_lat", "work_lon", "work_park", "student",
                 "school_lat", "school_lon"], "person.csv")

# Null in any condition counts as "not met" (same as pandas between/notna).
hh = hh.with_columns(
    pl.all_horizontal([
        pl.col("num_vehicles").is_between(0, 8),
        pl.col("num_adults").is_between(1, 13),
        pl.col("num_kids").is_between(0, 12),
        pl.col("num_people").is_between(1, 13),
        pl.col("num_workers").is_between(0, 13),
        pl.col("home_lat").is_not_null(),
        pl.col("home_lon").is_not_null(),
    ]).fill_null(False).cast(pl.Int8).alias("hh_complete")
)
report("Complete households", hh["hh_complete"].sum(), hh.height)

employed = pl.col("employment").is_in([1, 2, 3, 7])
telework_ok = pl.col("telework_freq").is_in(list(range(1, 9)) + [996])   # 996 = "Never" (valid)

person_rules = [
    # employed, job_type 3 (work only from home): no telework_freq needed
    employed & (pl.col("job_type") == 3),
    # employed, job_type 2 or 4: telework_freq only
    employed & pl.col("job_type").is_in([2, 4]) & telework_ok,
    # employed, job_type 1 or 5: telework_freq AND work location info
    employed & pl.col("job_type").is_in([1, 5]) & telework_ok
    & pl.col("work_lat").is_not_null() & pl.col("work_lon").is_not_null()
    & pl.col("work_park").is_between(1, 4),
    # not employed (5, 6, 8): complete if employment status present
    # (employment = 4 is not in the codebook, so it matches no rule here -> incomplete)
    pl.col("employment").is_in([5, 6, 8]),
    # students: school location present
    pl.col("student").is_in([0, 1]) & pl.col("school_lat").is_not_null() & pl.col("school_lon").is_not_null(),
    # non-students / online-only students: student status present
    pl.col("student").is_in([2, 3, 4]),
]
# A person is complete if ANY rule is met (employment and student
# rules are not mutually exclusive).
person = person.with_columns(
    pl.any_horizontal([r.fill_null(False) for r in person_rules])
      .cast(pl.Int8).alias("person_complete")
)
report("Complete persons", person["person_complete"].sum(), person.height)

# ---------- Add employment to day ----------
# Guard against duplicate person rows silently inflating the day file
assert person[JOIN_KEY].is_unique().all(), f"{JOIN_KEY} not unique in person.csv"

n_before = day.height
n_no_person_match = day.join(person.select(JOIN_KEY), on=JOIN_KEY, how="anti").height
day = day.join(person.select([JOIN_KEY, "employment"]), on=JOIN_KEY, how="left")
assert day.height == n_before, "Join changed row count"

# Unmatched day rows always have null employment, so subtract them to get the rest.
n_employment_blank = day["employment"].null_count() - n_no_person_match
print(f"Day rows: {day.height:,}")
print(f"  whose person_id is not in person.csv: {n_no_person_match:,}")
print(f"  whose person_id is in person.csv but employment is blank: {n_employment_blank:,}\n")

# ---------- Check: people in the person file vs the day file ----------
# Context: in BATS 2023, the person file contains more individuals than the day file
# because, in households comprising both family members and unrelated household members,
# travel data were collected only from family members (a.k.a. "roommategate").
persons_in_day = day.select(JOIN_KEY).unique()
persons_not_in_day = person.join(persons_in_day, on=JOIN_KEY, how="anti")
persons_only_in_day = persons_in_day.join(person.select(JOIN_KEY), on=JOIN_KEY, how="anti")
print(f"Persons in person.csv: {person.height:,}")
print(f"Persons with at least one row in day.csv: {persons_in_day.height:,}")
print(f"Persons in person.csv with no rows in day.csv: {persons_not_in_day.height:,}")
print(f"Persons in day.csv but not in person.csv: {persons_only_in_day.height:,}")
print("Note: in BATS 2023, the person file contains more individuals than the day file "
      "because, in households comprising both family members and unrelated household "
      "members, travel data were collected only from family members "
      "(a.k.a. \"roommategate\").\n")

# ---------- Completeness: day and trip ----------
require(day, ["hh_id", "person_num", "day_num", "telecommute_time", "num_reasons_no_travel"], "day.csv")
require(trip, ["hh_id", JOIN_KEY, "person_num", "day_num",
               "o_purpose_category", "d_purpose_category", "mode_type",
               "depart_hour", "depart_minute", "depart_seconds",
               "arrive_hour", "arrive_minute", "arrive_second",
               "o_lon", "o_lat", "d_lon", "d_lat"], "trip.csv")

# Telecommute time: only employed people (1, 2, 3, 7) are asked, so they are complete when
# telecommute_time is answered. Everyone else is complete without it.
is_employed_day = pl.col("employment").is_in([1, 2, 3, 7]).fill_null(False)
day = day.with_columns(
    (~is_employed_day | pl.col("telecommute_time").is_not_null())
    .cast(pl.Int8).alias("telecommute_time_complete")
)
report("Person-days with complete telecommute time (required of employed persons only)",
       day["telecommute_time_complete"].sum(), day.height)

# Trip completeness
trip_present_cols = ["depart_hour", "depart_minute", "depart_seconds",
                     "arrive_hour", "arrive_minute", "arrive_second",
                     "o_lon", "o_lat", "d_lon", "d_lat"]
trip = trip.with_columns(
    pl.all_horizontal([
        pl.col("o_purpose_category").is_between(1, 13),
        pl.col("d_purpose_category").is_between(1, 13),
        pl.col("mode_type").is_between(1, 14),
        *[pl.col(c).is_not_null() for c in trip_present_cols],
    ]).fill_null(False).alias("trip_complete")
)
report("Complete trips", trip["trip_complete"].sum(), trip.height)

# Person-trip completeness: household x person x telecommute x trip all complete
trip = (
    trip.join(hh.select(["hh_id", "hh_complete"]), on="hh_id", how="left", validate="m:1")
        .join(person.select([JOIN_KEY, "person_complete"]), on=JOIN_KEY, how="left", validate="m:1")
        .join(day.select(["hh_id", "person_num", "day_num", "telecommute_time_complete"]),
              on=["hh_id", "person_num", "day_num"], how="left", validate="m:1")
        .with_columns(
            ((pl.col("hh_complete") == 1)
             & (pl.col("person_complete") == 1)
             & (pl.col("telecommute_time_complete") == 1)
             & pl.col("trip_complete"))
            .fill_null(False).alias("personTrip_complete")
        )
)
print(f"Trips with no matching day row: {trip['telecommute_time_complete'].null_count():,}\n")

# ---------- Person-day file, built from the trip file ----------
# Step 1: one row per person-day that has trips, summarising that day's trips.
# Step 2: add person-days with no trips but a no-travel reason (num_reasons_no_travel > 0).
# Step 3: merge in the relevant day-file, person and household variables.
# Day-file rows with no trips and no reason are not part of this file.
trip_person_day = (
    trip.group_by(["hh_id", JOIN_KEY, "day_num"])
        .agg(
            pl.len().alias("n_trips_in_trip_file"),
            (~pl.col("trip_complete")).sum().alias("n_trip_details_incomplete"),
            (~pl.col("personTrip_complete")).sum().alias("n_incomplete_trips"),
        )
        .with_columns((pl.col("n_incomplete_trips") == 0).alias("allTrips_complete"))
)

reason_days = (
    day.filter(pl.col("num_reasons_no_travel").fill_null(0) > 0)
       .select(["hh_id", JOIN_KEY, "day_num"])
       .join(trip_person_day.select(["hh_id", JOIN_KEY, "day_num"]),
             on=["hh_id", JOIN_KEY, "day_num"], how="anti")
)

person_day = (
    pl.concat([trip_person_day, reason_days], how="diagonal")
      .join(day.select(["hh_id", JOIN_KEY, "day_num", "person_num", TRIPS_COL,
                        "num_reasons_no_travel", "telecommute_time",
                        "telecommute_time_complete"]),
            on=["hh_id", JOIN_KEY, "day_num"], how="left", validate="m:1")
      .join(person.select([JOIN_KEY, "employment", "person_complete"]),
            on=JOIN_KEY, how="left", validate="m:1")
      .join(hh.select(["hh_id", "hh_complete"]), on="hh_id", how="left", validate="m:1")
      .with_columns(pl.col("n_trips_in_trip_file").fill_null(0))
)

# A person-day is complete if all its trips are complete, or it has no trips and a
# no-travel reason was given.
has_trips = pl.col("n_trips_in_trip_file") > 0
person_day = person_day.with_columns(
    pl.when(has_trips).then(pl.lit("num_trips > 0"))
      .otherwise(pl.lit("num_trips = 0"))
      .alias("trip_group"),
    pl.when(has_trips & pl.col("allTrips_complete")).then(pl.lit(STATUS_TRIPS_COMPLETE))
      .when(has_trips).then(pl.lit(STATUS_TRIPS_INCOMPLETE))
      .otherwise(pl.lit(STATUS_NO_TRAVEL))
      .alias("person_day_completeness"),
).with_columns(
    pl.col("person_day_completeness").is_in([STATUS_TRIPS_COMPLETE, STATUS_NO_TRAVEL])
      .alias("personDay_complete")
)

print("Person-days in the person-day file "
      f"(trip.csv days + day.csv no-travel days with a reason given): {person_day.height:,}")
print(f"  trip.csv days: {trip_person_day.height:,}")
print(f"  day.csv no-travel days with a reason given: {reason_days.height:,}")

# Day-file rows that did not make it into the person-day file
left_out_days = day.join(person_day.select(["hh_id", JOIN_KEY, "day_num"]),
                         on=["hh_id", JOIN_KEY, "day_num"], how="anti")
print(f"Day-file rows not in the person-day file (zero trips, no reason): "
      f"{left_out_days.height:,} of {day.height:,}")

# Reconcile day.csv trip counts with trip.csv
n_trip_mismatch = person_day.filter(
    pl.col(TRIPS_COL).is_not_null() & (pl.col(TRIPS_COL) != pl.col("n_trips_in_trip_file"))
).height
n_left_out_with_trips = left_out_days.filter(pl.col(TRIPS_COL).fill_null(0) > 0).height
print(f"Person-days where {TRIPS_COL} in day.csv differs from trips in trip.csv: {n_trip_mismatch:,}")
print(f"Remaining rows in day.csv that didn't make it into the person-day file "
      f"but report {TRIPS_COL} > 0: {n_left_out_with_trips:,}\n")

# Household-day: a household-day is complete if every person-day in it is complete.
household_day = (
    person_day.group_by(["hh_id", "day_num"])
              .agg(pl.col("personDay_complete").all().alias("householdDay_complete"))
              .sort(["hh_id", "day_num"])
)
report("Household-days with complete responses among participating members",
       household_day["householdDay_complete"].sum(), household_day.height)

# Additional flag: household members missing from the day file.
# Expected members = people in person.csv for the household.
# Present = people with at least one row in day.csv for that household-day.
# householdDay_complete above only looks at person-days that exist, so it can be True
# when some members have no record; householdDay_complete_all_members also requires
# that nobody is missing.
n_in_person_file = person.group_by("hh_id").agg(pl.len().alias("n_persons_in_person_file"))
n_in_day_file = (
    day.group_by(["hh_id", "day_num"])
       .agg(pl.col(JOIN_KEY).n_unique().alias("n_persons_in_day_file"))
)
household_day = (
    household_day
    .join(n_in_person_file, on="hh_id", how="left", validate="m:1")
    .join(n_in_day_file, on=["hh_id", "day_num"], how="left", validate="1:1")
    .with_columns(
        (pl.col("n_persons_in_day_file") < pl.col("n_persons_in_person_file"))
        .fill_null(False).alias("members_missing_from_day_file")
    )
    .with_columns(
        (pl.col("householdDay_complete") & ~pl.col("members_missing_from_day_file"))
        .alias("householdDay_complete_all_members")
    )
)
report("Household-days with members missing from the day file",
       household_day["members_missing_from_day_file"].sum(), household_day.height)
report("Complete household-days where all members participated",
       household_day["householdDay_complete_all_members"].sum(), household_day.height)

# Household level: a household is complete if it has at least 1 complete household-day;
# a member is missing if they have no row in day.csv on any day.
household = (
    household_day.group_by("hh_id")
                 .agg(pl.col("householdDay_complete").any().alias("household_responses_complete"))
              .join(n_in_person_file, on="hh_id", how="left", validate="m:1")
              .join(day.group_by("hh_id").agg(pl.col(JOIN_KEY).n_unique().alias("n_persons_in_day_file")),
                    on="hh_id", how="left", validate="1:1")
              .with_columns(
                  (pl.col("n_persons_in_day_file") < pl.col("n_persons_in_person_file"))
                  .fill_null(False).alias("members_missing_from_day_file"))
              .sort("hh_id")
)
report("Households with members missing from the day file",
       household["members_missing_from_day_file"].sum(), household.height)
report("Households with at least 1 complete day among participating members",
       household["household_responses_complete"].sum(), household.height)

HH_LEVEL_STATUS_COL = "household_members_completeness"
HH_LEVEL_STATUS_ORDER = [
    "Members missing, no complete day",
    "Members missing, at least 1 complete day",
    "All members participated, no complete day",
    "All members participated, at least 1 complete day",
]
hh_missing_flag = pl.col("members_missing_from_day_file")
hh_complete_flag = pl.col("household_responses_complete")
household = household.with_columns(
    pl.when(hh_missing_flag & hh_complete_flag).then(pl.lit(HH_LEVEL_STATUS_ORDER[1]))
      .when(hh_missing_flag).then(pl.lit(HH_LEVEL_STATUS_ORDER[0]))
      .when(~hh_complete_flag).then(pl.lit(HH_LEVEL_STATUS_ORDER[2]))
      .otherwise(pl.lit(HH_LEVEL_STATUS_ORDER[3]))
      .alias(HH_LEVEL_STATUS_COL)
)
household_status_counts = (
    pl.DataFrame({HH_LEVEL_STATUS_COL: HH_LEVEL_STATUS_ORDER})
      .join(household.group_by(HH_LEVEL_STATUS_COL).agg(pl.len().alias("households")),
            on=HH_LEVEL_STATUS_COL, how="left")
      .with_columns(pl.col("households").fill_null(0))
      .with_columns((pl.col("households") / pl.col("households").sum() * 100)
                    .round(1).alias("pct_of_households"))
)
print("Households by members missing and at least 1 complete day")
print(household_status_counts)
print()

# Four mutually exclusive categories: members missing x responses complete
HH_STATUS_ORDER = [
    "Members missing, incomplete responses",
    "Members missing, otherwise complete",
    "All members participated, incomplete responses",
    "All members participated, complete",
]
HH_STATUS_COL = "household_day_members_completeness"
missing_flag = pl.col("members_missing_from_day_file")
complete_flag = pl.col("householdDay_complete")
household_day = household_day.with_columns(
    pl.when(missing_flag & complete_flag).then(pl.lit(HH_STATUS_ORDER[1]))
      .when(missing_flag).then(pl.lit(HH_STATUS_ORDER[0]))
      .when(~complete_flag).then(pl.lit(HH_STATUS_ORDER[2]))
      .otherwise(pl.lit(HH_STATUS_ORDER[3]))
      .alias(HH_STATUS_COL)
)
household_day_status_counts = (
    pl.DataFrame({HH_STATUS_COL: HH_STATUS_ORDER})
      .join(household_day.group_by(HH_STATUS_COL).agg(pl.len().alias("household_days")),
            on=HH_STATUS_COL, how="left")
      .with_columns(pl.col("household_days").fill_null(0))
      .with_columns((pl.col("household_days") / pl.col("household_days").sum() * 100)
                    .round(1).alias("pct_of_household_days"))
)
print("Household-days by members missing and completeness of responses")
print(household_day_status_counts)
print()

# ---------- Overall completeness summaries ----------
status_overall = (
    person_day.group_by("person_day_completeness")
       .agg(pl.len().alias("person_day_records"))
       .with_columns((pl.col("person_day_records") / pl.col("person_day_records").sum() * 100)
                     .round(1).alias("pct_of_person_days"))
       .sort("person_day_completeness")
)
print("Person-days by completeness status")
print(status_overall)
print("Note: person-days with zero trips and no reason given are excluded from the "
      "person-day file.\n")

# ---------- Which fields contribute to incompleteness ----------
# For each level of the completeness chain, count how many incomplete records have a
# problem (missing or out-of-range value) in each required field.
#   n_incomplete_with_field_problem: incomplete records where this field is a problem.
#       Records can have several problem fields, so these percentages can add to > 100.
#   n_only_problem: incomplete records where this is the ONLY problem field, so
#       fixing that field alone would make the record complete.


def bad_range(col, lo, hi):
    """True when the value is missing or outside lo..hi."""
    return ~pl.col(col).is_between(lo, hi).fill_null(False)


def bad_not_in(col, valid):
    """True when the value is missing or not one of the valid codes."""
    return ~pl.col(col).is_in(valid).fill_null(False)


def is_blank(col):
    return pl.col(col).is_null()


def blocker_summary(df, level, sides):
    """sides: list of {field: problem expression}. A record is incomplete when EVERY
    side has at least one problem (one side for trips, households and telecommute;
    two for persons, because either the employment side OR the student side can
    make a person complete)."""
    n_cols = [f"_n_side{i}" for i in range(len(sides))]
    d = df.with_columns([
        pl.sum_horizontal([e.fill_null(False).cast(pl.Int8) for e in side.values()]).alias(n_cols[i])
        for i, side in enumerate(sides)
    ])
    d = d.filter(pl.all_horizontal([pl.col(c) > 0 for c in n_cols]))

    n_total = df.height
    n_inc = d.height
    rows = []
    for i, side in enumerate(sides):
        for field, expr in side.items():
            problem = expr.fill_null(False)
            n_blocking = d.filter(problem).height
            n_only = d.filter(problem & (pl.col(n_cols[i]) == 1)).height
            rows.append({
                "level": level,
                "field": field,
                "records": n_total,
                "incomplete_records": n_inc,
                "n_incomplete_with_field_problem": n_blocking,
                "pct_of_incomplete": round(n_blocking / n_inc * 100, 1) if n_inc else None,
                "n_only_problem": n_only,
                "pct_only_problem": round(n_only / n_inc * 100, 1) if n_inc else None,
            })
    return pl.DataFrame(rows, infer_schema_length=None).sort(
        "n_incomplete_with_field_problem", descending=True)


def show_and_check(level, table, flag_incomplete):
    n = table["incomplete_records"][0]
    status = "OK" if n == flag_incomplete else "MISMATCH"
    print(f"{level}")
    print(f"  incomplete records from field checks = {n:,}; from completeness flag = {flag_incomplete:,} -> {status}")
    print(table.drop("level"), "\n")


# Trips: trip_complete requires every one of these fields
trip_fields = {
    "o_purpose_category": bad_range("o_purpose_category", 1, 13),
    "d_purpose_category": bad_range("d_purpose_category", 1, 13),
    "mode_type": bad_range("mode_type", 1, 14),
    **{c: is_blank(c) for c in trip_present_cols},
}
trip_blockers = blocker_summary(trip, "Trips", [trip_fields])

# Persons: complete if the employment side OR the student side is satisfied
emp_employed = pl.col("employment").is_in([1, 2, 3, 7]).fill_null(False)
needs_telework = emp_employed & pl.col("job_type").is_in([1, 2, 4, 5]).fill_null(False)
needs_work_loc = emp_employed & pl.col("job_type").is_in([1, 5]).fill_null(False)
person_employment_side = {
    "employment": bad_not_in("employment", [1, 2, 3, 5, 6, 7, 8]),
    "job_type": emp_employed & bad_not_in("job_type", [1, 2, 3, 4, 5]),
    "telework_freq": needs_telework & bad_not_in("telework_freq", list(range(1, 9)) + [996]),
    "work_lat": needs_work_loc & is_blank("work_lat"),
    "work_lon": needs_work_loc & is_blank("work_lon"),
    "work_park": needs_work_loc & bad_range("work_park", 1, 4),
}
student_in_school = pl.col("student").is_in([0, 1]).fill_null(False)
person_student_side = {
    "student": bad_not_in("student", [0, 1, 2, 3, 4]),
    "school_lat": student_in_school & is_blank("school_lat"),
    "school_lon": student_in_school & is_blank("school_lon"),
}
person_blockers = blocker_summary(person, "Persons",
                                  [person_employment_side, person_student_side])

# Households
hh_fields = {
    "num_vehicles": bad_range("num_vehicles", 0, 8),
    "num_adults": bad_range("num_adults", 1, 13),
    "num_kids": bad_range("num_kids", 0, 12),
    "num_people": bad_range("num_people", 1, 13),
    "num_workers": bad_range("num_workers", 0, 13),
    "home_lat": is_blank("home_lat"),
    "home_lon": is_blank("home_lon"),
}
hh_blockers = blocker_summary(hh, "Households", [hh_fields])

# Telecommute time (day level): only required of employed persons (1, 2, 3, 7)
telecommute_fields = {
    "telecommute_time": is_blank("telecommute_time")
                        & pl.col("employment").is_in([1, 2, 3, 7]).fill_null(False),
}
# (person-days that matched a day-file row, so the telecommute flag exists)
person_day_tc = person_day.filter(pl.col("telecommute_time_complete").is_not_null())
telecommute_blockers = blocker_summary(person_day_tc, "Person-days: telecommute time",
                                       [telecommute_fields])

# Person-days with trips: which component of the chain makes the day incomplete
days_with_trips = person_day.filter(pl.col("n_trips_in_trip_file") > 0)
day_components = {
    "trip details (purpose, mode, times, locations)": pl.col("n_trip_details_incomplete") > 0,
    "person profile (employment, job, school fields)": pl.col("person_complete").fill_null(0) == 0,
    "telecommute time": pl.col("telecommute_time_complete").fill_null(0) == 0,
    "household profile": pl.col("hh_complete").fill_null(0) == 0,
}
day_blockers = blocker_summary(days_with_trips, "Person-days with trips: component", [day_components])

print("Fields contributing to incompleteness (counts are among incomplete records)\n")
show_and_check("Trips", trip_blockers, (~trip["trip_complete"]).sum())
show_and_check("Persons", person_blockers, (person["person_complete"] == 0).sum())
show_and_check("Households", hh_blockers, (hh["hh_complete"] == 0).sum())
show_and_check("Person-days: telecommute time", telecommute_blockers,
               (person_day_tc["telecommute_time_complete"] == 0).sum())
show_and_check("Person-days with trips: which component makes the day incomplete", day_blockers,
               person_day.filter(pl.col("person_day_completeness") == STATUS_TRIPS_INCOMPLETE).height)

field_contribution = pl.concat([trip_blockers, person_blockers, hh_blockers,
                                telecommute_blockers, day_blockers])

# ---------- Save (nothing is written to BASE) ----------
person_day.write_csv(RUN_DIR / "person_day_completeness.csv")
household_day.write_csv(RUN_DIR / "household_day_completeness.csv")
household_day_status_counts.write_csv(RUN_DIR / "household_days_by_members_and_completeness.csv")
household.write_csv(RUN_DIR / "household_completeness.csv")
household_status_counts.write_csv(RUN_DIR / "households_by_members_and_completeness.csv")
status_overall.write_csv(RUN_DIR / "person_days_by_completeness_status.csv")
field_contribution.write_csv(RUN_DIR / "field_contribution_to_incompleteness.csv")
print(f"\nSaved outputs to: {RUN_DIR}")

sys.stdout = sys.__stdout__
_log_file.close()