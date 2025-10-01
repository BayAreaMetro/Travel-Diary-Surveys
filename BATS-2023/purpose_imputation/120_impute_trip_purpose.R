# Setup inputs and outputs =====================================================

step_number <- 120
step_name <- "Impute trip purposes"

source("scripts/initialize_step.R", local = TRUE)

if (ms_or_rm == "ms") {
  stop("Scripts from this point on should only be run on munged tables")
}

# List inputs
input_table_names <- c(
  "variable_list",
  "value_labels",
  "w_hh",
  "w_person",
  "w_day",
  "w_trip"
)

message("Inputs: \n    ", paste(input_table_names, collapse = "\n    "))


# List outputs
output_table_names <- c(
  "variable_list",
  "value_labels",
  "w_trip"
)

message("Outputs: \n    ", paste(output_table_names, collapse = "\n    "))


## get inputs------------------------------------------------------------------

message("Loading inputs")

for (input in input_table_names) {
  assign(x = input, value = get_table(input, settings = settings, step_number))
}

survey_values <- get("survey_values", settings)

# Do the work ==================================================================


## ----prepare_trip_data, echo = FALSE, include = FALSE-----------------------------------------------------------------------------------------------------

# FIXME: Put these in settings
# settings in the location type algorithm
d_max_sm <- 100 # meters
d_max_lg <- 150 # meters
dwell_max <- 300 # minutes

loc_types <- c(
  "home" = 1L,
  "work" = 2L,
  "school" = 3L,
  "other" = 4L
)

trip0 <- w_trip[browser == 0] # Create a copy for imputation modifications (joined back to w_trip at the end.)

# Join hh, person, day tables to trip table

# get day_complete from day table
trip1 <- merge(
  unique(trip0),
  unique(w_day[browser == 0, .(hh_id, person_id, day_num, day_complete = is_complete)]),
  by = c("person_id", "day_num"),
  suffixes = c("", "_day"),
  all.x = TRUE
)

stopifnot(
  nrow(trip1) == nrow(trip0),
  trip1[day_num > 0 & is.na(day_complete), .N] == 0
)

# Get home loc from hh table
trip1[
  w_hh,
  `:=`(
    home_lon = i.home_lon,
    home_lat = i.home_lat
  ),
  on = "hh_id"
]

work_loc_cols <- names(w_person)[str_detect(names(w_person), "work") &
  str_detect(names(w_person), "lat|lon")]

# Get a few variables from the person table
w_person[, worker := 1 * employment %in% get("employment_employed", survey_values)]

keep_cols <- c(
  "person_id", "age", "worker", "student",
  work_loc_cols, "school_lon", "school_lat"
)

trip2 <- merge(
  trip1,
  w_person[
    ,
    ..keep_cols
  ],
  by = "person_id",
  all.x = TRUE
)

stopifnot(nrow(trip2) == nrow(trip2))


# Calculate trip distances to habitual locations -------------------------------

trip3 <- copy(trip2)

work_locs <- unique(str_remove_all(work_loc_cols, "_lon|_lat"))

get_min_work_distance <- function(row, work_locs1, prefix) {
  min_distance <- Inf
  for (loc1 in work_locs1) {
    lat_col <- paste0(loc1, "_lat")
    lon_col <- paste0(loc1, "_lon")
    source_lat <- row[[paste0(prefix, "_lat")]]
    source_lon <- row[[paste0(prefix, "_lon")]]
    distance <- get_distance_meters(
      cbind(as.numeric(row[[lon_col]]), as.numeric(row[[lat_col]])),
      cbind(as.numeric(source_lon), as.numeric(source_lat))
    ) %>% round(1)
    min_distance <- min(min_distance, distance, na.rm = TRUE)
  }
  return(min_distance)
}

trip3[, `:=`(
  o_distance_home = get_distance_meters(
    cbind(home_lon, home_lat),
    cbind(o_lon, o_lat)
  ) %>%
    round(1),
  o_distance_work = apply(.SD, 1, get_min_work_distance,
    work_locs1 = work_locs, prefix = "o"
  ),
  o_distance_school = get_distance_meters(
    cbind(school_lon, school_lat),
    cbind(o_lon, o_lat)
  ) %>%
    round(1),
  d_distance_home = get_distance_meters(
    cbind(home_lon, home_lat),
    cbind(d_lon, d_lat)
  ) %>%
    round(1),
  d_distance_work = apply(.SD, 1, get_min_work_distance,
    work_locs1 = work_locs, prefix = "d"
  ),
  d_distance_school = get_distance_meters(
    cbind(school_lon, school_lat),
    cbind(d_lon, d_lat)
  ) %>%
    round(1)
)]

trip3[, `:=`(
  o_distance_home = replace(o_distance_home, is.nan(o_distance_home), NA),
  o_distance_work = replace(o_distance_work, is.nan(o_distance_work), NA),
  o_distance_school = replace(o_distance_school, is.nan(o_distance_school), NA),
  d_distance_home = replace(d_distance_home, is.nan(d_distance_home), NA),
  d_distance_work = replace(d_distance_work, is.nan(d_distance_work), NA),
  d_distance_school = replace(d_distance_school, is.nan(d_distance_school), NA)
)]


# Assign habitual locations to trips ------------------------------------------

trip4 <- copy(trip3)
trip4[, d_location_type := loc_types["other"]]

trip4[d_distance_home <= d_max_sm, d_location_type := loc_types["home"]]

trip4[
  days_last_trip == 1 & d_distance_home <= d_max_lg & dwell_mins >= dwell_max,
  d_location_type := loc_types["home"]
]

trip4[
  days_first_trip == 1 & d_distance_home <= d_max_lg & dwell_mins >= dwell_max,
  d_location_type := loc_types["home"]
]

trip4[worker == 1 & d_distance_work <= d_max_sm, d_location_type := loc_types["work"]]

trip4[
  (student %in% get("student_in_person", survey_values) |
    age %in% get("ages_student", survey_values)) &
    d_distance_school <= d_max_sm,
  d_location_type := loc_types["school"]
]

trip4[, o_location_type := loc_types["other"]]

trip4[o_distance_home <= d_max_sm, o_location_type := loc_types["home"]]

trip4[
  days_last_trip == 1 & o_distance_home <= d_max_lg & dwell_mins >= dwell_max,
  o_location_type := loc_types["home"]
]

trip4[
  days_first_trip == 1 & o_distance_home <= d_max_lg & dwell_mins >= dwell_max,
  o_location_type := loc_types["home"]
]

trip4[
  is.na(dwell_mins) & o_distance_home <= d_max_lg,
  o_location_type := loc_types["home"]
]

trip4[worker == 1 & o_distance_work <= d_max_sm, o_location_type := loc_types["work"]]

trip4[
  (student %in% get("student_in_person", survey_values) |
    age %in% get("ages_student", survey_values)) &
    o_distance_school <= d_max_sm,
  o_location_type := loc_types["school"]
]


# Reclass purpose categories ---------------------------------------------------

# NB: Make sure imputation type is set to "2" for these after imputation step

# Save old categories for checking
trip5 <- copy(trip4)

trip5[, d_purpose_category_old := d_purpose_category]
trip5[, o_purpose_category_old := o_purpose_category]

# Create a lagged d_distance_work and school because if we use
# _distance_work/school we end up with mismatches between o_purpose_category
# and lag(d_purpose_category) because origin locations do not always match
# previous destination locations.

setorder(trip5, person_id, depart_time)
trip5[, d_distance_work_lag := get_prev(d_distance_work), by = "person_id"]
trip5[, d_distance_school_lag := get_prev(d_distance_school), by = "person_id"]


# Reclass work and work-related depending on distance
purpose_work <- purpose_category_tbl[
  purpose_category_label == "work", purpose
]

purpose_category_work <- purpose_category_tbl[
  purpose_category_label == "work", unique(purpose_category)
]

purpose_work_related <- purpose_category_tbl[
  purpose_category_label == "work_related", purpose
]

purpose_category_work_related <- purpose_category_tbl[
  purpose_category_label == "work_related", unique(purpose_category)
]


trip5[
  (o_purpose == purpose_work & d_distance_work_lag <= d_max_lg),
  o_purpose_category := purpose_category_work
]

trip5[
  o_purpose == purpose_work &
    d_distance_work_lag > d_max_lg,
  `:=`(
    o_purpose_category = purpose_category_work_related,
    o_purpose = purpose_work_related[1]
  )
]

trip5[
  d_purpose == purpose_work &
    d_distance_work <= d_max_lg,
  d_purpose_category := purpose_category_work
]

trip5[
  d_purpose == purpose_work &
    d_distance_work > d_max_lg,
  `:=`(
    d_purpose_category = purpose_category_work_related,
    d_purpose = purpose_work_related[1]
  )
]


# Re-class school and school-related depending on distance

purpose_school <- purpose_category_tbl[
  purpose_category_label == "school", purpose
]

purpose_category_school <- purpose_category_tbl[
  purpose_category_label == "school", unique(purpose_category)
]

purpose_school_related <- purpose_category_tbl[
  purpose_category_label == "school_related", purpose
]

purpose_category_school_related <- purpose_category_tbl[
  purpose_category_label == "school_related", unique(purpose_category)
]

trip5[
  o_purpose %in% purpose_school &
    d_distance_school_lag > d_max_lg,
  `:=`(
    o_purpose_category = purpose_category_school_related,
    o_purpose = purpose_school_related[1]
  )
]

trip5[
  d_purpose %in% purpose_school &
    d_distance_school > d_max_lg,
  `:=`(
    d_purpose_category = purpose_category_school_related,
    d_purpose = purpose_school_related[1]
  )
]

trip5[
  o_purpose == purpose_school_related[1],
  o_purpose_category := purpose_category_school_related
]

trip5[
  d_purpose == purpose_school_related[1],
  d_purpose_category := purpose_category_school_related
]

# Clean up
trip5[, o_purpose_category_old := NULL]
trip5[, d_purpose_category_old := NULL]
trip5[, `:=`(d_distance_work_lag = NULL, d_distance_school_lag = NULL)]


# Check that we don't have purposes in more than one category and
# all purposes are in the proper categories
# exclude purpose_other_gpt recodes
purpose_other_code <- get("purpose_other", survey_values)

stopifnot(
  "Origin purposes are in multiple categories" = trip5[o_purpose != purpose_other_code, .N, .(o_purpose, o_purpose_category)][, .N, o_purpose][N > 1, .N] == 0,
  "Destination purposes are in multiple categories" = trip5[d_purpose != purpose_other_code, .N, .(d_purpose, d_purpose_category)][, .N, d_purpose][N > 1, .N] == 0,
  trip5[
    o_purpose_category == purpose_category_work &
      !(o_purpose %in% purpose_work) &
      o_purpose != purpose_other_code,
    .N
  ] == 0,
  trip5[
    o_purpose_category == purpose_category_work_related &
      !(o_purpose %in% purpose_work_related) &
      o_purpose != purpose_other_code,
    .N
  ] == 0,
  trip5[
    d_purpose_category == purpose_category_work &
      !(d_purpose %in% purpose_work) &
      d_purpose != purpose_other_code,
    .N
  ] == 0,
  trip5[
    d_purpose_category == purpose_category_work_related &
      !(d_purpose %in% purpose_work_related) &
      d_purpose != purpose_other_code,
    .N
  ] == 0,
  trip5[
    o_purpose_category == purpose_category_school &
      !(o_purpose %in% purpose_school) &
      o_purpose != purpose_other_code,
    .N
  ] == 0,
  trip5[
    o_purpose_category == purpose_category_school_related &
      !(o_purpose %in% purpose_school_related) &
      o_purpose != purpose_other_code,
    .N
  ] == 0,
  trip5[
    d_purpose_category == purpose_category_school &
      !(d_purpose %in% purpose_school) &
      d_purpose != purpose_other_code,
    .N
  ] == 0,
  trip5[
    d_purpose_category == purpose_category_school_related &
      !(d_purpose %in% purpose_school_related) &
      d_purpose != purpose_other_code,
    .N
  ] == 0
)

## ----impute_trip_purpose, echo = FALSE, include = FALSE---------------------------------------------------------------------------------------------------

# Get purpose category inputs
p_tbl <- unique(purpose_category_tbl[
  !is.na(purpose_category), .(purpose_category, purpose_category_label)
])

purpose_categories <- p_tbl[, purpose_category]
names(purpose_categories) <- p_tbl[, purpose_category_label]
purpose_categories <- as.list(purpose_categories)

m_tbl <- unique(mode_type_tbl[!is.na(mode_type), .(mode_type, mode_type_label)])
mode_types <- m_tbl[, mode_type]
names(mode_types) <- m_tbl[, mode_type_label]
mode_types <- as.list(mode_types)

# FIXME: Resolve coercion warnings!
trip_impute <- impute_trip_purposes(
  trip5,
  settings = settings,
  survey_values = get("survey_values", settings),
  purpose_categories = purpose_categories,
  mode_categories = mode_types,
  niterations = 5
)

trip_impute_copy <- copy(trip_impute) # Save for debugging later


# Check for problems
stopifnot(
  all(trip_impute[mmtype > 0, unique(d_pimptype)] == 19),
  all(trip_impute[mmorig == 0, unique(d_pimptype)] %in% c(1, 13, 14, 50)),
  trip_impute[d_pimputed == 1 & d_purpose != 1 & d_distance_home > d_max_lg, .N] == 0
)


## ----construct_final, echo=FALSE, include=FALSE-----------------------------------------------------------------------------------------------------------
# # Examine outputs for problems

# Make sure all of the trip_ids made it through the imputation process
stopifnot(
  length(setdiff(trip5[, trip_id], trip_impute[, trip_id])) == 0,
  length(setdiff(w_trip[browser == 0, trip_id], trip_impute[, trip_id])) == 0,
  length(setdiff(trip_impute[, trip_id], trip5[, trip_id])) == 0,
  "trip_id is not unique" = trip_impute[, .N, trip_id][N > 1, .N] == 0
)

# Drop all of the old columns that are repeated in w_trip (except for the trip_id)
# setdiff(names(trip_impute), names(w_trip))

keep_cols <- c(
  "trip_id",
  "o_pimputed", "d_pimputed",
  "o_pimputed_cat", "d_pimputed_cat",
  "o_pimptype", "d_pimptype",
  "o_ltimputed", "d_ltimputed",
  "o_location_type",
  "d_location_type",
  "mmtype", "mmorig"
)

trip_impute <- trip_impute[, keep_cols, with = FALSE]

# Join to w_trip
w_trip_copy <- copy(w_trip)
w_trip <- merge(
  w_trip,
  trip_impute,
  by = "trip_id",
  all = TRUE
)

stopifnot(w_trip[, .N] == w_trip_copy[, .N])

# Rename columns

# setdiff(names(w_trip), names(w_trip_copy))

purpose_cols <- c(
  "o_purpose", "d_purpose",
  "o_purpose_category", "d_purpose_category",
  "o_location_type", "d_location_type"
)
setnames(
  w_trip,
  old = purpose_cols,
  new = paste0(purpose_cols, "_reported")
)

setnames(
  w_trip,
  old = c(
    "o_pimputed", "d_pimputed", "o_pimputed_cat", "d_pimputed_cat",
    "o_ltimputed", "d_ltimputed",
    "o_pimptype", "d_pimptype"
  ),
  new = c(
    purpose_cols,
    "o_purpose_impute_type", "d_purpose_impute_type"
  )
)


# Check for purposes that were changed in pre-processing

# Update d_purpose_impute_type for trips that were modified in pre-processing
# NB: imputation_type = 2 is for d_pimputed that are changed in preprocessing due to
# mismatches between purpose and location type.
# NOTE: origin purposes are set based on lagged d_purpose and don't need the same fix

# w_trip[
#   d_purpose_impute_type == 1 & (d_purpose_reported != d_purpose),
#   .(trip_id, d_purpose_reported, d_purpose)]

w_trip[
  d_purpose_impute_type == 1 & (d_purpose_reported != d_purpose),
  d_purpose_impute_type := 2
]


# Check for any trips that were added back that did not go through the imputation
# these are browswer move trips

# w_trip[is.na(d_purpose), .N, browser]
# w_trip[is.na(o_purpose), .N, browser]

w_trip[
  is.na(d_purpose),
  `:=`(
    d_purpose = d_purpose_reported,
    d_purpose_category = d_purpose_category_reported,
    d_purpose_impute_type = -1
  )
]

w_trip[
  is.na(o_purpose),
  `:=`(
    o_purpose = o_purpose_reported,
    o_purpose_category = o_purpose_category_reported,
    o_purpose_impute_type = -1
  )
]

keep_cols_after_renaming <- c(
  "o_purpose", "d_purpose",
  "o_purpose_category", "d_purpose_category",
  "o_purpose_impute_type", "d_purpose_impute_type",
  "o_location_type", "d_location_type",
  "o_purpose_reported", "o_purpose_category_reported", "o_location_type_reported",
  "d_purpose_reported", "d_purpose_category_reported", "d_location_type_reported",
  "mmtype", "mmorig"
)

stopifnot(
  (nrow(w_trip_copy) - nrow(w_trip)) == 0,
  setdiff(names(w_trip_copy), names(w_trip)) %>%
    length() == 0,
  setdiff(names(w_trip), c(names(w_trip_copy), keep_cols_after_renaming)) %>%
    length() == 0
)


#'
## ---------------------------------------------------------------------------------------------------------------------------------------------------------

if (FALSE) {
  # Some debugging code

  trip_impute_copy[d_purpose != d_pimputed, .N]
  w_trip[d_purpose_reported != d_purpose, .N]

  trip_impute_copy[o_purpose != o_pimputed, .N]
  w_trip[o_purpose_reported != o_purpose, .N]

  trip_impute_copy[d_purpose != d_pimputed, .N, keyby = d_pimptype]
  w_trip[d_purpose_reported != d_purpose, .N, keyby = d_purpose_impute_type]

  # Note that codes for o_purpose_impute_type are not the same as d_purpose_impute_type
  trip_impute_copy[o_purpose != o_pimputed, .N, keyby = o_pimptype]
  w_trip[o_purpose_reported != o_purpose, .N, keyby = o_purpose_impute_type]

  trip_impute_copy[d_purpose != d_pimputed, .N, keyby = .(d_purpose, d_pimputed)]
  w_trip[d_purpose_reported != d_purpose, .N, keyby = .(d_purpose_reported, d_purpose)]


  setdiff(names(trip5), names(w_trip))
  setdiff(names(w_trip), names(trip5))

  trip5[, .N]
  trip_impute[, .N]
  w_trip[browser == 0, .N]
  w_trip[, .N]
}


# Check that we don't have purposes in more than one category

stopifnot(
  "Impute type of one has different imputed vs reported purposes" = w_trip[
    d_purpose_impute_type == 1 & (d_purpose_reported != d_purpose), .N
  ] == 0,
  "d_purpose falls into > 1 purpose category" =
    w_trip[d_purpose != purpose_other_code, .N, .(d_purpose, d_purpose_category)][, .N, d_purpose][N > 1, .N] == 0,
  "d_purpose_reported falls into > 1 purpose_category" =
    w_trip[d_purpose_reported != purpose_other_code, .N, .(d_purpose_reported, d_purpose_category_reported)][, .N, d_purpose_reported][N > 1, .N] == 0
)

if (FALSE) {
  w_trip_copy[, .N, .(d_purpose, d_purpose_category)][, .N, d_purpose][N > 1]

  trip_impute[, .N, .(d_pimputed, d_pimputed_cat)][, .N, d_pimputed][N > 1]
  trip_impute[d_pimputed == 44, .(d_pimputed, d_pimputed_cat, d_pimptype, mmorig)]

  w_trip[, .N, .(d_purpose, d_purpose_category)][, .N, d_purpose][N > 1]
  w_trip[d_purpose == 44, .N, d_purpose_category]
  purpose_categories
  purpose_category_tbl
}


for (cat in names(purpose_categories)) {
  catcode <- purpose_categories[[cat]]

  pcodes <- purpose_category_tbl[purpose_category_label == cat, purpose]

  if (length(pcodes) == 0) next()

  message(catcode, " ", cat, ": ", paste(pcodes, collapse = ", "), "\n")

  stopifnot(
    w_trip[
      d_purpose_category == catcode &
        !(d_purpose %in% pcodes) &
        d_purpose != purpose_other_code, # exclude purpose_other_gpt recodes
      .N
    ] == 0
  )
}

if (FALSE) {
  w_trip[
    d_purpose_category == catcode & !(d_purpose %in% pcodes),
    .(trip_id, d_purpose_category, d_purpose)
  ]
}



# Reporting ====================================================================

totals <- lapply(output_table_names, function(x) nrow(get(x)))

totals <- data.table(output_table_names, totals)

names(totals) <- c("Working table", "Total records")

report_tables[[step_name]] <- totals

report <- add_to_report(report, totals)


# Store Outputs ================================================================

## Write the tables to rds files -------------------------------------------

store_output_tables(output_table_names, step_number, settings$output_dir)
write_report(report, settings$dbname, settings$output_dir)
write_report_tables(report_tables, settings$dbname, settings$output_dir)
