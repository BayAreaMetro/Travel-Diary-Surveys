## Impute trip purpose function
# -------------------------------------


impute_trip_purposes <- function(
    trip,
    settings,
    survey_values,
    purpose_categories,
    mode_categories,
    niterations = 5) {
  # browser()
  setDT(trip)
  trip <- copy(trip)

  # Setup ---------------------------------------------------------------------

  # FIXME: Replace with get_distance_meters
  degtorad <- function(deg) {
    deg * pi / 180
  }

  gcdist <- function(th1, ph1, th2, ph2) {
    diameter <- 2 * 6372800.0

    ph1 <- degtorad(ph1 - ph2)
    th1 <- degtorad(th1)
    th2 <- degtorad(th2)

    dz <- sin(th1) - sin(th2)
    dx <- cos(ph1) * cos(th1) - cos(th2)
    dy <- sin(ph1) * cos(th1)
    gcdist <- asin(sqrt(dx^2 + dy^2 + dz^2) / 2) * diameter

    return(gcdist)
  }

  # Define some functions for readability
  lag <- get_prev
  lead <- get_next
  case_when <- dplyr::case_when

  # Declare some constants ------------------------------------------------------

  v_home <- 1
  v_work <- 2
  v_school <- 3
  v_other <- 4
  v_missing <- 5

  # purpose home, work, school, other codes
  purpimp <- c(
    home = get("purpose_home", survey_values),
    work = get("purpose_work", survey_values),
    school = get("purpose_school", survey_values),
    other = get("purpose_other", survey_values)
  )

  # purpose_category home, work, school, other codes
  pcatimp <- c(
    home = get("home", purpose_categories),
    work = get("work", purpose_categories),
    school = get("school", purpose_categories),
    other = get("other", purpose_categories)
  )

  # Define function for assigning purpose
  get_purpose_code <- function(
      age,
      survey_values,
      v_check,
      v_school,
      purpimp) {
    purpose_code <- case_when(
        v_check != v_school ~ purpimp[v_check],
        age >= get("age_adult", survey_values) ~ get("purpose_school", survey_values),
        age == get("age_under_5", survey_values) ~ get("purpose_preschool", survey_values),
        TRUE ~ get("purpose_k12", survey_values)
      )

    return(purpose_code)
  }

  # initialize new variables to -1
  trip[
    ,
    `:=`(
      d_pimptype = -1L,
      d_pimputed = -1L,
      d_pimputed_cat = -1L,
      d_ltimputed = -1L,
      o_pimptype = -1L,
      o_pimputed = -1L,
      o_pimputed_cat = -1L,
      o_ltimputed = -1L
    )
  ]

  # Initialize indices to 0
  trip[, `:=`(pd_ftrip = 0, pd_ltrip = 0, pd_ctrips = 0, pd_itrips = 0)]

  trip[, o_pcat2 := o_purpose_category]
  trip[, d_pcat2 := d_purpose_category]

  # Reclassify non-response as 'other' so it will get imputed
  trip[
    is.na(o_purpose_category),
    o_pcat2 := get("other", purpose_categories)
  ] # Missing, no response

  trip[
    is.na(d_purpose_category),
    d_pcat2 := get("other", purpose_categories)
  ] # Missing, no response

  # Define a broader, 5-category purpose category variable

  # this uses the same 4 categories as location_type plus 5 for missing
  trip[, d_pcat3 := v_other]

  trip[d_pcat2 == get("home", purpose_categories), d_pcat3 := v_home]
  trip[d_pcat2 == get("work", purpose_categories), d_pcat3 := v_work]
  trip[d_pcat2 == get("school", purpose_categories), d_pcat3 := v_school]
  trip[d_pcat2 == get("other", purpose_categories), d_pcat3 := v_missing]

  # Create random numbers
  set.seed("2004")
  trip[, rand0to1 := runif(n = .N)]

  # Calculate first and last trips
  # Basically just indicates whether each trip is the first/last trip of the person-day
  # pd_ftrip and pd_ltrip are the first and last trips for each person-day

  setorder(trip, person_id, depart_time)

  trip[, row_num := 1:.N]

  trip[,
    `:=`(
      is_p_ftrip = as.integer(trip_num == min(trip_num)),
      is_p_ltrip = as.integer(trip_num == max(trip_num))
    ),
    by = .(person_id)
  ]

  trip[,
    `:=`(
      is_pd_ftrip = as.integer(trip_num == min(trip_num)),
      is_pd_ltrip = as.integer(trip_num == max(trip_num))
    ),
    by = .(person_id, day_num)
  ]

  trip[,
    `:=`(
      pd_ftrip = as.numeric(cumsum(row_num * is_pd_ftrip)),
      pd_ltrip = as.numeric(cumsum(row_num * is_pd_ltrip))
    ),
    by = .(person_id, day_num)
  ]

  # Count complete and incomplete trips for each person day
  trip[,
    `:=`(
      pd_ctrips = as.numeric(sum(trip_survey_complete)),
      pd_itrips = as.numeric(sum(1L - trip_survey_complete))
    ),
    by = .(person_id, day_num)
  ]

  # Create lead and lag versions of purpose with all trips and by person_id
  trip[,
    `:=`(
      d_purpose_lag = lag(d_purpose),
      d_purpose_lead = lead(d_purpose),
      d_pcat2_lag = lag(d_pcat2),
      d_pcat2_lead = lead(d_pcat2)
    ),
    by = "person_id"
  ]

  # Type 0 -----------------------------------------------------------------------
  # The destination location type matches the stated destination purpose
  # This is the majority of cases, The imputed purpose is set to be the same as
  # the stated purpose, as there is no evidence otherwise.
  # (Note: Specific types of corrections that shift destination purposes across
  # trips in a day (such as Test 7 for H/O trips below) may overwrite the imputed
  # purpose for these trips, even when there is no mismatch.)

  # Determine which days are valid
  trip[, pd_valid := 1]

  trip[
    pd_ftrip == 0 | pd_ctrips < settings[["min_complete_trips"]] |
      pd_itrips > settings[["max_incomplete_trips"]],
    pd_valid := 0,
    by = .(person_id, day_num)
  ]

  # Set initial imputed destination purpose -1 invalid day, 0 not set, otherwise use purpose

  # Set mismatch type for trip
  trip[
    ,
    mmtype := case_when(
      pd_ftrip > 0 & pd_valid == 0 ~ -1,
      pd_ftrip > 0 & d_pcat3 == d_location_type ~ 0, # No mismatch
      pd_ftrip > 0 & d_pcat3 == v_missing & d_location_type == v_other ~ 7, # O/M - this has been changed
      pd_ftrip > 0 & d_pcat3 != v_home & d_location_type == v_home ~ 1, # H/O
      pd_ftrip > 0 & d_pcat3 == v_home & d_location_type != v_home ~ 2, # O/H
      pd_ftrip > 0 & d_pcat3 != v_work & d_location_type == v_work ~ 3, # W/O
      pd_ftrip > 0 & d_pcat3 == v_work & d_location_type != v_work ~ 4, # O/W
      pd_ftrip > 0 & d_pcat3 != v_school & d_location_type == v_school ~ 5,
      pd_ftrip > 0 & d_pcat3 == v_school & d_location_type != v_school ~ 6,
      TRUE ~ -1
    )
  ]

  trip[, mmorig := mmtype] # Save a copy to refer back to and change mmtype as it goes

  # Set initial purpose imputation type
  imputation_type <- 1

  trip[
    ,
    `:=`(
      d_pimptype = 0,
      d_pimputed = 0,
      d_pimputed_cat = get("other", purpose_categories), # Missing
      d_ltimputed = 0
    )
  ]

  trip[
    mmtype == -1,
    `:=`(
      d_pimptype = -1,
      d_pimputed = -1,
      d_pimputed_cat = -1,
      d_ltimputed = d_location_type
    )
  ]

  trip[
    mmtype == 0,
    `:=`(
      d_pimptype = imputation_type,
      d_pimputed = as.integer(d_purpose),
      d_pimputed_cat = d_pcat2,
      d_ltimputed = d_location_type
    )
  ]

  # NB: imputation_type = 2 is for d_pimputed that are changed in preprocessing due to
  # mismatches between purpose and location type.

  # FIXME:
  # Set imputation_type = 2 here instead of in 177_impute_trip_purpose.R

  # Run an outer iteration loop and make all tests separable so they can be started
  # in different iterations

  for (iteration in 1:niterations) {
    message("iteration ", iteration, " =========================================")

    # Loop on three location types: home, work, other (school?)
    for (v_check in v_home:v_school) {
      mmtest <- 2 * v_check - 1 # 1, 3, 5

      message("mmtest ", mmtest, " ---------------------------------------------")

      # Type 1, 3, 5: imputed destination location is home/work/school,
      # stated destination is not the same.  These are the so-called
      # H/O, W/O, S/O trips.  Process if not already imputed.


      # Test 1 -------------------------------------------------------------------
      # Test 1: If purpose is 'Change mode', check for reasonableness.  Although
      # the change mode location is within the radius to have an imputed location
      # of home, most such trips may be valid. A valid change mode stop would meet
      # one of the following two tests:
      # - The location type for the previous destination (or the origin if it is
      #   the first trip of the day) is Home, the mode is Walk or bike, and the
      #   mode for the next trip is Transit, or
      # - The location type for the next destination is Home, the mode is Transit,
      #   and the mode for the next trip is Walk.
      # If it passes either test, set the purpose imputation type to 11.
      # Otherwise continue

      imputation_type <- 3

      if (iteration >= 1) {
        trip[
          ,
          change_idx := (
            (pd_valid > 0 & mmtype == mmtest &
              d_pimptype == 0) &

              # loc type for prev dest is home and mode is walk or bike and next mode is transit
              ((o_location_type == v_check &
                d_pcat2 == get("change_mode", purpose_categories) &
                mode_type %in% c(mode_categories[["walk"]], mode_categories[["bike"]]) &
                lead(mode_type) == mode_categories[["transit"]]) |

                # loc type for next dest is home, mode is transit, and mode for nxt trip is walk or bike
                (is_pd_ltrip < 1 &
                  lead(d_location_type) == v_check &
                  d_pcat2 == get("change_mode", purpose_categories) &
                  mode_type == mode_categories[["transit"]] &
                  lead(mode_type) %in% c(mode_categories[["walk"]], mode_categories[["bike"]]))))
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = d_purpose,
            d_pimputed_cat = d_pcat2,
            d_ltimputed = v_other,
            mmtype = 0
          )
        ]

        trip[, change_idx := NULL]
      }

      # Test 2 ------------------------------------------------------------------
      # Test 2: If there is no location type/purpose mismatch for either the
      # preceding or the next trip, and neither the previous or the next trip is
      # at Home, then set the imputed purpose to 'Home' and stop, otherwise
      # continue. (Note: These are mainly cases of people reporting what they did at home
      # rather than 'Home' as a purpose.)

      # Test 2A
      imputation_type <- 4

      if (iteration >= 1) {
        # In first iteration, don't apply for shopping or errand purposes

        trip[
          ,
          change_idx := (
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0 &
              (iteration > 1 | (d_pcat2 != get("shop", purpose_categories) & d_pcat2 != get("errand", purpose_categories))) &
              is_pd_ftrip == 0 &
              lag(d_location_type) != v_check &
              lag(mmtype) == 0 &
              is_pd_ltrip == 0 &
              lead(d_location_type) != v_check &
              lead(mmtype) == 0)
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = get_purpose_code(age, survey_values, v_check, v_school, purpimp),
            d_pimputed_cat = pcatimp[v_check],
            d_ltimputed = v_check,
            d_pcat3 = v_check,
            mmtype = 0
          )
        ]

        trip[, change_idx := NULL]

        # Test 2B: variation if last trip of the day, only for home
        imputation_type <- 5

        if (v_check == v_home) {
          trip[
            ,
            change_idx := (
              pd_valid > 0 &
                mmtype == mmtest &
                d_pimptype == 0 &
                (iteration > 1 | (d_pcat2 != get("shop", purpose_categories) & d_pcat2 != get("errand", purpose_categories))) &
                is_pd_ftrip == 0 &
                lag(d_location_type) != v_check &
                lag(mmtype == 0) &
                is_pd_ltrip == 1)
          ]

          message(
            "iteration = ", iteration,
            " | mmtest = ", mmtest,
            " | imputation_type = ", imputation_type,
            " | no. of cases = ", trip[change_idx == TRUE, .N]
          )

          trip[
            change_idx == TRUE,
            `:=`(
              d_pimptype = imputation_type,
              d_pimputed = get_purpose_code(age, survey_values, v_check, v_school, purpimp),
              d_pimputed_cat = pcatimp[v_check],
              d_ltimputed = v_check,
              d_pcat3 = v_check,
              mmtype = 0
            )
          ]

          trip[, change_idx := NULL]
        }

        # Test 2C: variation of 2A that allows for missing purposes in adjacent trips
        imputation_type <- 6

        trip[
          ,
          change_idx := (
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0 &
              (iteration > 1 | (d_pcat2 != get("shop", purpose_categories) & d_pcat2 != get("errand", purpose_categories))) &
              is_pd_ftrip == 0 &
              lag(d_location_type) != v_check &
              (lag(mmtype) == 0 | lag(d_pcat3) == v_missing) &
              is_pd_ltrip == 0 &
              lead(d_location_type) != v_check &
              (lead(mmtype) == 0 | lead(d_pcat3) == v_missing))
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = get_purpose_code(age, survey_values, v_check, v_school, purpimp),
            d_pimputed_cat = pcatimp[v_check],
            d_ltimputed = v_check,
            d_pcat3 = v_check,
            mmtype = 0
          )
        ]

        trip[, change_idx := NULL]

        # Test 2D: variation of 2B that allows for missing purpose in prev trip
        # only for home
        imputation_type <- 7

        trip[
          ,
          change_idx := (
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0 &
              (iteration > 1 | (d_pcat2 != get("shop", purpose_categories) & d_pcat2 != get("errand", purpose_categories))) &
              is_pd_ftrip == 0 &
              lag(d_location_type) != v_home &
              (lag(mmtype) == 0 | lag(d_pcat3) == v_missing) &
              is_pd_ltrip == 1)
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        if (v_check == v_home) {
          trip[
            change_idx == TRUE,
            `:=`(
              d_pimptype = imputation_type,
              d_pimputed = get_purpose_code(age, survey_values, v_check, v_school, purpimp),
              d_pimputed_cat = pcatimp[v_check],
              d_ltimputed = v_check,
              d_pcat3 = v_check,
              mmtype = 0
            )
          ]
        }

        trip[, change_idx := NULL]
      }

      # Test 3 ----------------------------------------------------------------
      # Test 3:  If there is no location type/purpose mismatch for either the
      # preceding or the next trip, and either or both the previous or the next
      # trip is at Home, then compare the dwell time for the current trip to the
      # dwell time(s) for the adjacent Home stop(s). If the duration is shorter
      # than X minutes and the adjacent home duration is at least Y minutes and
      # the stated destination purpose is one that could conceivably be
      # performed very near the home location (e.g. shop, meal,
      # social/recreation, errand, escort, work-related), then set the imputed
      # purpose to the stated purpose and stop, otherwise continue. (Note:
      # Imputing a home purpose for these cases would create a Home-Home trip.
      # An analysis will be carried out to classify potential cases to set
      # appropriate values of X and Y durations and eligible trip purposes.

      if (iteration > 1) {
        XDur1 <- 30
        YDur1 <- 90
        XDur2 <- 60
        YDur2 <- 180

        # Test 3A: previous trip
        imputation_type <- 8

        trip[
          ,
          change_idx := (
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0 &
              d_pcat3 != v_missing & # Don't use for missing
              is_pd_ftrip == 0 &
              lag(mmtype) == 0 &
              lag(d_location_type) == v_check &
              lag(d_pcat3) == v_check &
              dwell_mins <= XDur2 &
              lag(dwell_mins) >= YDur1 &
              lag(dwell_mins) >= dwell_mins * 3)
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = d_purpose,
            d_pimputed_cat = d_pcat2,
            d_ltimputed = v_other,
            d_pcat3 = v_other,
            mmtype = 0
          )
        ]

        trip[, change_idx := NULL]

        # Test 3B: next trip
        imputation_type <- 9

        trip[
          ,
          change_idx := (
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0 &
              is_pd_ltrip == 0 &
              lead(mmtype) == 0 &
              lead(d_location_type) == v_check &
              lead(d_pcat3) == v_check &
              dwell_mins <= XDur2 &
              lead(dwell_mins) >= YDur1 &
              lead(dwell_mins) >= dwell_mins * 3)
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = d_purpose,
            d_pimputed_cat = d_pcat2,
            d_ltimputed = v_other,
            d_pcat3 = v_other,
            mmtype = 0
          )
        ]

        trip[, change_idx := NULL]
      }

      # Test 4 ---------------------------------------------------------------
      # Test 4:  If both the previous destination and the next destination
      # have the opposite mismatch ( stated purpose is Home but location type
      # is not Home), and there are no mismatches for the adjacent
      # destinations (the one preceding the previous destination and the one
      # after the next destination, then set the imputed purpose for the
      # previous destination and the next destination to the stated purpose
      # for H/O destination, set the imputed purpose for the H/O destination
      # to Home, and stop, otherwise continue.
      # (Note: This is the case where a respondent mixes up trip ends, so
      # reports a Home-Shop-Home actual tour as a Shop-Home-Shop tour. This
      # rule does not identify multiple such tours with no correct matches
      # in-between, so may need to be modified to do so.)

      imputation_type <- 10

      if (iteration >= 1) {
        trip[
          ,
          change_idx := (
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0 &
              d_pcat3 != v_missing & # Don't use for missing
              is_pd_ftrip == 0 &
              (lag(is_pd_ftrip) == 1 | lag(mmtype, 2) == 0) &
              lag(d_location_type) != v_check &
              lag(d_pcat3) == v_check &
              is_pd_ltrip == 0 &
              (lead(is_pd_ltrip) == 1 | lead(mmtype, 2) == 0) &
              lead(d_location_type) != v_check &
              lead(d_pcat3) == v_check)
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = d_purpose_lag,
            d_pimputed_cat = d_pcat2_lag,
            d_ltimputed = d_location_type,
            d_pcat3 = d_location_type,
            mmtype = 0
          )
        ]

        # Change previous trip
        trip[
          which(change_idx) - 1,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = d_purpose_lead,
            d_pimputed_cat = d_pcat2_lead,
            d_ltimputed = d_location_type,
            d_pcat3 = d_location_type,
            mmtype = 0
          )
        ]

        # Change next trip
        trip[
          which(change_idx) + 1,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = d_purpose_lag,
            d_pimputed_cat = d_pcat2_lag,
            d_ltimputed = d_location_type,
            d_pcat3 = d_location_type,
            mmtype = 0
          )
        ]

        trip[, change_idx := NULL]
      }

      # Test 5 -------------------------------------------------------------------
      # Test 5:  If the previous destination has the opposite mismatch (stated
      # purpose is Home but location type is not Home), and there are no
      # mismatches for the adjacent destinations (the one preceding the previous
      # destination and the next destination, then set the imputed purpose for the
      # previous destination to the stated purpose for H/O destination, set the
      # imputed purpose for the H/O destination to Home, and stop, otherwise
      # continue.  (Note: This is a simpler version of Test 4, where only the
      # preceding trip is "flipped", not both adjacent destinations. This rule may
      # need to be modified to identify two such pairs in succession.)

      imputation_type <- 11

      if (iteration >= 1) {
        trip[
          ,
          change_idx := (
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0 &
              d_pcat3 != v_missing & # Don't use for missing
              is_pd_ftrip == 0 &
              (lag(is_pd_ftrip) == 1 | lag(mmtype, 2) == 0) &
              lag(d_location_type) != v_check &
              lag(d_pcat3) == v_check &
              (is_pd_ltrip == 1 | lead(mmtype) == 0))
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = d_purpose_lag,
            d_pimputed_cat = d_pcat2_lag,
            d_ltimputed = d_location_type,
            d_pcat3 = d_location_type,
            mmtype = 0
          )
        ]

        # Previous trip
        trip[
          which(change_idx) - 1,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = d_purpose_lead,
            d_pimputed_cat = d_pcat2_lead,
            d_ltimputed = d_location_type,
            d_pcat3 = d_location_type,
            mmtype = 0
          )
        ]

        trip[, change_idx := NULL]
      }

      # Test 6 -------------------------------------------------------------------
      # Test 6:  If the next destination has the opposite mismatch (stated purpose
      # is Home but location type is not Home), and there are no mismatches for the
      # adjacent destinations (the previous destination and the one after the next
      # destination, then set the imputed purpose for the next destination to the
      # stated purpose for H/O destination, set the imputed purpose for the H/O
      # destination to Home, and stop, otherwise continue.  (Note: This is the same
      # as Test 5, but in the opposite order.)}

      imputation_type <- 12

      if (iteration >= 1) {
        trip[
          ,
          change_idx := (
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0 &
              d_pcat3 != v_missing & # Don't use for missing
              (is_pd_ftrip == 1 | lag(mmtype) == 0) &
              is_pd_ltrip == 0 &
              (lead(is_pd_ltrip) == 1 | lead(mmtype, 2) == 0) &
              lead(d_location_type) != v_check &
              lead(d_pcat3) == v_check &
              d_distance_home <= d_max_lg)
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = d_purpose_lead,
            d_pimputed_cat = d_pcat2_lead,
            d_ltimputed = d_location_type,
            d_pcat3 = d_location_type,
            mmtype = 0
          )
        ]

        # Next trip
        trip[
          which(change_idx) + 1,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = d_purpose_lag,
            d_pimputed_cat = d_pcat2_lag,
            d_ltimputed = d_location_type,
            d_pcat3 = d_location_type,
            mmtype = 0
          )
        ]

        trip[, change_idx := NULL]
      }

      # Test 7 -------------------------------------------------------------------
      # Test 7: Starting with the current x/O trip, try shifting the destination
      # purposes for all subsequent trips to the next trip.  If this does not
      # cause any mismatches (a 'Missing' purpose cannot be mismatched), then set
      # the imputed purpose for the H/O destination to Home and the imputed
      # purpose for each subsequent destination to the stated purpose for the
      # preceding destination, and stop, otherwise.

      imputation_type <- 13

      if (iteration >= 1) {
        # Within a person day
        # Look for mismatch between d_location_type and previous d_pcat3
        # If there are no mismatches when shifting, then we should shift them
        # permanently.
        # assign d_pimputed to purpimp[v_check] etc
        # At subsequent rows, assign d_pimputed to lag(d_purpose)

        # NOT USED: Start of a non-functionized version
        # test[, `:=`(consider = as.integer(pd_valid > 0 & mmtype == mmtest &
        #                                     d_pimptype == 0 & is_pd_ltrip == 0),
        #             mismatch = as.integer(d_location_type != lag(d_pcat3) &
        #                                     lag(d_pcat3) != v_missing),
        #             lag_d_purpose = lag(d_purpose),
        #             lag_d_pcat2 = lag(d_pcat2)),
        #      by = c('hh_id', 'person_num', 'day_num')]
        #

        fcn_imptype_13 <- function(SD) {
          # Test whether there is anything to do and exit ASAP
          consider_lgl <- with(
            SD,
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0 &
              d_pcat3 != v_missing &
              is_pd_ltrip == 0
          )

          if (!any(consider_lgl)) {
            return(SD)
          }

          SD1 <- copy(SD)

          # Make some temporary variables needed later
          SD1[
            ,
            `:=`(
              consider = as.integer(consider_lgl),
              notok = as.integer(
                (lead(d_location_type) != d_pcat3 & d_pcat3 != v_missing) |
                  pd_ltrip > 0 & (d_pcat3 == v_home | d_pimputed == v_home) &
                    lag(d_pcat3) == v_missing
              )
            )
          ]


          # Find out if there is anything to do in this function
          consider_idx <- which(SD1[, consider] == 1)

          if (length(consider_idx) > 0) {
            for (start_idx in consider_idx) {
              ok <- SD1[start_idx:nrow(SD1), notok] == 0

              if (all(ok[!is.na(ok)])) {
                # Change trip purpose for first mismatched row
                SD1[
                  start_idx,
                  `:=`(
                    d_pimptype = imputation_type,
                    d_pimputed = get_purpose_code(age, survey_values, v_check, v_school, purpimp),
                    d_pimputed_cat = pcatimp[v_check],
                    d_ltimputed = v_check,
                    d_pcat3 = v_check,
                    mmtype = 0
                  )
                ]

                # Change trip purposes for subsequent rows
                SD1[
                  (start_idx + 1):nrow(SD1),
                  `:=`(
                    d_pimptype = imputation_type,
                    d_pimputed = d_purpose_lag,
                    d_pimputed_cat = d_pcat2_lag,
                    d_ltimputed = d_location_type,
                    mmtype = 0
                  )
                ] # Set to zero b/c mmtypes are not correct for these rows anyway

                break()
              }
            }
          }

          # Drop temporary variables
          SD1[
            ,
            `:=`(
              consider = NULL,
              notok = NULL
            )
          ]

          return(SD1)
        }

        trip <- trip[, fcn_imptype_13(.SD), by = .(person_id, day_num)]
      }

      # Test 8 -------------------------------------------------------------------
      # Test 8: Still a lot of cases with home at end of day that don't have purpose home

      imputation_type <- 14

      min_dwell <- 180

      if (iteration > 1 & v_check == 1) {
        trip[
          ,
          change_idx := (
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0 &
              is_pd_ltrip == 1 &
              dwell_mins >= min_dwell)
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = get_purpose_code(age, survey_values, v_check, v_school, purpimp),
            d_pimputed_cat = pcatimp[v_check],
            d_ltimputed = v_check,
            d_pcat3 = v_check,
            mmtype = 0
          )
        ]

        # Check if it created a new home-home trip with previous home location.
        # If it did, assign this trip purpose to previous location (swap)

        trip[
          ,
          change_idx2 := change_idx & !is_pd_ftrip & (lag(d_ltimputed) == v_check)
        ]

        trip[
          which(change_idx2) - 1,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = d_purpose_lead,
            d_pimputed_cat = d_pcat2_lead,
            d_ltimputed = v_other,
            d_pcat3 = v_other
          )
        ]

        trip[, change_idx := NULL]

        trip[, change_idx2 := NULL]
      }


      # Test 9 -------------------------------------------------------------------
      # Test 9: Sometimes home, work and school can have same locations. Check
      # with distance and accept purpose if within distance

      imputation_type <- 15

      max_distance <- 200

      if (iteration > 1) {
        # Create a temporary variable
        trip[, dcheck := 99999]
        trip[d_pcat2 == get("home", purpose_categories), dcheck := d_distance_home]
        trip[d_pcat2 == get("work", purpose_categories), dcheck := d_distance_work]
        trip[d_pcat2 == get("school", purpose_categories), dcheck := d_distance_school]
        trip[dcheck < 0, dcheck := 99999]

        trip[
          ,
          change_idx := (pd_valid > 0 &
            mmtype == mmtest &
            d_pimptype == 0 &
            dcheck <= max_distance)
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = d_purpose,
            d_pimputed_cat = d_pcat2,
            mmtype = 0
          )
        ]

        trip[
          change_idx == TRUE & d_pcat2 == get("home", purpose_categories),
          `:=`(d_ltimputed = v_home, d_pcat3 = v_home)
        ]

        trip[
          change_idx == TRUE & d_pcat2 == get("work", purpose_categories),
          `:=`(d_ltimputed = v_work, d_pcat3 = v_work)
        ]

        trip[
          change_idx == TRUE & d_pcat2 == get("school", purpose_categories),
          `:=`(d_ltimputed = v_school, d_pcat3 = v_school)
        ]

        trip[, dcheck := NULL]
        trip[, change_idx := NULL]
      }

      # Test 10 ------------------------------------------------------------------
      # Test 10: Purpose "work related" at primary workplace can be changed to
      # work, unless adjacent purpose is work

      imputation_type <- 16

      if (iteration > 1 & v_check == 2) {
        trip[
          ,
          change_idx := (pd_valid > 0 &
            mmtype == mmtest &
            d_pimptype == 0 & d_pcat2 == get("work_related", purpose_categories) &
            (is_pd_ftrip == 1 | lag(d_pcat2) != get("work", purpose_categories)) &
            (is_pd_ltrip == 1 | lead(d_pcat2) != get("work", purpose_categories)))
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = get_purpose_code(age, survey_values, v_check, v_school, purpimp),
            d_pimputed_cat = pcatimp[v_check],
            d_ltimputed = v_check,
            d_pcat3 = v_check,
            mmtype = 0
          )
        ]

        trip[, change_idx := NULL]
      }

      # Test 11 ------------------------------------------------------------------
      # Test 11: Change purpose to location type if it won't create two trips with
      # the same purpose

      imputation_type <- 17

      tmp <- copy(trip)

      if (iteration > 1) {
        trip[
          ,
          change_idx := (
            pd_valid > 0 & mmtype == mmtest & d_pimptype == 0)
        ]

        trip[, d_pimputed_cat_lag := lag(d_pimputed_cat), by = "person_id"]
        trip[, d_pimputed_cat_lead := lead(d_pimputed_cat), by = "person_id"]

        trip[, `:=`(prevcat = 0, nextcat = 0)]

        trip[
          change_idx == TRUE & is_pd_ftrip == 1,
          prevcat := pcatimp[o_location_type]
        ]

        trip[
          change_idx == TRUE & is_pd_ftrip == 0 & lag(mmtype) == 0,
          prevcat := d_pimputed_cat_lag
        ]

        trip[
          change_idx == TRUE & is_pd_ftrip == 0 & lag(mmtype) != 0,
          prevcat := d_pcat2_lag
        ]

        trip[change_idx == TRUE & is_pd_ltrip == 1, nextcat := 0]

        trip[
          change_idx == TRUE & is_pd_ltrip == 0 & lead(mmtype) == 0,
          nextcat := d_pimputed_cat_lead
        ]

        trip[
          change_idx == TRUE & is_pd_ltrip == 0 & lead(mmtype) != 0,
          nextcat := d_pcat2_lead
        ]

        trip[
          ,
          change_idx2 := change_idx &
            (prevcat != pcatimp[v_check] & nextcat != pcatimp[v_check])
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx2 == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = get_purpose_code(age, survey_values, v_check, v_school, purpimp),
            d_pimputed_cat = pcatimp[v_check],
            d_ltimputed = v_check,
            d_pcat3 = v_check,
            mmtype = 0
          )
        ]

        # check again for home-home trips created in previous step
        # (necessary because all trips are considered simultaneously,
        #  not sequentially as in Mark's Pascal code)

        trip[, `:=`(prevcat = 0, nextcat = 0)]

        trip[, d_pimputed_cat_lag := lag(d_pimputed_cat), by = "person_id"]

        trip[, d_pimputed_cat_lead := lead(d_pimputed_cat), by = "person_id"]

        trip[
          change_idx == TRUE & is_pd_ftrip == 1,
          prevcat := pcatimp[o_location_type]
        ]

        trip[
          change_idx == TRUE & is_pd_ftrip == 0 & lag(mmtype) == 0,
          prevcat := d_pimputed_cat_lag
        ]

        trip[
          change_idx == TRUE & is_pd_ftrip == 0 & lag(mmtype) != 0,
          prevcat := d_pcat2_lag
        ]

        trip[, change_idx2 := change_idx & (prevcat == pcatimp[v_check])]

        # Revert values where change_idx2 is true
        repl <- tmp[trip[, change_idx2]]

        trip[
          change_idx2 == TRUE,
          `:=`(
            d_pimptype = repl[, d_pimptype],
            d_pimputed = repl[, d_pimputed],
            d_pimputed_cat = repl[, d_pimputed_cat],
            d_ltimputed = repl[, d_ltimputed],
            d_pcat3 = repl[, d_pcat3],
            mmtype = repl[, mmtype]
          )
        ]

        trip[, change_idx := NULL]
        trip[, change_idx2 := NULL]
      }

      # No further test defined --------------------------------------------------
      # Very few cases remaining, just keep current purpose, but don't set
      # the mismatch type to 0

      imputation_type <- 19

      if (iteration >= niterations) {
        trip[
          ,
          change_idx := (
            pd_valid > 0 & mmtype == mmtest & d_pimptype == 0)
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = d_purpose,
            d_pimputed_cat = d_pcat2,
            d_ltimputed = d_location_type,
            d_pcat3 = d_location_type
          )
        ]

        trip[, change_idx := NULL]
      }
    }

    # Loop on three location types home, work, school -------------------------------

    for (v_check in v_home:v_school) { # for v_check (O/x purposes)

      mmtest <- 2 * v_check # 2, 4, 6

      # Types 2, 4, 6: Imputed destination location is not home/work/school,
      # stated destination purpose is. Note that if these O/x destinations
      # already have an imputed purpose based on an x/O trip in the same day (e.g.
      # from Rules 4, 5, 6 or 7 above), then no further imputation is needed here.

      # Test 1 ------------------------------------------------------------------
      # Test 1: check for a likely escort stop

      imputation_type <- 20

      max_dwell <- 30

      if (iteration >= 1) {
        trip[
          ,
          change_idx := (
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0 &
              is_pd_ltrip == 0 &
              mode_type == lead(mode_type) &
              num_trav_tmp != lead(num_trav_tmp) &
              dwell_mins <= max_dwell)
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type,
            d_pimputed = get("purpose_other_escort", survey_values)[1],
            d_pimputed_cat = get("escort", purpose_categories),
            d_ltimputed = v_other,
            d_pcat3 = v_other,
            mmtype = 0
          )
        ]

        trip[, change_idx := NULL]
      }

      # Test 2 ------------------------------------------------------------------
      # Test 2: If the trip is within various distance limits of the appropriate
      # location type, and there is not an adjacent trip with the same location
      # type, then accept the purpose

      imputation_type <- 21

      # Set some constants
      maxdist_1 <- 200
      maxdist_2 <- 300
      maxdist_3 <- 500

      if (iteration >= 1) {
        # Create a temporary variable
        trip[, dcheck := d_distance_school]
        trip[v_check == v_home, dcheck := d_distance_home]
        trip[v_check == v_work, dcheck := d_distance_work]
        trip[dcheck < 0 | is.na(dcheck), dcheck := 99999]

        trip[
          ,
          change_idx := (
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0 &
              o_location_type != v_check &
              (is_pd_ltrip == 1 | lead(d_location_type) != v_check))
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE & dcheck <= maxdist_3,
          `:=`(
            d_pimptype = imputation_type + 2,
            d_pimputed = d_purpose,
            d_pimputed_cat = d_pcat2,
            d_ltimputed = v_check,
            d_pcat3 = v_check,
            mmtype = 0
          )
        ]

        trip[
          change_idx == TRUE & dcheck <= maxdist_2,
          d_pimptype := imputation_type + 1
        ]

        trip[
          change_idx == TRUE & dcheck <= maxdist_1,
          d_pimptype := imputation_type
        ]

        trip[, dcheck := NULL]
        trip[, change_idx := NULL]
      }

      # Test 3 -------------------------------------------------------------------
      # Test 3: If the visit is the last destination of the day (spans 3 am) and has a stay of at
      # least X min, then it is an overnight stay.
      # Classify such cases according to type and frequency:
      #   (1) in the region, visited once,
      #   (2) in the region, visited multiple times,
      #   (3) out of the region visited once,
      #   (4) out of the region, visited multiple times.
      # (To qualify as a multiple visit, the other trip(s) must also be O/H trips
      # with location within 150 m of the first O/H trip.)
      # Set the imputed purpose to 'non-home overnight destination', perhaps with
      # additional categorization by type/frequency, and stop.

      imputation_type <- 24

      # Set some constants
      min_dwell <- 180
      min_distance <- 500

      if (iteration >= 1) {
        trip[
          ,
          change_idx := (
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0 &
              is_pd_ltrip == 1 &
              dwell_mins >= min_dwell &
              d_distance_home >= min_distance)
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[
          change_idx == TRUE,
          `:=`(
            d_pimptype = imputation_type + d_in_region,
            d_pimputed = fifelse(
              d_in_region == 1,
              get("purpose_overnight_in_region", survey_values),
              get("purpose_overnight_out_region", survey_values)
            ),
            d_pimputed_cat = get("overnight", purpose_categories),
            d_ltimputed = v_other,
            d_pcat3 = v_other,
            mmtype = 0
          )
        ]

        trip[, change_idx := NULL]
      }

      # Test 4 -------------------------------------------------------------------
      # Test 4: Some short stops look like bogus stops that were given the same
      # purpose as the next trip, but have very short duration.  If
      # dwell-time<=10 min, and next mode and purpose and location type ok - add a
      # code to merge the trip

      # this step was commented out in Mark's original code see commit history
      # for previous code

      imputation_type <- 26

      # Test 5 ------------------------------------------------------------------
      # Remaining "work" trips not to primary work location should be
      # recorded as work-related purpose

      # this step was commented out in Mark's original code see commit history
      # for previous code

      imputation_type <- 28

      # Test 6 ------------------------------------------------------------------
      # Remaining "school" trips not to primary school location should be
      # recorded as school-related purpose

      # this step was commented out in Mark's original code see commit history
      # for previous code

      imputation_type <- 29

      # No rules apply, but will be fixed below ---------------------------------

      imputation_type <- 30

      if (iteration >= niterations) {
        trip[
          ,
          change_idx := (
            pd_valid > 0 &
              mmtype == mmtest &
              d_pimptype == 0)
        ]

        message(
          "iteration = ", iteration,
          " | mmtest = ", mmtest,
          " | imputation_type = ", imputation_type,
          " | no. of cases = ", trip[change_idx == TRUE, .N]
        )

        trip[change_idx == TRUE, `:=`(d_pimptype = imputation_type)]

        trip[, change_idx := NULL]
      }
    }

    # Test 7 ------------------------------------------------------------------
    # The destination purpose is missing, and has not been imputed as part of the
    # imputation for other trip types. Only do this on the last iteration -
    # last resort. Count all other stops made by the same person during the survey
    # period to destinations within 150 meters of the same destination with
    # purposes other than the given one. If there are multiple other visits by the
    # person, draw one at random to be the "matching" one. If there are no other
    # visits by the person, then tabulate all other stops made by other persons
    # during the survey period to destinations within 75 meters of the
    # destination. If there are multiple such visits, pick one at random to be the
    # matching one. If it is not visited  by any person, then set the purpose to
    # 'other-cannot be imputed'. (Note: This is an agreed-upon variation of the
    # method discussed and tested earlier. There should be very few cases in the
    # leftover category. The cases with missing purpose and mode will also be
    # candidates for mode imputation, which will be dealt with outside this
    # program

    imputation_type <- 31

    # Set constants
    dlimit1 <- 50
    dlimit2 <- 100
    dlimit3 <- 200

    # For each trip within a person-day that meets the criteria
    if (iteration >= niterations) {
      # Get trips that still need imputation
      which_imp <- which(
        with(trip, (pd_valid == 1) & (d_pimptype %in% c(30) | mmtype == 7))
      )

      message(
        "imputation_type = ", imputation_type,
        " | no. of cases = ", length(which_imp)
      )

      # loop over all such trips
      for (ii in seq_along(which_imp)) {
        if (ii %% 200 == 0) {
          message("\tFinding matches for no. ", ii)
        }

        # initialize dcheck
        trip[, dcheck := NA_real_]

        i <- which_imp[ii]

        # Reset d_pimptype
        trip[i, d_pimptype := 0]

        pid <- trip[i, person_id]
        tid <- trip[i, trip_id]

        # find all the rows with the same person_id
        # that are within 200 m and have the right d_pcat2
        pers_idx <- with(
          trip,
          (person_id == pid &
            d_pcat2 != purpose_categories["other"] &
            !d_pcat2 %in% c(
              get("home", purpose_categories),
              get("work", purpose_categories),
              get("school", purpose_categories)
            ) &
            trip_id != tid)
        )

        ninrange1 <- 0
        ninrange2 <- 0
        ninrange3 <- 0

        npers_idx <- sum(pers_idx, na.rm = TRUE)

        if (npers_idx > 0) {
          trip[
            pers_idx,
            dcheck := gcdist(
              trip[i, d_lat],
              trip[i, d_lon],
              d_lat,
              d_lon
            )
          ]

          inrange_idx1 <- with(trip, dcheck <= dlimit1)
          ninrange1 <- sum(inrange_idx1, na.rm = TRUE)

          inrange_idx2 <- with(trip, dcheck > dlimit1 & dcheck <= dlimit2)
          ninrange2 <- sum(inrange_idx2, na.rm = TRUE)

          inrange_idx3 <- with(trip, dcheck > dlimit2 & dcheck <= dlimit3)
          ninrange3 <- sum(inrange_idx3, na.rm = TRUE)
        }

        if (sum(c(ninrange1, ninrange2, ninrange3)) > 0) {
          rnd <- trip[i, rand0to1]

          if (ninrange1 > 0) {
            imputation_type <- 31
            match_idx <- which(inrange_idx1)[floor(rnd * ninrange1) + 1]
          } else if (ninrange2 > 0) {
            imputation_type <- 32
            match_idx <- which(inrange_idx2)[floor(rnd * ninrange2) + 1]
          } else if (ninrange3 > 0) {
            imputation_type <- 33
            match_idx <- which(inrange_idx3)[floor(rnd * ninrange3) + 1]
          }

          # Assign trip purposes
          trip[
            trip_id == tid,
            `:=`(
              d_pimptype = imputation_type,
              d_pimputed = trip[match_idx, d_purpose],
              d_pimputed_cat = trip[match_idx, d_pcat2],
              d_ltimputed = v_other,
              d_pcat3 = v_other,
              mmtype = 0
            )
          ]
        } else { # End assign purpose from same person

          # If no suitable trips found within a person, search other people

          # Did this person make work or school trips?
          nworkt <- trip[
            person_id == pid & d_pcat2 %in% c(
              get("work", purpose_categories),
              get("work_related", purpose_categories)
            ),
            .N
          ]

          nschlt <- trip[
            person_id == pid & d_pcat2 %in% c(
              get("school", purpose_categories)
            ),
            .N
          ]

          othr_idx <- with(
            trip,
            (person_id != pid & d_pcat2 != get("other", purpose_categories) &
              (nworkt > 0 | !(d_pcat2 %in% c(
                get("work", purpose_categories),
                get("work_related", purpose_categories)
              ))) &
              (nschlt > 0 | d_pcat2 != get("school", purpose_categories)))
          )

          trip[
            othr_idx,
            dcheck := gcdist(
              trip[i, d_lat],
              trip[i, d_lon],
              d_lat,
              d_lon
            )
          ]

          inrange_idx1 <- with(trip, dcheck <= dlimit1)
          ninrange1 <- sum(inrange_idx1, na.rm = TRUE)

          inrange_idx2 <- with(trip, dcheck > dlimit1 & dcheck <= dlimit2)
          ninrange2 <- sum(inrange_idx2, na.rm = TRUE)

          inrange_idx3 <- with(trip, dcheck > dlimit2 & dcheck <= dlimit3)
          ninrange3 <- sum(inrange_idx3, na.rm = TRUE)

          if (sum(c(ninrange1, ninrange2, ninrange3)) > 0) {
            rnd <- trip[i, rand0to1]

            if (ninrange1 > 0) {
              imputation_type <- 34
              match_idx <- which(inrange_idx1)[floor(rnd * ninrange1) + 1]
            } else if (ninrange2 > 0) {
              imputation_type <- 35
              match_idx <- which(inrange_idx2)[floor(rnd * ninrange2) + 1]
            } else if (ninrange3 > 0) {
              imputation_type <- 36
              match_idx <- which(inrange_idx3)[floor(rnd * ninrange3) + 1]
            }

            # Assign trip purposes
            trip[
              i,
              `:=`(
                d_pimptype = imputation_type,
                d_pimputed = case_when(
                  trip[match_idx, d_pcat2] == get("home", purpose_categories) ~
                    get("purpose_social", survey_values),
                  trip[match_idx, d_pcat2] == get("work", purpose_categories) ~
                    get("purpose_work_related", survey_values),
                  trip[match_idx, d_pcat2] == get("school", purpose_categories) ~
                    get("purpose_other_edu", survey_values),
                  TRUE ~ as.integer(trip[match_idx, d_purpose])
                ),
                d_pimputed_cat = case_when(
                  trip[match_idx, d_pcat2] == get("home", purpose_categories) ~
                    get("social_rec", purpose_categories),
                  trip[match_idx, d_pcat2] == get("work", purpose_categories) ~
                    get("work_related", purpose_categories),
                  trip[match_idx, d_pcat2] == get("school", purpose_categories) ~
                    get("school_related", purpose_categories),
                  TRUE ~ as.integer(trip[match_idx, d_pcat2])
                ),
                d_ltimputed = v_other,
                d_pcat3 = v_other,
                mmtype = 0
              )
            ]
          }

          # if not imputed and stated purpose is work, use work related
          if (trip[i, d_pimptype] == 0 &
            trip[i, d_pcat3] == v_work &
            trip[i, d_location_type] != v_work) {
            imputation_type <- 37

            trip[
              i,
              `:=`(
                d_pimptype = imputation_type,
                d_pimputed = get("purpose_work_related", survey_values),
                d_pimputed_cat = get("work_related", purpose_categories),
                d_ltimputed = v_other,
                d_pcat3 = v_other,
                mmtype = 0
              )
            ]

            # if stated purpose is school, use school related
          } else if (trip[i, d_pimptype] == 0 &
            trip[i, d_pcat3] == v_school &
            trip[i, d_location_type] != v_school) {
            imputation_type <- 38

            trip[
              i,
              `:=`(
                d_pimptype = imputation_type,
                d_pimputed = get("purpose_other_edu", survey_values),
                d_pimputed_cat = get("school_related", purpose_categories),
                d_ltimputed = v_other,
                d_pcat3 = v_other,
                mmtype = 0
              )
            ]
          } else if (trip[i, d_pimptype] == 0) {
            # no other evidence code as other/missing
            imputation_type <- 39

            trip[
              i,
              `:=`(
                d_pimptype = imputation_type,
                d_pimputed = get("purpose_other", survey_values),
                d_pimputed_cat = get("other", purpose_categories),
                d_ltimputed = d_location_type,
                mmtype = 0
              )
            ]
          }
        }
      }
    }
  }

  # Test 8 --------------------------------------------------------------------
  # Origin purpose for the first trip of the day
  # In most cases, the imputed destination purpose for the previous trip will be
  # the imputed origin purpose for the current trip.  For the first survey day,
  # however, and perhaps for survey days where the preceding day is incomplete
  # or has no travel, there may be no previous trip, or it may provide incorrect
  # data. For such cases, the imputed purpose will be set to the imputed origin
  # location type (Home, Work, School, Other). For cases where the origin
  # location type is Other, the process for Missing purpose (Type 7) can be used

  # for all except first trip of day, use imputed destination purpose
  # Create lead and lag versions of purpose with all trips and by person_id
  trip[,
    `:=`(
      d_pimputed_lag = lag(d_pimputed),
      d_pimputed_cat_lag = lag(d_pimputed_cat),
      d_ltimputed_lag = lag(d_ltimputed)
    ),
    by = "person_id"
  ]

  trip[
    pd_valid > 0 & is_pd_ftrip == 0,
    `:=`(
      o_pimptype = 1L,
      o_pimputed = as.integer(d_pimputed_lag),
      o_pimputed_cat = as.integer(d_pimputed_cat_lag),
      o_ltimputed = as.integer(d_ltimputed_lag)
    ),
    by = .(person_id, day_num)
  ]

  # if first trip of day is a valid follow up to last trip of prev. day, use that
  trip[, pd_valid_lag := lag(pd_valid), by = .(person_id)]

  trip[
    pd_valid > 0 & trip_num > 1 & pd_valid_lag > 0 & is_pd_ftrip == 1,
    `:=`(
      o_pimptype = 2L,
      o_pimputed = as.integer(d_pimputed_lag),
      o_pimputed_cat = as.integer(d_pimputed_cat_lag),
      o_ltimputed = as.integer(d_ltimputed_lag)
    ),
    by = .(person_id)
  ]

  # first trip of day needs to be imputed based on location
  trip[
    pd_valid > 0 & !(trip_num > 1 & pd_valid_lag > 0) & is_pd_ftrip == 1,
    `:=`(
      o_pimptype = 2L + o_location_type,
      o_pimputed = as.integer(purpimp[o_location_type]),
      o_pimputed_cat = as.integer(pcatimp[o_location_type]),
      o_ltimputed = as.integer(o_location_type)
    ),
    by = .(person_id)
  ]

  # This is creating some new home-trips - add code to set based on location type
  # - may be inconsistent with previous day, but that is better than starting
  # the day with a home-home trip}

  trip[
    pd_valid > 0 & is_pd_ftrip == 1 &
      o_location_type != 1 & o_pimputed_cat == get("home", purpose_categories) & d_pimputed_cat == get("home", purpose_categories),
    `:=`(
      o_pimptype = 2 + o_location_type,
      o_pimputed = purpimp[o_location_type],
      o_pimputed_cat = pcatimp[o_location_type],
      o_ltimputed = o_location_type
    )
  ]


  # Write out the data ----------------------------------------------------------

  # fix a few cases that still have old missing codes - to new missing codes
  trip[
    d_pimputed < -1 | d_pimputed > 900,
    d_pimputed := get("purpose_other", survey_values)
  ]

  trip[
    o_pimputed < -1 | o_pimputed > 900,
    o_pimputed := get("purpose_other", survey_values)
  ]

  trip_out <- trip

  return(trip_out)
}
