
# General:
### Primary Sampling Units (PSUs) in CI calculation:
I think you might be missing the sampling unit, i.e., household_id and person_id in some of the survey designs for calculating trip-level CI estimates. This would affect the standard error CI calculations. I'll be honest the exact math is a bit fuzzy but in general it accounts for "clustered" data where multiple observations come from the same higher-level unit (e.g., multiple trips from the same person). The result is that standard errors will become larger.

Otherwise you end up with artificially low standard errors at the trip level since you get multiple trips per person and multiple persons per household and all those trips will have the exact same weight per person *(or should without the whacky bias adjustment)*. 

I saw they were used in some scripts but not all. `hh_id` used here:

- `Calculate_Package_Deliveries.R`
- `Calculate_RemoteWorkersByCounty.R`
- `Calculate_telework_freq_shares.R`

But nothing here:

- `BATS_2019_2023_TripRate_TripDistance.R`
- `BATS_2019_2023_TripLevel_Shares.R`
- `Calculate_home_to_work_distance.R`
- `Calculate_SharesByCommuteCatetories.R`
- `summarize_BATS_2019_2023_for_dashboard.R`
- `summarize_BATS_2019_2023_DepartureTime.R`

Example:

```R
srv_design <- df_dummy %>%
        as_survey_design(
        ids = c(hhno, pno),  # Nested: households contain persons
        weights = trexpfac, 
        strata = all_of(strata_vars)
)
```

### Diffs CIs before minus after adding PSU to survey design
The SE ends up bigger in all cases. Mostly small but might be noticeable in some plots.
```
survey_cycle mode_label weighted_share            se        ci_95 ci_lower_95
        <dbl> <chr>               <dbl>         <dbl>        <dbl>       <dbl>
1         2019 WALK                    0 -0.00588      -0.0115      0.0115
2         2019 DA                      0 -0.00949      -0.0186      0.0186
3         2019 OTHER                   0 -0.000965     -0.00189     0.00189    
4         2019 TNC                     0 -0.000517     -0.00101     0.00101
5         2019 WALKTRAN                0 -0.00304      -0.00595     0.00595
6         2019 HOV2                    0 -0.00621      -0.0122      0.0122     
7         2019 HOV3                    0 -0.0104       -0.0204      0.0204
8         2019 BIKE                    0 -0.00150      -0.00294     0.00294
9         2019 DRIVETRAN               0 -0.000198     -0.000387    0.000387   
10         2019 SCHBUS                  0 -0.0000000500 -0.000000113 0.000000113
11         2023 WALK                    0 -0.00400      -0.00784     0.00784
12         2023 DA                      0 -0.00731      -0.0143      0.0143     
13         2023 OTHER                   0 -0.000852     -0.00167     0.00167
14         2023 TNC                     0 -0.000435     -0.000853    0.000853   
15         2023 WALKTRAN                0 -0.00130      -0.00256     0.00256
16         2023 HOV2                    0 -0.00426      -0.00836     0.00836
17         2023 HOV3                    0 -0.00858      -0.0168      0.0168     
18         2023 BIKE                    0 -0.00138      -0.00271     0.00271
19         2023 DRIVETRAN               0 -0.0000189    -0.0000371   0.0000371
20         2023 SCHBUS                  0 -0.0000248    -0.0000486   0.0000486
```


## BATS_2019_2023\Add_Stratification_Variables_And_Labels_to_LinkedTrips.R
* Has 2019 income been adjusted for inflation to match 2023?


## BATS_2019_2023_TripLevel_Shares.R
* line 68: Why only consider full-time workers? Is it because it's ambiguous to gauge hybrid-ness? If so, you could try normalizing using a ratio of telework_freq / commute_freq to get a sense of telework intensity for part-time workers too. Narrowing to full-time makes sense, but it does leave out a big chunk of the workforce. The problem is the 2-3 days a week is ambiguous.

(Unweighted)
| employment | label                          | n     | %       | Cumulative % |
|------------|--------------------------------|-------| --------| -------------|
| 7          | Unpaid volunteer or intern     | 369   | 0.48%   | 0.48%        |
| 2          | Part-time                      | 5980  | 7.81%   | 8.29%        |
| 3          | Self-employed                  | 9931  | 12.97%  | 21.27%       |
| 1          | Full-time                      | 60272 | 78.73%  | 100.00%      |


* lines 261-288: Creates a bunch of NAs, does it need to be pivoted first or something? I'm not a tidyverse person so it's hard for me to interpret all the pipe logic.


## summarize_BATS_2019_2023_DepartureTime.R
* Otherwise everything looks good to me.
* I wonder if we might consider travel duration in stead of just departure time? Not sure if it holds any insight.


## Calculate_telework_freq_shares.R
* Similar question about limiting to full-time workers only.


## BATS_2019_2023_TripRate_TripDistance
