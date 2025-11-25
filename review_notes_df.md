
# BATS_2019_2023\Add_Stratification_Variables_And_Labels_to_LinkedTrips.R
* Has 2019 income been adjusted for inflation to match 2023?


# BATS_2019_2023\BATS_2019_2023_TripLevel_Shares.R
* line 68: Only considering full time workers for work from home analysis? 
* lines 80-220: I think you might be missing the sampling unit, i.e., household_id and person_id. I think this would affect the variance estimates. I'll be honest it 

    ```R
    srv_design <- df_dummy %>%
    as_survey_design(
        ids = c(hhno, pno),  # Nested: households contain persons
        weights = trexpfac, 
        strata = all_of(strata_vars)
    )
    ```


    ## Before:
    ```
    survey_cycle mode_label weighted_share        se    ci_95 ci_lower_95
            <dbl> <chr>               <dbl>     <dbl>    <dbl>       <dbl>
    1         2019 WALK            0.136     0.00298   0.00583    0.130    
    2         2019 DA              0.455     0.00532   0.0104     0.445
    3         2019 OTHER           0.00526   0.000768  0.00150    0.00375
    4         2019 TNC             0.00987   0.000889  0.00174    0.00812
    5         2019 WALKTRAN        0.0513    0.00216   0.00423    0.0471
    6         2019 HOV2            0.193     0.00446   0.00874    0.184
    7         2019 HOV3            0.130     0.00388   0.00761    0.123
    8         2019 BIKE            0.0178    0.00101   0.00198    0.0158   
    9         2019 DRIVETRAN       0.00153   0.000754  0.00148    0.0000512
    10         2019 SCHBUS          0.0000714 0.0000714 0.000140  -0.0000686
    11         2023 WALK            0.150     0.00211   0.00413    0.145    
    12         2023 DA              0.409     0.00319   0.00625    0.403
    13         2023 OTHER           0.0187    0.000837  0.00164    0.0170
    14         2023 TNC             0.00604   0.000447  0.000875   0.00517  
    15         2023 WALKTRAN        0.0294    0.000991  0.00194    0.0274
    16         2023 HOV2            0.220     0.00282   0.00553    0.215
    17         2023 HOV3            0.151     0.00297   0.00582    0.145    
    18         2023 BIKE            0.0157    0.000744  0.00146    0.0143
    19         2023 DRIVETRAN       0.000192  0.0000725 0.000142   0.0000498
    20         2023 SCHBUS          0.000453  0.000225  0.000440   0.0000134
    ```
    ### After
    ```
    survey_cycle mode_label weighted_share        se    ci_95 ci_lower_95
            <dbl> <chr>               <dbl>     <dbl>    <dbl>       <dbl>
    1         2019 WALK            0.136     0.00886   0.0174     0.118
    2         2019 DA              0.455     0.0148    0.0290     0.426    
    3         2019 OTHER           0.00526   0.00173   0.00340    0.00186
    4         2019 TNC             0.00987   0.00141   0.00276    0.00711
    5         2019 WALKTRAN        0.0513    0.00519   0.0102     0.0411   
    6         2019 HOV2            0.193     0.0107    0.0209     0.172
    7         2019 HOV3            0.130     0.0143    0.0280     0.102
    8         2019 BIKE            0.0178    0.00251   0.00492    0.0129   
    9         2019 DRIVETRAN       0.00153   0.000952  0.00187   -0.000336
    10         2019 SCHBUS          0.0000714 0.0000715 0.000140  -0.0000687
    11         2023 WALK            0.150     0.00611   0.0120     0.138    
    12         2023 DA              0.409     0.0105    0.0206     0.388
    13         2023 OTHER           0.0187    0.00169   0.00331    0.0154
    14         2023 TNC             0.00604   0.000882  0.00173    0.00432  
    15         2023 WALKTRAN        0.0294    0.00230   0.00450    0.0249
    16         2023 HOV2            0.220     0.00709   0.0139     0.206
    17         2023 HOV3            0.151     0.0115    0.0226     0.128
    18         2023 BIKE            0.0157    0.00212   0.00416    0.0116
    19         2023 DRIVETRAN       0.000192  0.0000915 0.000179   0.0000127
    20         2023 SCHBUS          0.000453  0.000249  0.000489  -0.0000352
    ```

    ### Diffs:
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

    --> Not a huge deal, but still increases the standard errors a bit, larger error bars.