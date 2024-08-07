-- Draft --
# Travel Diary Survey Conflation <!-- omit in toc -->

### Table of Contents

- [Define the Problem Statement](#define-the-problem-statement)
- [Project Scope](#project-scope)
- [Project Resources](#project-resources)
- [Data Sources](#data-sources)
  - [trips schema](#trips-schema)
  - [locations schema](#locations-schema)
- [Analysis Parameters](#analysis-parameters)
- [Methodology](#methodology)
  - [Install Required Packages](#install-required-packages)
  - [Usage Guide](#usage-guide)
    - [Description](#description)
    - [Arguments](#arguments)
    - [Example Usage](#example-usage)
    - [Running the Script](#running-the-script)
- [Expected Outcomes](#expected-outcomes)
- [Results](#results)
- [Tags](#tags)

## Define the Problem Statement

Associate travel diary survey smartphone trip traces with Bay Area roadway facilities to enable matching of survey demographics/trip characteristics of users with bridges, express lanes, etc. The script should work with hundreds of thousands, possibly millions of x,y smartphone pings:

    1. Starts with a network, possibly OpenStreetMap, but ideally it could be flexible and accommodate a modeling network (and maybe other networks too)
    2. It also starts with a set of points with x,y values associated with numbered trips (each trip has multiple points associated with it, and has a unique household, person, and trip ID)
    3. The script should match the points to network facility links
    4. Smartphone pings aren't constant, so some facility segments are skipped. The script should fill in interstitial links that were skipped by the list of trip points
    5. The script would probably be in Python, though I personally would love something in R. I don't know if it would use PostGIS/Redshift, or something else to manage the large data files. We're probably open about this.
    6. The script should filter out non-auto modes (otherwise it will include BART trips on routes within freeway medians)
    7. The data include PII

## Project Scope

- **Data Collection** Shimon Israel
- **Data Preparation/ Modeling**  Joshua Croff
- **Data Development**  Joshua Croff
- **Data Analysis / Summary**  Shimon Isreal 

## Project Resources

- [Asana Task](https://app.asana.com/0/304776046055605/1206835675432259/f)
- [Input File Directory on Box](https://mtcdrive.box.com/s/igest7sgigyt24rexyobsbdgr7vrl5ei)
- [Output File Directory on Box](https://mtcdrive.box.com/s/5zo8d8ytesqaqya23os543wkdf29ksfr)

## Data Sources

- [trips.csv (internal access only)](https://mtcdrive.box.com/s/5zo8d8ytesqaqya23os543wkdf29ksfr)
- [locations.csv (internal access only)](https://mtcdrive.box.com/s/j7wrtou0mrlvfio5owj4iia7hfye524c)


### trips schema

| Field Name   | Description                                           | Domain                                                                                   |
|--------------|-------------------------------------------------------|------------------------------------------------------------------------------------------|
| trip_id      | Unique identifier for the trip                        |                                                                                          |
| o_in_region  | Trip originates in region                             | 0: No<br>1: Yes                                                                          |
| d_in_region  | Trip destination in region                            | 0: No<br>1: Yes                                                                          |
| mode_type    | Primary mode used to complete trip                    | 1: Walk<br>2: Bike<br>3: Bikeshare<br>4: Scootershare<br>5: Taxi<br>6: TNC<br>7: Other<br>8: Car<br>9: Careshare<br>10: School bus<br>11: Shuttle/Vanpool<br>12: Ferry<br>13: Transit<br>14: Long Distance Passenger<br>995: Missing Response |
| mode_1       | First mode of travel used to complete trip if multiple modes used | Too many to list                                                                         |
| mode_2       | Second mode of travel used to complete trip if multiple modes used | Too many to list                                                                         |
| mode_3       | Third mode of travel used to complete trip if multiple modes used  | Too many to list                                                                         |
| mode_4       | Fourth mode of travel used to complete trip if multiple modes used | Too many to list                                                                         |

### locations schema
| Field Name   | Description                                           | Domain                                                                                   |
|--------------|-------------------------------------------------------|------------------------------------------------------------------------------------------|
| trip_id      | Unique identifier for the trip                        |                                                                                          |
| collect_time | | |
| accuracy | | |
| bearing | | |
| speed | | |
| lat | Latitude | |
| lon | Longitude | |

## Analysis Parameters

For this project, the following parameters will be used:

- **Network**: OpenStreetMap
- **Network type**: Drive
- **Trips**: Mode Type = Taxi, TNC, Car, Careshare, Shuttle/Vanpool & Origin/Destination in Region

## Methodology

### Install Required Packages

1. Install the required package dependencies using the [environment.yml](environment.yml) file.
   1. `conda env create -f environment.yml`
2. Clone the [mappymatch github repository](https://github.com/BayAreaMetro/mappymatch), which has been forked and modified from the original repository.
   1. Install the mappymatch package by running `pip install -e /path/to/mappymatch`.

### Usage Guide

#### Description
The `tds_conflation.py` script processes GPS trip traces and matches them to the OpenStreetMap network. The script is designed to run efficiently on large datasets and can be run in parallel. The script can be run using a locally-stored OSM map for the Bay Area or each trip can be conflated to an OSM network with calls to the Overpass API via `mappymatch` functions. 

#### Arguments

- `--test`: Run in test mode. When this flag is set, the output will be saved locally instead of being uploaded to Box. 
- `--num_trip_ids`: Specifically the number of unique trip IDs to process. This is useful for testing with a smaller dataset. (Type: `int`)
- `processes`: Number of processes to use for parallel processing. The default value is 8. (Type: `int`)
- `--use_regional_nx_map`: Use a single NxMap instance for the entire region. This helps avoid throttling issues with OsMNx. (Type: `bool`, default: `True`)
- `--geofence_buffer`: Buffer size around the trace to use, in meters. The default value is 1000 meters. (Type: `int`)

#### Example Usage

1. Run in test mode with default settings:

   ```python
   python tds_conflation.py --test
   ```

2. Run with a subset of 100 trip IDs:

   ```python
   python tds_conflation.py --num_trip_ids 100
   ```

3. Run with 4 processes:

   ```python
   python tds_conflation.py --processes 4
   ```

4. Run with a geofence buffer of 500 meters:

   ```python
   python tds_conflation.py --geofence_buffer 500
   ```

5. Run using the network from the API:

   ```python
   python tds_conflation.py --use_regional_nx_map False
   ```

6. Combine multiple arguments:

   ```python
   python tds_conflation.py --test --num_trip_ids 50 --processes 4 --geofence_buffer 1000
   ```

#### Running the Script

To run the script, use the following command in the terminal:

```python
python tds_conflation.py [arguments]
```

Replace `[arguments]` with the desired arguments as shown in the examples above. 

**Notes**:

- The `--use_regional_nx_map` argument defaults to `True` to prevent OsMNx from pulling down data for every trace and getting throttled.
- Ensure that all required dependencies are installed and properly configured before running the script.

## Expected Outcomes

Provide your expectations (if any) for the results of this work. Your expectations will form the basis for deciding if the work is complete, or if we need to revisit the problem statement and/or refine the methodology used to solve the problem.

## Results

TDS Conflation test results: 
- File path: `M:\Data\HomeInterview\Bay Area Travel Study 2023\Data\Full Unweighted 2023 Dataset\OSM_match_v1_first1000trips`
- Tableau online view (internal-only): [trip-trace-viewer - Full Unweighted 2023 Dataset first 1000 trips](https://10ay.online.tableau.com/#/site/metropolitantransportationcommission/workbooks/1834359?:origin=card_share_link)

Geopackage layers:
- trace_gdf: raw original traces
- trace_line_gdf: line created from gps traces
- matched_gdf: matched links in the network
- matched_path_gdf: full matched path through the network

Full Results: `M:\Data\HomeInterview\Bay Area Travel Study 2023\Data\Full Unweighted 2023 Dataset\OSM_match_v2`
Full Base network: `M:\Data\HomeInterview\Bay Area Travel Study 2023\Data\Survey Conflation\OSM_regional_network_convex_hull`

## Tags

**travel diary survey**, **transportation network**, **conflation**