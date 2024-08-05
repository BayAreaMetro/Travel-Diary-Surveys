import getpass
import pathlib

# Define user
user = getpass.getuser()

# Define file name
location_tbl = "location.csv"
trip_tbl = "trip.csv"

# M directory for survey work
M_survey_data_dir = pathlib.Path("M:\\Data\\HomeInterview\\Bay Area Travel Study 2023\\Data")

# Define input file paths
location_path = M_survey_data_dir / "Full Unweighted 2023 Dataset" / location_tbl
trip_path = M_survey_data_dir / "Full Unweighted 2023 Dataset" / trip_tbl

region_boundary_path = (
    M_survey_data_dir
    / "Survey Conflation"
    / "OSM_regional_network_convex_hull"
    / "bay_area_regional_boundary_convex_hull.geojson"
)
local_network_path = (
    M_survey_data_dir
    / "Survey Conflation"
    / "OSM_regional_network_convex_hull"
    / "bay_area_network.json"
)

# Define output file paths
out_file_path = M_survey_data_dir / "Full Unweighted 2023 Dataset" / "OSM_match_v2"
gpkg_path = out_file_path / "tds_conflation_results.gpkg"
