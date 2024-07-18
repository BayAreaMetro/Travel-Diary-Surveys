import pathlib
import getpass

# Define user
user = getpass.getuser()

# Define file name
location_tbl = "location.csv"
trip_tbl = "trip.csv"

# Define Box System Root Directory
if user=="lzorn":
    box_dir = pathlib.Path("E:\Box")
else:
    box_dir = pathlib.Path(f"/Users/{user}/Library/CloudStorage/Box-Box")

# Define BAUS directory on Box for .csv output files
file_dir = box_dir / "Modeling and Surveys" / "Surveys" / "Travel Diary Survey" / \
    "Biennial Travel Diary Survey" / "Data" / "2023" / "Full Unweighted 2023 Dataset"

# Define input file paths
location_path = file_dir / location_tbl
trip_path = file_dir / trip_tbl

# Define output file paths
out_file_path = box_dir / "DataViz Projects" / "Spatial Analysis and Mapping" / "TDS Conflation" / "Data"
gpkg_path = out_file_path / "tds_conflation_results.gpkg"
