USAGE = """
  Script to load the matched trip gpkg and export the trip_ids that have matches for summary tableau.
"""

import datetime
import pathlib

import config
import geopandas

if __name__ == "__main__":

    print(f"{datetime.datetime.now()} Reading {config.gpkg_path} layer matched_path_gdf")
    matched_path_gpkg_gdf = geopandas.read_file(config.gpkg_path, layer="matched_path_gdf")
    print(matched_path_gpkg_gdf.dtypes)
    print(matched_path_gpkg_gdf.describe())
    print(
        f"rows with trip_id == 0? {len(matched_path_gpkg_gdf.loc[matched_path_gpkg_gdf.trip_id == 0])}"
    )
    # summarize number of rows per trip_id
    trip_id_summary = (
        matched_path_gpkg_gdf.groupby(["trip_id"]).size().reset_index(name="match_counts")
    )
    print(f"trip_id_summary.head():\n{trip_id_summary.head()}")
    output_file = config.out_file_path / "trip_id_matched_path_summary.csv"
    trip_id_summary.to_csv(
        output_file,
        index=False,
        mode="w",
    )
    print(f"{datetime.datetime.now()} Wrote {output_file}")

    print(f"{datetime.datetime.now()} complete")
