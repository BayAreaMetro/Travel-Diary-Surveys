import pathlib

import config
import geopandas as gpd
import osmnx as ox

# Define paths to the regional boundary and output file paths
out_gpkg = config.out_file_path / "bay_area_road_map.gpkg"


def download_bay_area_roadmap(
    polygon_path: pathlib.Path, out_path: pathlib.Path, network_type="drive"
):
    """Download the road network for the Bay Area region within the given polygon and save it as a geopackage.

    Args:
        polygon_path (pathlib.Path): Path to the polygon geojson file.
        network_type (string {"all", "all_public", "bike", "drive", "drive_service", "walk"}): what type of street network to get. Defaults to "drive".
    """
    # read the polygon
    region_gdf = gpd.read_file(polygon_path)
    # create a graph from the polygon
    g = ox.graph_from_polygon(polygon=region_gdf.iloc[0]["geometry"], network_type=network_type)
    # save graph as a geopackage
    ox.save_graph_geopackage(g, filepath=out_path)


if __name__ == "__main__":
    download_bay_area_roadmap(config.region_boundary_path, out_gpkg)
