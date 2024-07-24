USAGE = """
  Script to load the cached bay area network NxMap file and save it as a shapefile and gpkg.
"""
import pathlib

import config
import momepy
from mappymatch.maps.nx.nx_map import NxMap

if __name__ == "__main__":

    regional_network_file = pathlib.Path(
        "E:\\Box\\DataViz Projects\\Spatial Analysis and Mapping\\TDS Conflation\\Data\\bay_area_network.json"
    )
    regional_map = NxMap.from_file(str(regional_network_file))

    # https://docs.momepy.org/en/stable/user_guide/graph/convert.html
    edges_gdf = momepy.nx_to_gdf(regional_map.g, points=False)
    print(edges_gdf.dtypes)

    # if values in the edges_gdf are lists, convert to strings
    for col in edges_gdf.columns:
        if not edges_gdf.dtypes[col] == object:
            continue
        print(f"Checking column {col} for for list type")

        is_list = edges_gdf[col].apply(lambda x: isinstance(x, list))
        if is_list.any():
            print(f"list elements:\n{edges_gdf.loc[is_list, :]}")
            edges_gdf[col] = edges_gdf[col].astype(str)

    output_file = pathlib.Path() / "output" / "bay_area_network.shp"
    edges_gdf.to_file(output_file)
    print(f"Wrote {output_file}")

    output_file = pathlib.Path() / "output" / "bay_area_network.gpkg"
    edges_gdf.to_file(output_file, driver="GPKG")
    print(f"Wrote {output_file}")
