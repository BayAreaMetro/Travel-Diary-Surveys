import yaml
import geopandas as gpd
import os
_join = os.path.join
class GeoCrosswalker:
    def __init__(self, config_file):
        """
        Initialize the GeoCrosswalker with a configuration file in YAML format.

        :param config_file: Path to the YAML configuration file.
        """
        self.config = self._load_config(config_file)

    def _load_config(self, config_file):
        """
        Load the configuration from a YAML file.

        :param config_file: Path to the YAML configuration file.
        :return: Parsed configuration as a dictionary.
        """
        try:
            with open(config_file, 'r') as file:
                return yaml.safe_load(file)
        except FileNotFoundError:
            raise FileNotFoundError(f"Configuration file '{config_file}' not found.")
        except yaml.YAMLError as e:
            raise ValueError(f"Error parsing YAML file: {e}")

    def _load_geodata(self, data):
        """
        Load geospatial data from a file path or return the GeoDataFrame if already provided.

        :param data: File path to a shapefile or a GeoDataFrame object.
        :return: GeoDataFrame
        """
        if isinstance(data, str):
            return gpd.read_file(_join(self.config['geo_dir'], data))
        elif isinstance(data, gpd.GeoDataFrame):
            return data
        else:
            raise ValueError("Input must be a file path or a GeoDataFrame.")

    def geo_crosswalk_point(self, shapefiles, id_columns):
        """
        Create a geo crosswalk by nesting smaller layers into larger layers using representative points.

        :param shapefiles: List of shapefile paths or GeoDataFrames in order of smallest to largest.
        :param id_columns: List of column names to use as IDs for each shapefile.
        :return: A GeoDataFrame representing the geo crosswalk.
        """
        if len(shapefiles) < 2:
            raise ValueError("At least two shapefiles are required to create a geo crosswalk.")

        if len(shapefiles) != len(id_columns):
            raise ValueError("The number of shapefiles must match the number of ID columns.")

        crosswalk = None

        for i in range(len(shapefiles) - 1):
            # Load the smaller and larger layers
            smaller_layer = self._load_geodata(shapefiles[i])[[id_columns[i], 'geometry']]
            larger_layer = self._load_geodata(shapefiles[i + 1])[[id_columns[i + 1], 'geometry']]

            # Ensure both layers have the same CRS
            if smaller_layer.crs != larger_layer.crs:
                larger_layer = larger_layer.to_crs(smaller_layer.crs)

            # Convert the smaller layer to a point layer using representative_point
            smaller_layer['geometry'] = smaller_layer['geometry'].representative_point()

            # Perform spatial join to nest smaller layer within the larger layer
            joined = gpd.sjoin(smaller_layer, larger_layer, how='left', predicate='within')

            # Keep only necessary columns for the crosswalk
            joined = joined[[id_columns[i], id_columns[i + 1]]]

            # Ensure all base geographies are preserved in the output
            joined = smaller_layer.merge(joined, on=id_columns[i], how='left')

            # Update crosswalk for the next iteration
            crosswalk = joined if crosswalk is None else crosswalk.merge(joined, on=id_columns[i], how='left')

        return crosswalk

    def geo_crosswalk_intersection(self, shapefiles, id_columns, normalize=True):
        """
        Create a geo crosswalk by calculating the share of intersection between smaller and larger geometries.

        :param shapefiles: List of shapefile paths or GeoDataFrames in order of smallest to largest.
        :param id_columns: List of column names to use as IDs for each shapefile.
        :return: A GeoDataFrame representing the geo crosswalk with intersection shares.
        """
        if len(shapefiles) < 2:
            raise ValueError("At least two shapefiles are required to create a geo crosswalk.")

        if len(shapefiles) != len(id_columns):
            raise ValueError("The number of shapefiles must match the number of ID columns.")

        crosswalk = None

        for i in range(len(shapefiles) - 1):
            # Load the smaller and larger layers
            smaller_layer = self._load_geodata(shapefiles[i])[[id_columns[i], 'geometry']]
            larger_layer = self._load_geodata(shapefiles[i + 1])[[id_columns[i + 1], 'geometry']]

            # Ensure both layers have the same CRS
            if smaller_layer.crs != larger_layer.crs:
                larger_layer = larger_layer.to_crs(smaller_layer.crs)

            # Calculate intersection between smaller and larger geometries
            intersection = gpd.overlay(smaller_layer, larger_layer, how='intersection')

            # Calculate the area of the intersection and the original smaller geometry
            intersection['intersection_area'] = intersection['geometry'].area
            smaller_layer['original_area'] = smaller_layer['geometry'].area

            # Merge the intersection areas back to the smaller layer to calculate share
            intersection = intersection.merge(smaller_layer[[id_columns[i], 'original_area']], on=id_columns[i])
            intersection['share'] = intersection['intersection_area'] / intersection['original_area']

            # Filter out intersections with a share less than 5%
            if normalize:
                intersection = intersection[intersection['share'] >= 0.05]
                # Renormalize shares so they sum to 1 for each smaller geography
                intersection['normalized_share'] = intersection.groupby(id_columns[i])['share'].transform(lambda x: x / x.sum())
                # Keep only necessary columns for the crosswalk
                intersection = intersection[[id_columns[i], id_columns[i + 1], 'normalized_share']]
                intersection.rename(columns={'normalized_share': 'share'}, inplace=True)
            else:
                intersection = intersection[[id_columns[i], id_columns[i + 1], 'share']]

            # Ensure all base geographies are preserved in the output
            intersection = smaller_layer[[id_columns[i]]].merge(intersection, on=id_columns[i], how='left')

            # Update crosswalk for the next iteration
            crosswalk = intersection if crosswalk is None else crosswalk.merge(intersection, on=id_columns[i], how='left')

        return crosswalk

    def fill_nearest_value(self, gdf, group_columns, value_column):
        """
        For each record in a GeoDataFrame, find the record that shares the longest boundary with it
        and fill in the value for the target column if it is missing.

        :param gdf: GeoDataFrame with geometries.
        :param group_columns: List of column names to group by for finding nearest neighbors.
        :param value_column: The column whose missing values will be filled.
        :return: GeoDataFrame with missing values filled.
        """
        def find_longest_boundary(row, candidates):
            # Calculate the shared boundary length with each candidate
            shared_boundaries = candidates.geometry.apply(lambda x: row.geometry.intersection(x).length)
            # Filter out records where the shared boundary length is 0
            valid_candidates = shared_boundaries[shared_boundaries > 0]
            if valid_candidates.empty:
                
                row_point = row.geometry.representative_point()
                candidates_points = candidates.geometry#.apply(lambda x: x.representative_point())
                distances = candidates_points.apply(lambda x: row_point.distance(x))
                # Filter out records where distance is not > 0
                valid_candidates = distances[distances > 0]
                # Find the candidate with the nearest point
                longest_boundary = valid_candidates.idxmin()
                if valid_candidates.empty:
                    return None
            else:
                longest_boundary = valid_candidates.idxmax()

            return candidates.loc[longest_boundary, value_column]

        # Create a copy of the GeoDataFrame to avoid modifying the original
        gdf = gdf.copy()

        # Split the GeoDataFrame into groups based on the group_columns
        all_candidates = gdf.dropna(subset=[value_column])
        missing_rows = gdf[gdf[value_column].isna()]

        # Fill missing values
        def find_value(row, candidates, value_column):
            mini_cand = candidates[candidates[group_columns].eq(row[group_columns]).all(axis=1)]
            if not mini_cand.empty:
                return find_longest_boundary(row, mini_cand)
            return None
        gdf.loc[gdf[value_column].isna(), value_column] = missing_rows.apply(find_value, axis=1, args=(all_candidates, value_column))
        
        return gdf


# Example usage:
# crosswalker = GeoCrosswalker('path_to_config.yml')
# crosswalk = crosswalker.geo_crosswalk_intersection(['smallest.shp', 'medium.shp', 'largest.shp'], ['id_small', 'id_medium', 'id_large'])