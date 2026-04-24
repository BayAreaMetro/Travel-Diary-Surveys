import yaml
import geopandas as gpd
import os
from shapely.ops import unary_union

import numpy as np
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point, Polygon, MultiPolygon
from shapely.ops import unary_union

# Optional imports for network method
try:
    import osmnx as ox
    import networkx as nx
    OSMNX_AVAILABLE = True
except Exception:
    OSMNX_AVAILABLE = False


import os
import sys
from typing import Optional

_join = os.path.join
class GeoCrosswalker:
    def __init__(self, config_file):
        """
        Initialize the GeoCrosswalker with a configuration file in YAML format.

        :param config_file: Path to the YAML configuration file.
        """
        self.config = self._load_config(config_file)
        self.MILES_TO_METERS = 1609.344
        self.SQM_TO_SQMI  = 1.0 / (self.MILES_TO_METERS ** 2)

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

    def geo_crosswalk_point(self, shapefiles, id_columns, center='representative'):
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
            # smaller_layer['geometry'] = smaller_layer['geometry'].representative_point()
            if center == 'centroid':
                smaller_layer = smaller_layer.assign(geometry=smaller_layer['geometry'].centroid)
            elif center == 'representative':
                smaller_layer = smaller_layer.assign(geometry=smaller_layer['geometry'].representative_point())
            else:
                raise ValueError("center parameter must be either 'centroid' or 'representative'")

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

    def _resolve_geometry_overlap(self, original_geom, reference_geoms):
        """
        Resolves overlaps of an original geometry with multiple reference geometries.
        
        Parameters:
        - original_geom: Shapely geometry to check for overlaps.
        - reference_geoms: GeoSeries of reference geometries.
        
        Returns:
        - Shapely geometry with minor overlaps removed.
        """
        # Find intersections with reference geometries
        overlaps = reference_geoms[reference_geoms.intersects(original_geom)]
        
        if len(overlaps) <= 1:
            # If no or only one intersection, return the original geometry
            return original_geom
        
        # Calculate the area of overlap for each intersecting geometry
        overlap_areas = overlaps.apply(lambda ref_geom: original_geom.intersection(ref_geom).area)
        
        # Identify the reference geometry with the largest overlap
        max_overlap_geom = overlaps.loc[overlap_areas.idxmax()]
        
        # Remove overlaps with other reference geometries
        for ref_geom in overlaps:
            if ref_geom != max_overlap_geom:
                original_geom = original_geom.difference(ref_geom)
        
        return original_geom

    def clip_geometry(self, gdf_to_clip, reference_gdf):
        """
        Clips the geometries in gdf_to_clip to the boundaries of reference_gdf.
        """
        # Ensure gdf_to_clip is a GeoDataFrame
        if not isinstance(gdf_to_clip, gpd.GeoDataFrame):
            raise ValueError("gdf_to_clip must be a GeoDataFrame")
        
        # Ensure the geometry column exists
        if 'geometry' not in gdf_to_clip.columns:
            raise KeyError("gdf_to_clip is missing the 'geometry' column")
        
        gdf_to_clip = gdf_to_clip.to_crs(reference_gdf.crs)

        # Ensure CRS alignment
        if gdf_to_clip.crs != reference_gdf.crs:
            raise ValueError("CRS mismatch between gdf_to_clip and reference_gdf")
        
        # Debugging: Print the first few rows of the GeoDataFrames
        # print("gdf_to_clip:")
        # print(gdf_to_clip.head())
        # print("reference_gdf:")
        # print(reference_gdf.head())
        
        # Apply the overlap resolution
        gdf_to_clip['geometry'] = gdf_to_clip.apply(
            lambda row: self._resolve_geometry_overlap(row['geometry'], reference_gdf['geometry']),
            axis=1
        )
        return gdf_to_clip

    def remove_overlaps_small_share(self, gdf: gpd.GeoDataFrame, id_col: str):
        """
        Remove overlapping areas by subtracting the intersection from the polygon
        that has the smaller share of the overlap (intersection area / polygon area).

        Parameters
        ----------
        gdf : GeoDataFrame
            Input polygons (assumed valid; ideally in an equal-area CRS).
        id_col : str
            Column with unique polygon IDs.

        Returns
        -------
        GeoDataFrame
            Polygons with overlaps removed based on smallest share rule.
        """
        # Ensure we only have id + geometry
        base = gdf[[id_col, "geometry"]].copy()

        # Precompute areas (used for share comparisons)
        base["__area__"] = base.geometry.area
        area_map = dict(zip(base[id_col], base["__area__"]))

        # Build left/right for overlay
        left = base.rename(columns={id_col: "left_id"})[["left_id", "geometry"]]
        right = base.rename(columns={id_col: "right_id"})[["right_id", "geometry"]]

        # Pairwise intersections
        inter = gpd.overlay(left, right, how="intersection")

        # Drop self-intersections
        inter = inter[inter["left_id"] != inter["right_id"]].copy()

        if inter.empty:
            # No overlaps; return original
            return gdf

        # Deduplicate symmetric pairs (A,B) vs (B,A)
        inter["pair_key"] = inter.apply(
            lambda r: tuple(sorted((r["left_id"], r["right_id"]))), axis=1
        )
        inter = inter.drop_duplicates(subset=["pair_key"])

        # Compute intersection areas and shares
        inter["int_area"] = inter.geometry.area
        inter["left_share"] = inter["int_area"] / inter["left_id"].map(area_map)
        inter["right_share"] = inter["int_area"] / inter["right_id"].map(area_map)

        # Decide which polygon loses the overlap (smaller share)
        inter["loser_id"] = inter.apply(
            lambda r: r["left_id"] if r["left_share"] < r["right_share"] else r["right_id"], axis=1
        )

        # Collect all overlap geometries to remove per loser_id
        to_remove = (
            inter.groupby("loser_id")["geometry"]
                .apply(lambda geoms: unary_union(list(geoms)))
        )

        # Apply difference per loser_id
        cleaned = base.copy()
        remove_map = dict(to_remove)
        cleaned["geometry"] = cleaned.apply(
            lambda r: r["geometry"].difference(remove_map.get(r[id_col], None))
                    if r[id_col] in remove_map else r["geometry"],
            axis=1
        )

        # Return with original columns (except helper area)
        cleaned = cleaned.drop(columns=["__area__"])
        # Reattach other original columns (if any) by id
        return cleaned

# Example usage:
# crosswalker = GeoCrosswalker('path_to_config.yml')
# crosswalk = crosswalker.geo_crosswalk_intersection(['smallest.shp', 'medium.shp', 'largest.shp'], ['id_small', 'id_medium', 'id_large'])



  

    def estimate_utm_crs(self, gdf: gpd.GeoDataFrame) -> str:
        """
        Estimate a UTM CRS string for a GeoDataFrame in lat/lon.
        Returns EPSG code string like 'EPSG:32610' (northern) or 'EPSG:32710' (southern).
        """
        # Get centroid of total bounds in WGS84
        gdf_wgs = gdf.to_crs("EPSG:4326")
        bounds = gdf_wgs.total_bounds  # minx, miny, maxx, maxy
        lon = (bounds[0] + bounds[2]) / 2.0
        lat = (bounds[1] + bounds[3]) / 2.0
        zone = int((lon + 180) / 6) + 1
        epsg = 32600 + zone if lat >= 0 else 32700 + zone
        return f"EPSG:{epsg}"

    def project_to_utm(self, gdf: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
        """
        Project a GeoDataFrame to an appropriate UTM CRS.
        If OSMnx is available, use its projector; else estimate UTM.
        """
        if OSMNX_AVAILABLE:
            return ox.projection.project_gdf(gdf)
        # Fallback to estimated UTM
        utm = self.estimate_utm_crs(gdf)
        return gdf.to_crs(utm)

    def build_euclidean_walkshed(self,stop_geom, distance_m: float) -> Polygon:
        """Simple circular buffer in meters."""
        return stop_geom.buffer(distance_m)

    def build_network_walkshed(self, G, stop_point, distance_m: float, edge_buffer_m: float = 25.0) -> Polygon:
        """
        Build a network-based walkshed polygon:
        - Project graph to a metric CRS (UTM/appropriate local projection).
        - Find nearest node to the stop (in the same CRS as the graph).
        - Use Dijkstra path lengths ('length' attribute) to get nodes within distance_m.
        - Convert reachable edges to a GeoDataFrame, buffer edges, and union into a polygon.

        Parameters
        ----------
        G : networkx.MultiDiGraph
            OSMnx graph (likely created in EPSG:4326).
        stop_point : shapely.geometry.Point
            Stop location in a **projected CRS (meters)** that matches the projected graph (see below).
        distance_m : float
            Walking distance threshold in meters.
        edge_buffer_m : float
            Buffer applied to reachable edges to polygonize the walkshed.

        Returns
        -------
        shapely.geometry.Polygon or MultiPolygon
        """
        import osmnx as ox
        import networkx as nx
        from shapely.geometry import Polygon, MultiPolygon
        from shapely.ops import unary_union

        # 1) Project the graph to a metric CRS (OSMnx picks a suitable UTM)
        Gp = ox.project_graph(G)

        # Ensure edges have 'length' in meters
        need_lengths = False
        for _, _, data in Gp.edges(data=True):
            if 'length' not in data or data['length'] is None:
                need_lengths = True
                break
        if need_lengths:
            Gp = ox.distance.add_edge_lengths(Gp)

        # 2) Find nearest node to the stop in the **projected graph CRS**
        #    Your stop_point should already be in meters. If not, reproject it to Gp's CRS:
        #    stop_point = gpd.GeoSeries([stop_point], crs=stops_utm.crs).to_crs(ox.graph_to_gdfs(Gp, nodes=False).crs).iloc[0]
        try:
            # Newer OSMnx versions
            node = ox.nearest_nodes(Gp, stop_point.x, stop_point.y)
        except AttributeError:
            # Older OSMnx versions
            node = ox.distance.nearest_nodes(Gp, stop_point.x, stop_point.y)

        # 3) Compute network distances and induce reachable subgraph
        lengths = nx.single_source_dijkstra_path_length(Gp, node, cutoff=distance_m, weight='length')
        if not lengths:
            return stop_point.buffer(1.0)  # tiny fallback polygon

        sub_nodes = set(lengths.keys())
        subgraph = Gp.subgraph(sub_nodes).copy()

        # 4) Convert edges to GeoDataFrame and buffer to polygonize walkshed
        #    Use top-level function in modern OSMnx:
        gdf_nodes, gdf_edges = ox.graph_to_gdfs(subgraph)
        if gdf_edges.empty:
            return stop_point.buffer(1.0)

        # gdf_edges is already in the projected CRS of Gp (meters) -> buffer is in meters
        buffered = gdf_edges.geometry.buffer(edge_buffer_m)
        unioned = unary_union(buffered)

        if isinstance(unioned, (Polygon, MultiPolygon)):
            return unioned

        # Fallback if union returns unexpected geometry type
        return stop_point.buffer(distance_m)


    def compute_area_weighted_totals(self,
        blocks_gdf: gpd.GeoDataFrame,
        walkshed_geom,
        hh_field: str,
        pop_field: str,
        emp_field: str
    ) -> dict:
        """
        Intersect blocks with a walkshed polygon and compute area-weighted households & employment.
        Returns dict with totals and area_m2.
        """
        # Find candidate blocks via spatial index
        candidates_idx = list(blocks_gdf.sindex.intersection(walkshed_geom.bounds))
        candidates = blocks_gdf.iloc[candidates_idx].copy()
        if candidates.empty:
            return {"hh_total": 0.0, "pop_total": 0.0, "emp_total": 0.0, "area_m2": float(walkshed_geom.area)}

        # Intersections and area weights
        hh_sum = 0.0
        pop_sum = 0.0
        emp_sum = 0.0
        for _, row in candidates.iterrows():
            inter = row.geometry.intersection(walkshed_geom)
            if inter.is_empty:
                continue
            inter_area = inter.area
            if inter_area <= 0:
                continue
            block_area = row.geometry.area
            if block_area <= 0:
                continue
            weight = inter_area / block_area
            # Safely read fields (coerce to float)
            hh_val = float(row.get(hh_field, 0) or 0)
            pop_val = float(row.get(pop_field, 0) or 0)
            emp_val = float(row.get(emp_field, 0) or 0)
            hh_sum += hh_val * weight
            pop_sum += pop_val * weight
            emp_sum += emp_val * weight

        return {"hh_total": hh_sum, "pop_total": pop_sum, "emp_total": emp_sum, "area_m2": float(walkshed_geom.area)}


    # ----------------------- Main API -----------------------

    def compute_stop_densities(self,
        blocks: gpd.GeoDataFrame,
        stops: gpd.GeoDataFrame,
        households_field: str,
        employment_field: str,
        pop_field: str,
        stop_id_field: str,
        buffer_miles: float = 0.5,
        method: str = "euclidean",           # 'euclidean' or 'network'
        osm_network_type: str = "walk",      # used only for 'network' method
        edge_buffer_m: float = 25.0,         # used only for 'network' method
        output_gpkg: Optional[str] = None,   # if provided, saves walksheds + attributes
        walkshed_layer_name: str = "stop_walksheds"
    ) -> pd.DataFrame:
        """
        Compute households & employment totals and densities for each bus stop.

        Returns a pandas DataFrame and (optionally) writes a GeoPackage layer with polygons.
        """
        # Load input data
        

        # Validate fields
        for fld in [households_field, pop_field, employment_field]:
            if fld not in blocks.columns:
                raise ValueError(f"Field '{fld}' not found in blocks data columns: {list(blocks.columns)}")

        # Ensure geometries exist
        if blocks.geometry.is_empty.all():
            raise ValueError("Blocks shapefile has empty geometries.")
        if stops.geometry.is_empty.all():
            raise ValueError("Stops shapefile has empty geometries.")

        # Project both to a suitable UTM (meters) for area & buffering
        blocks_utm = self.project_to_utm(blocks)
        stops_utm  = self.project_to_utm(stops)
        # Align CRS
        stops_utm = stops_utm.to_crs(blocks_utm.crs)

        # Buffer distance in meters
        buffer_m = buffer_miles * self.MILES_TO_METERS

        # Prepare OSM network if requested
        import inspect

        # ... inside compute_stop_densities, after you have blocks_utm and buffer_m ...

        G = None
        if method.lower() == "network":
            if not OSMNX_AVAILABLE:
                raise RuntimeError("OSMnx not installed; install it or use method='euclidean'.")

            # Convert blocks extent to WGS84 for OSMnx (it expects lat/lon)
            blocks_ll = blocks_utm.to_crs("EPSG:4326")
            minx, miny, maxx, maxy = blocks_ll.total_bounds  # west, south, east, north
            west, south, east, north = minx, miny, maxx, maxy

            # Slight padding to avoid empty graphs on tight AOIs
            pad_deg = 0.002  # ~200–250 m; tune as needed
            west -= pad_deg
            south -= pad_deg
            east  += pad_deg
            north += pad_deg

            # Detect API: tuple bbox (new) vs 4 positional args (old)
            # In new API, signature has a parameter named 'bbox'
            try:
                sig = inspect.signature(ox.graph_from_bbox)
                has_tuple_param = 'bbox' in sig.parameters
            except Exception:
                # Conservative fallback: assume new API
                has_tuple_param = True

            # Build the graph using the appropriate signature
            try:
                if has_tuple_param:
                    # ✅ New API: tuple bbox
                    bbox = (west, south, east, north)  # (left, bottom, right, top)
                    G = ox.graph_from_bbox(bbox, network_type=osm_network_type, simplify=True)
                else:
                    # ✅ Old API: four positional args
                    G = ox.graph_from_bbox(north, south, east, west, network_type=osm_network_type, simplify=True)
            except Exception as e:
                # If bbox fails (rare, but possible), try polygon-based AOI
                area_poly_utm = unary_union(blocks_utm.geometry).buffer(buffer_m * 0.25)
                area_poly_ll = gpd.GeoSeries([area_poly_utm], crs=blocks_utm.crs).to_crs("EPSG:4326").iloc[0]
                G = ox.graph_from_polygon(area_poly_ll, network_type=osm_network_type, simplify=True)

            # If still empty, enlarge bbox a bit more and retry
            if G.number_of_edges() == 0 or G.number_of_nodes() == 0:
                pad_deg2 = 0.005
                bbox2 = (west - pad_deg2, south - pad_deg2, east + pad_deg2, north + pad_deg2)
                try:
                    if has_tuple_param:
                        G = ox.graph_from_bbox(bbox2, network_type=osm_network_type, simplify=True)
                    else:
                        # translate bbox2 back to 4 args: (north, south, east, west)
                        G = ox.graph_from_bbox(bbox2[3], bbox2[1], bbox2[2], bbox2[0],
                                            network_type=osm_network_type, simplify=True)
                except Exception:
                    pass  # keep whatever we have

            # Add edge lengths only if missing (many OSMnx versions already set 'length')
            # If graph is empty, skip to avoid distance errors.
            if G.number_of_edges() > 0:
                need_lengths = False
                for _, _, data in G.edges(data=True):
                    if 'length' not in data or data['length'] is None:
                        need_lengths = True
                        break
                if need_lengths:
                    G = ox.distance.add_edge_lengths(G)

        # Precompute: area-weighted totals per stop
        results = []
        walkshed_records = []

        # Spatial index for blocks to speed up intersection queries
        blocks_utm = blocks_utm.copy()
        # (Make sure households/employment are numeric)
        for fld in [households_field, pop_field, employment_field]:
            blocks_utm[fld] = pd.to_numeric(blocks_utm[fld], errors="coerce").fillna(0)

        for idx, stop in stops_utm.iterrows():
            stop_id = stop.get(stop_id_field, idx)  # fallback to row index if no stop_id
            geom = stop.geometry

            # Build walkshed polygon
            if method.lower() == "network":
                walkshed = self.build_network_walkshed(G, geom, buffer_m, edge_buffer_m=edge_buffer_m)
            else:
                walkshed = self.build_euclidean_walkshed(geom, buffer_m)

            # Compute area-weighted totals from blocks
            totals = self.compute_area_weighted_totals(blocks_utm, walkshed, households_field, pop_field,  employment_field)

            # Densities
            area_m2 = totals["area_m2"]
            area_sqmi  = area_m2 * self.SQM_TO_SQMI
            hh_density_per_sqmi  = (totals["hh_total"] / area_sqmi)  if area_sqmi  > 0 else np.nan
            pop_density_per_sqmi  = (totals["pop_total"] / area_sqmi)  if area_sqmi  > 0 else np.nan
            emp_density_per_sqmi = (totals["emp_total"] / area_sqmi) if area_sqmi  > 0 else np.nan

            results.append({
                stop_id_field: stop_id,
                "hh_total": totals["hh_total"],
                "pop_total": totals["pop_total"],
                "emp_total": totals["emp_total"],
                "area_m2": area_m2,
                "area_sqmi": area_sqmi,
                "hh_density_per_sqmi": hh_density_per_sqmi,
                "pop_density_per_sqmi": pop_density_per_sqmi,
                "emp_density_per_sqmi": emp_density_per_sqmi
            })

            # Store polygon for optional export
            walkshed_records.append({
                stop_id_field: stop_id,
                "geometry": walkshed,
                "hh_total": totals["hh_total"],
                "pop_total": totals["pop_total"],
                "emp_total": totals["emp_total"],
                "method": method.lower()
            })

        df = pd.DataFrame(results)

        # Optional GeoPackage output (walkshed polygons)
        if output_gpkg:
            walkshed_gdf = gpd.GeoDataFrame(walkshed_records, geometry="geometry", crs=blocks_utm.crs)
            walkshed_gdf.to_file(output_gpkg, layer=walkshed_layer_name, driver="GPKG")
            print(f"Saved walksheds to: {output_gpkg} (layer: {walkshed_layer_name})")

        return df


    # ----------------------- CLI (optional) -----------------------

