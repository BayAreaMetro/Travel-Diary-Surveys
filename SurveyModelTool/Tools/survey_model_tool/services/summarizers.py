# Generalized Folium choropleth function
import folium
from folium.features import GeoJsonTooltip


import yaml
import os
import geopandas as gpd
import pandas as pd
from shapely.geometry import Point, LineString
from shapely.ops import transform
import warnings
import folium
import branca.colormap as cm


class SurveyModelSummarizer:
    def __init__(self, data, config):
        self.data = data
        self.config = self.load_config(config)

    @staticmethod
    def load_config(config_path):
        """
        Load configuration from a YAML file.

        Args:
            config_path (str): Path to the YAML configuration file

        Returns:
            dict: Configuration dictionary loaded from the YAML file

        Raises:
            FileNotFoundError: If the config file doesn't exist
            yaml.YAMLError: If the YAML file is malformed
        """
        if not os.path.exists(config_path):
            raise FileNotFoundError(f"Config file not found: {config_path}")

        try:
            with open(config_path, 'r', encoding='utf-8') as file:
                config = yaml.safe_load(file)
            return config
        except yaml.YAMLError as e:
            raise yaml.YAMLError(f"Error parsing YAML config file {config_path}: {e}")
        except Exception as e:
            raise Exception(f"Error reading config file {config_path}: {e}")


    def choropleth(self, gdf, value_column, tooltip_fields, geo_id = 'TAZ',tooltip_aliases=None, map_location=[37.4, -122.1], zoom_start=9, fill_color='YlGnBu'):
        """
        Create a Folium choropleth map with tooltips from a GeoDataFrame.
        Args:
            gdf: GeoDataFrame with geometry and data columns.
            value_column: Column name to color the choropleth.
            tooltip_fields: List of column names to show in tooltip.
            tooltip_aliases: List of aliases for tooltip fields (optional).
            map_location: [lat, lon] for map center.
            zoom_start: Initial zoom level.
            fill_color: Choropleth color scheme.
        Returns:
            folium.Map object
        """
        geojson_data = gdf.to_json()
        m = folium.Map(location=map_location, zoom_start=zoom_start, tiles='cartodbpositron')
        folium.Choropleth(
            geo_data=geojson_data,
            data=gdf,
            columns=[geo_id, value_column],
            key_on=f'feature.properties.{geo_id}',
            fill_color=fill_color,
            fill_opacity=0.7,
            line_opacity=0.2,
            legend_name=value_column
        ).add_to(m)
        folium.GeoJson(
            geojson_data,
            tooltip=GeoJsonTooltip(
                fields=tooltip_fields,
                aliases=tooltip_aliases if tooltip_aliases else tooltip_fields,
                localize=True
            )
        ).add_to(m)
        return m


    def display_shapefile(
        self,
        gdf,
        tooltip_fields=None,
        tooltip_aliases=None,
        map_location=[37.4, -122.1],
        zoom_start=9,
        style_kwargs=None
    ):
        """
        Display any shapefile (lines or polygons) on a Folium map with optional tooltips.

        Args:
            gdf (GeoDataFrame): The GeoDataFrame to display.
            tooltip_fields (list): List of attribute columns to show in tooltip.
            tooltip_aliases (list): List of aliases for tooltip fields (optional).
            map_location (list): [lat, lon] for map center.
            zoom_start (int): Initial zoom level.
            style_kwargs (dict): Style options for GeoJson layer.

        Returns:
            folium.Map: Folium map object with the shapefile displayed.
        """
        m = folium.Map(location=map_location, zoom_start=zoom_start, tiles='cartodbpositron')
        geojson_data = gdf.to_json()

        # Default style: blue lines/polygons with some transparency
        default_style = {
            'color': 'blue',
            'weight': 2,
            'fillOpacity': 0.2,
            'opacity': 0.8
        }
        if style_kwargs:
            default_style.update(style_kwargs)

        def style_function(feature):
            return default_style

        tooltip = None
        if tooltip_fields:
            tooltip = GeoJsonTooltip(
                fields=tooltip_fields,
                aliases=tooltip_aliases if tooltip_aliases else tooltip_fields,
                localize=True
            )

        folium.GeoJson(
            geojson_data,
            style_function=style_function,
            tooltip=tooltip
        ).add_to(m)

        return m


    def path_intersects(self, reference_geometry, origin_taz_col=None, dest_taz_col=None, 
                       taz_geometry_source=None, flag_column_name='path_intersects'):
        """
        Create straight line paths between origin and destination TAZ and check if they intersect 
        with a reference geometry.
        
        Args:
            reference_geometry: Shapely geometry or GeoDataFrame to check intersections against
            origin_taz_col (str, optional): Column name for origin TAZ. If None, reads from config
            dest_taz_col (str, optional): Column name for destination TAZ. If None, reads from config  
            taz_geometry_source: GeoDataFrame with TAZ geometries/centroids, or path to shapefile
            flag_column_name (str): Name for the new column indicating intersection (default: 'path_intersects')
            
        Returns:
            pandas.DataFrame: Original data with added intersection flag column
            
        Raises:
            ValueError: If required config parameters or geometries are missing
        """
        # Get column names from config if not provided
        if origin_taz_col is None:
            origin_taz_col = self.config.get('origin_taz_column')
            if origin_taz_col is None:
                raise ValueError("origin_taz_col not provided and 'origin_taz_column' not found in config")
                
        if dest_taz_col is None:
            dest_taz_col = self.config.get('dest_taz_column') 
            if dest_taz_col is None:
                raise ValueError("dest_taz_col not provided and 'dest_taz_column' not found in config")
        
        # Load TAZ geometries if path provided
        if isinstance(taz_geometry_source, str):
            if not os.path.exists(taz_geometry_source):
                raise FileNotFoundError(f"TAZ geometry file not found: {taz_geometry_source}")
            taz_gdf = gpd.read_file(taz_geometry_source)
        elif isinstance(taz_geometry_source, gpd.GeoDataFrame):
            taz_gdf = taz_geometry_source
        else:
            # Try to get from config
            taz_shapefile = self.config.get('taz_shapefile')
            if taz_shapefile and os.path.exists(taz_shapefile):
                taz_gdf = gpd.read_file(taz_shapefile)
            else:
                raise ValueError("TAZ geometry source not provided and 'taz_shapefile' not found in config or file doesn't exist")
        
        # Ensure TAZ geometries have a TAZ ID column
        taz_id_col = self.config.get('taz_id_column', 'TAZ')  # Default to 'TAZ' if not in config
        if taz_id_col not in taz_gdf.columns:
            # Try common TAZ column names
            possible_cols = ['TAZ', 'taz', 'TAZ_ID', 'taz_id', 'ZONE', 'zone']
            taz_id_col = next((col for col in possible_cols if col in taz_gdf.columns), None)
            if taz_id_col is None:
                raise ValueError(f"Could not find TAZ ID column in geometry data. Available columns: {list(taz_gdf.columns)}")
        
        # Create centroids if geometries are polygons
        if taz_gdf.geometry.iloc[0].geom_type in ['Polygon', 'MultiPolygon']:
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                taz_gdf['centroid'] = taz_gdf.geometry.centroid
            point_geom = 'centroid'
        else:
            point_geom = 'geometry'
        
        # Create a dictionary mapping TAZ ID to geometry
        taz_geom_dict = dict(zip(taz_gdf[taz_id_col], taz_gdf[point_geom]))
        
        # Prepare reference geometry
        if isinstance(reference_geometry, gpd.GeoDataFrame):
            # If it's a GeoDataFrame, union all geometries
            ref_geom = reference_geometry.unary_union
            ref_crs = reference_geometry.crs
        else:
            # Assume it's already a Shapely geometry
            ref_geom = reference_geometry
            ref_crs = None  # Will check below

        # Check CRS/projection match between reference and TAZ geometries
        if hasattr(taz_gdf, "crs") and taz_gdf.crs is not None:
            taz_crs = taz_gdf.crs
            # If reference is a GeoDataFrame, compare CRS
            if ref_crs is not None and taz_crs != ref_crs:
                # Reproject reference_geometry to match taz_gdf
                if isinstance(reference_geometry, gpd.GeoDataFrame):
                    reference_geometry = reference_geometry.to_crs(taz_crs)
                    ref_geom = reference_geometry.unary_union
                    ref_crs = taz_crs
                else:
                    warnings.warn("Reference geometry is not a GeoDataFrame and CRS does not match TAZ geometry. Please ensure they are in the same CRS.")
        # If reference is a shapely geometry, warn user to ensure CRS match
        elif ref_crs is not None:
            warnings.warn("TAZ geometry has no CRS information, but reference geometry does. Please ensure they are in the same coordinate system.")

        # Create a copy of the data to avoid modifying the original
        result_data = self.data.copy()
        
        # Check if required columns exist in data
        if origin_taz_col not in result_data.columns:
            raise ValueError(f"Origin TAZ column '{origin_taz_col}' not found in data")
        if dest_taz_col not in result_data.columns:
            raise ValueError(f"Destination TAZ column '{dest_taz_col}' not found in data")
        
        # Function to create line and check intersection
        def check_intersection(row):
            try:
                origin_taz = row[origin_taz_col]
                dest_taz = row[dest_taz_col]
                
                # Skip if TAZ values are missing or invalid
                if pd.isna(origin_taz) or pd.isna(dest_taz):
                    return False
                
                # Get origin and destination points
                origin_point = taz_geom_dict.get(origin_taz)
                dest_point = taz_geom_dict.get(dest_taz)
                
                if origin_point is None or dest_point is None:
                    return False
                
                # Create straight line between points
                line = LineString([origin_point, dest_point])
                
                # Check intersection with reference geometry
                return line.intersects(ref_geom)
                
            except Exception as e:
                # Log warning but continue processing
                print(f"Warning: Error processing row {row.name}: {e}")
                return False
        
        # Apply intersection check to all rows
        result_data[flag_column_name] = result_data.apply(check_intersection, axis=1)
        self.data = result_data
        return 



    def plot_od_layers( self, gdf = None, plot_destinations = True, geo_id = 'COUNTY', flow_table = None, 
                            origin_col='OCOUNTY', dest_col='DCOUNTY', weight_col='trip_weight',
                            map_center=[37.3, -121.8], filter = None, zoom_start=9):
        """
        Create a Folium map with a layer for each origin county, coloring destination counties by OD flow.
        """
        fmap = folium.Map(location=map_center, zoom_start=zoom_start, tiles='cartodbpositron')

        poly_filter = filter
        if plot_destinations:
            title_fill = 'origin'
            filter_col1 = origin_col
            filter_col2 = dest_col
        else:
            title_fill = 'destination'
            filter_col1 = dest_col
            filter_col2 = origin_col
        if filter is None:
            poly_filter = flow_table[filter_col1].unique()
        flow_table = flow_table[flow_table[filter_col1].isin(poly_filter)]
        # Get global min/max for color scale
        max_flow = flow_table[~(flow_table[filter_col1] == flow_table[filter_col2])][weight_col].max()
        min_flow = flow_table[weight_col][flow_table[weight_col] > 0].min() if (flow_table[weight_col] > 0).any() else 0
        colormap = cm.linear.YlOrRd_09.scale(min_flow, max_flow)
        colormap.caption = "Trips from selected {}".format(title_fill)
        for poly in poly_filter:
            flows = flow_table[flow_table[filter_col1] == poly].copy()
            flows = flows.set_index(filter_col2)[weight_col]
            gdf_layer = gdf.copy()
            gdf_layer['flow'] = gdf_layer[geo_id].map(flows).fillna(0)
            gdf_layer = gdf_layer[gdf_layer.flow > 0]
            if gdf_layer.empty:
                print(f"No flows found for {title_fill} {poly}. Skipping layer.")
                continue
            # Format the flow column with commas and no decimal points (as string)
            gdf_layer['flow_fmt'] = gdf_layer['flow'].apply(lambda x: f"{int(round(x)):,}")
            def style_function(feature):
                flow = feature['properties']['flow']
                return {
                    'fillColor': colormap(flow) if flow > 0 else '#cccccc',
                    'color': 'black',
                    'weight': 1,
                    'fillOpacity': 0.7 if flow > 0 else 0.2,
                }
            tooltip = folium.GeoJsonTooltip(
                fields=[geo_id, 'flow_fmt'],
                aliases=[geo_id.title(), 'Trips']
            )
            layer = folium.FeatureGroup(name=f"{title_fill.title()}: {poly}", show=False)
            # Add the choropleth layer
            folium.GeoJson(
                gdf_layer,
                style_function=style_function,
                tooltip=tooltip,
                name=f"{title_fill.title()}: {poly}"
            ).add_to(layer)

            # Add the reference polygon outline (no fill, only outline)
            # Find the reference polygon in gdf by geo_id == poly
            ref_poly_gdf = gdf[gdf[geo_id] == poly]
            if not ref_poly_gdf.empty:
                def outline_style(feature):
                    return {
                        'fillColor': 'none',
                        'color': 'blue',
                        'weight': 3,
                        'fillOpacity': 0.0,
                        'opacity': 1.0
                    }
                folium.GeoJson(
                    ref_poly_gdf,
                    style_function=outline_style,
                    name=f"Outline: {poly}"
                ).add_to(layer)

            layer.add_to(fmap)
        colormap.add_to(fmap)
        folium.LayerControl(collapsed=False).add_to(fmap)
        return fmap

    def trip_rates( self,trips=None, pop = None, person_group_col=None, trip_group_col=None, person_weight_col = 'person_weight', trip_weight_col='trip_weight'):
        """Calculate trip rates based on specified grouping columns and trip weight column."""
        if trips is None:
            trip_data = self.trips.merge(self.person, on='person_id', how='left').merge(self.hh, on='hh_id', how='left')
            pop = self.person
        else:
            trip_data = trips.copy()
            pop = pop.copy()

        if person_group_col is None:
            person_group_col = ['gender']
            print(f"No grouping columns specified. Defaulting to {person_group_col}")
        if trip_group_col is None:
            group_list = person_group_col
        else:
            group_list = person_group_col + [trip_group_col]
        print(group_list)
        trip_data = trip_data.groupby(group_list,as_index = False)[[trip_weight_col]].sum()
        pop = pop.groupby(person_group_col,as_index = False)[[person_weight_col]].sum()
        trip_summary = trip_data.merge(pop, left_on=person_group_col, right_on=person_group_col, how='left')
        trip_summary['trip_rate'] = trip_summary[trip_weight_col] / trip_summary[person_weight_col]
        trip_summary['trip_rate'] = trip_summary['trip_rate'].fillna(0)

        return trip_summary

# Additional summarization methods can be added here as needed