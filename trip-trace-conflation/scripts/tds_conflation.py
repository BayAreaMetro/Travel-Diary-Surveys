USAGE = """
  Associate travel diary survey smartphone trip traces with Bay Area roadway facilities to enable matching
  of survey demographics/trip characteristics of users with bridges, express lanes, etc. The script should
  work with hundreds of thousands, possibly millions of x,y smartphone pings.
  
  See Readme.md for more detail.
"""
import argparse
import logging
import multiprocessing
import os
import pathlib
import shutil
import sys
from concurrent.futures import ProcessPoolExecutor, as_completed
from datetime import datetime
from logging.handlers import QueueHandler, QueueListener

import config
import geopandas as gpd
import networkx as nx
import osmnx as ox
import pandas as pd
import pyproj
import shapely.ops
from mappymatch import package_root
from mappymatch.constructs.geofence import Geofence
from mappymatch.constructs.trace import Trace
from mappymatch.maps.nx.nx_map import NetworkType, NxMap
from mappymatch.matchers.lcss.lcss import LCSSMatcher
from mappymatch.utils.crs import LATLON_CRS, XY_CRS
from shapely.geometry import LineString

# if --use_regional_nx_map is specified, then a single regional matcher is created
# per process.  For single process, this is done in main(); for multiprocess, this is done in init_worker()
# This is the instance of that matcher.
process_regional_nx_map = None
# per-process counter for number of trips matched
num_trips_matched = 0


## Define function to create a NxMap object from a GeoJSON file
def nx_map_from_geojson(geojson_path, local_network_path, network_type=NetworkType.DRIVE):
    """Creates a NxMap object from a GeoJSON file.

    Args:
        geojson_path (str): Path to the GeoJSON file. Must be in EPSG:4326.
        network_type (Enumerator, optional): Enumerator for Network Types supported by osmnx. Defaults to NetworkType.DRIVE.

    Returns:
        A NxMap instance
    """
    if not local_network_path.exists():
        logging.info(
            f"Local network file not found. Creating a new network file from geojson: {geojson_path}"
        )
        geofence = Geofence.from_geojson(str(geojson_path))
        logging.debug(f"{geofence.geometry.bounds=}")
        logging.debug(f"{geofence.geometry.area=}")
        nx_map = NxMap.from_geofence(geofence, network_type)
        logging.info(f"Saving network to {local_network_path}")
        nx_map.to_file(str(local_network_path))
    else:
        logging.info(f"Local network file found. Loading network file {local_network_path}")
        nx_map = NxMap.from_file(str(local_network_path))
        logging.info("Completed reading")

    return nx_map


def create_batch_traces(df, trip_id_column, xy=True):
    """Create a batch of traces from a dataframe with xy coordinates

    Args:
        df (Pandas Dataframe): Dataframe with xy coordinates in EPGS:4326.
        trip_id_column (String): Column name with unique trip ids.
        xy (bool, optional): Projects trace to EPSG:3857. Defaults to True.

    Returns:
        List: List of dictionaries with trip_id, trace, trace_gdf, and trace_line_gdf.
        Structure of the list:
        [
            {
                "trip_id": "unique_id",
                "trace": Trace object,
                "trace_gdf": GeoDataFrame with trace points,
                "trace_line_gdf": GeoDataFrame with trace line
            },
            ...
        ]
    """
    unique_ids = df[trip_id_column].unique()
    batch_traces = []
    for i in unique_ids:
        filter_df = df[df["trip_id"] == i]
        gdf = gpd.GeoDataFrame(
            filter_df, geometry=gpd.points_from_xy(filter_df.lon, filter_df.lat), crs=4326
        )
        batch_trace = Trace.from_geo_dataframe(frame=gdf, xy=xy)

        # create a trace_line_gdf from the trace
        coords = [(p.x, p.y) for p in batch_trace.coords]
        line = LineString(coords)
        trace_line_gdf = gpd.GeoDataFrame([{"geometry": line}], crs="EPSG:3857")
        trace_line_gdf["trip_id"] = i

        # create a trace_gdf from the trace
        trace_gdf = batch_trace._frame
        trace_gdf["trip_id"] = i

        # create a dictionary with the trip_id, trace, trace_gdf, and trace_line_gdf and append to the batch_traces list
        trace_dict = {
            "trip_id": i,
            "trace": batch_trace,
            "trace_gdf": trace_gdf,
            "trace_line_gdf": trace_line_gdf,
        }
        batch_traces.append(trace_dict)
    return batch_traces


def init_worker(
    log_queue: multiprocessing.Queue,
    use_regional_nx_map: bool,
    region_boundary_path: pathlib.Path,
    local_network_path: pathlib.Path,
    network_type,
) -> None:
    """Initialize a worker process.

    This initializes the log handling to handoff log messages to the given log_queue.

    It also creates a process-specific regional matcher, if use_regional_nx_map==True
    This is because these can't bre shared between processes because they can't be pickled, so
    each subprocess needs to make its own.
    """
    global process_regional_nx_map
    logger = logging.getLogger()
    logger.handlers.clear()
    logger.setLevel(logging.DEBUG)

    handler = QueueHandler(log_queue)
    logger.addHandler(handler)

    logging.info(f"init_worker() called for {os.getpid()=}")
    if use_regional_nx_map:
        # Commenting out for now but this makes the debug log noisy and adds another osmnx log
        # ox.settings.log_file = True
        # ox.settings.log_level = logging.DEBUG
        # ox.settings.logs_folder = pathlib.Path.cwd()
        logging.info(f"{process_regional_nx_map=}")

        now = datetime.now()
        process_regional_nx_map = nx_map_from_geojson(
            region_boundary_path, local_network_path, network_type
        )
        later = datetime.now()
        logging.info(f"use_regional_nx_map: Creating networkx map took: {later - now}")


def process_trace(
    trace_dict: dict,
    use_regional_nx_map: bool,
    geofence_buffer,
    network_type=NetworkType.DRIVE,
):
    """Process a single trace using a instance of the LCSSMatcher class.

    Returns a matched trace dictionary.

    Args:
        trace_dict (dict): dictionary with trip_id and trace.
        matcher (LCSSMatcher): instance of the LCSSMatcher class.
        geofence_buffer (int): Buffer distance around trip traces. Defaults to 1000 meters.
        network_type (Enumerator, optional): Enumerator for Network Types supported by osmnx. Defaults to NetworkType.DRIVE.

    Returns:
        dict: dictionary with trip_id, trace, matched_result, matched_gdf, and matched_path_gdf.
        Structure of the dictionary:
        {
            "trip_id": trip_id,
            "trace": trace,
            "unmatched_trips": None or trip_id,
            "trace_gdf": trace_gdf,
            "trace_line_gdf": trace_line_gdf,
            "matched_result": match_result,
            "matched_gdf": matched_gdf,
            "matched_path_gdf": matched_path_gdf,
        }

    """
    global process_regional_nx_map
    global num_trips_matched
    # logging.debug(f"process_trace() with {process_regional_nx_map=}")

    try:
        # create a geofence around the trace
        geofence = Geofence.from_trace(trace_dict["trace"], padding=geofence_buffer)
        # note: Geofence crs is set to EPSG:4326 because NxMap.from_geofence() requires it

        # Create a matcher object if matcher is None, else use the provided matcher
        if not use_regional_nx_map:
            # create a networkx map from the geofence
            nx_map = NxMap.from_geofence(geofence, network_type=network_type)
        else:
            # convert Geofence from EPSG:4326 (Geofence always uses this) to EPSG:3857 (to match the osmnx graph)
            project = pyproj.Transformer.from_crs(
                crs_from=LATLON_CRS, crs_to=XY_CRS, always_xy=True
            ).transform

            # create a truncated map for performance using
            # https://osmnx.readthedocs.io/en/stable/user-reference.html#osmnx.truncate.truncate_graph_polygon
            truncated_graph = ox.truncate.truncate_graph_polygon(
                process_regional_nx_map.g, shapely.ops.transform(project, geofence.geometry)
            )
            # create the smaller map from it
            nx_map = NxMap(truncated_graph)

        # create the matcher for the given NxMap
        matcher = LCSSMatcher(nx_map)
        # logging.debug(f"Running match_trace for {trace_dict['trip_id']}")
        match_result = matcher.match_trace(trace_dict["trace"])
    except Exception as e:
        logging.warning(
            f"The trace with trip_id {trace_dict['trip_id']} encountered an exception: {e}. Adding trip to the unmatched list."
        )
        trace_dict["unmatched_trips"] = trace_dict["trip_id"]
        return trace_dict

    num_trips_matched += 1
    MATCH_REPORT_FREQUENCY = 100
    if num_trips_matched % MATCH_REPORT_FREQUENCY == 0:
        logging.info(f"Received results for {num_trips_matched} trips")

    # check if any road ids within a list of matches are null
    road_id_check = True
    for match in match_result.matches:
        if match.road is None:
            road_id_check = False
            break
    if road_id_check == False:
        logging.warn(
            f"The trace with trip_id {trace_dict['trip_id']} has null road_ids meaning there was no match to the network. Adding to the unmatched list."
        )
        trace_dict["unmatched_trips"] = trace_dict["trip_id"]
    else:
        # create a geodataframe from the matches and add the trip_id; add the match result and matched df to the trace dictionary
        matched_df = match_result.matches_to_dataframe()
        matched_df["trip_id"] = trace_dict["trip_id"]
        matched_df["road_id"] = matched_df["road_id"]
        matched_gdf = gpd.GeoDataFrame(matched_df, geometry="geom", crs="EPSG:3857")
        # create a geodataframe from the matched path and add the trip_id; add the match result and matched df to the trace dictionary
        matched_path_df = match_result.path_to_dataframe()
        matched_path_df["trip_id"] = trace_dict["trip_id"]
        matched_path_df["road_id"] = matched_path_df["road_id"]
        matched_path_gdf = gpd.GeoDataFrame(matched_path_df, geometry="geom", crs="EPSG:3857")
        # add network attributes to the matched gdf and matched path gdf
        attrs = ["osmid", "ref", "name", "maxspeed", "highway", "bridge", "tunnel"]
        for attr in attrs:
            # get attributes from the raw graph
            # attr_dict = nx.get_edge_attributes(nx_map.g, attr)
            attr_dict = nx.get_edge_attributes(matcher.road_map.g, attr)
            # add attributes to the matched gdf
            matched_gdf[attr] = matched_gdf["road_id"].map(attr_dict)
            # add attributes to the matched path gdf
            matched_path_gdf[attr] = matched_path_gdf["road_id"].map(attr_dict)
        # Set unmatched_trips to None and add matched_gdf and matched_path_gdf to the trace dictionary
        trace_dict["unmatched_trips"] = None
        trace_dict["matched_gdf"] = matched_gdf
        trace_dict["matched_path_gdf"] = matched_path_gdf

    return trace_dict


def batch_process_traces_parallel(
    log_queue,
    traces,
    processes,
    use_regional_nx_map,
    region_boundary_path,
    local_network_path,
    network_type,
    geofence_buffer,
):
    """Batch process traces using an instance of the LCSSMatcher class in parallel using multiprocessing.

    Args:
        traces (List): list of dictionaries with trip_id and trace.
        matcher (LCSSMatcher): instance of the LCSSMatcher class.
        network_type (Enumerator, optional): Enumerator for Network Types supported by osmnx. Defaults to NetworkType.DRIVE.
        geofence_buffer (int): Buffer distance around trip traces. Defaults to 1000 meters.

    Returns:
        List: List of dictionaries with trip_id, trace, matched_result, matched_gdf, and matched_path_gdf.
        Structure of the list:
        [
            {
            "trip_id": trip_id,
            "trace": trace,
            "unmatched_trips": None or trip_id,
            "trace_gdf": trace_gdf,
            "trace_line_gdf": trace_line_gdf,
            "matched_result": match_result,
            "matched_gdf": matched_gdf,
            "matched_path_gdf": matched_path_gdf,
            },
        ...
        ]
    """

    # -- Run the application -- #
    if processes == 1:
        matched_traces = [
            process_trace(trace_dict, use_regional_nx_map, geofence_buffer, network_type)
            for trace_dict in traces
        ]
    else:
        matched_traces = []
        futures = []
        # process traces in parallel
        executor = ProcessPoolExecutor(
            max_workers=processes,
            initializer=init_worker,
            initargs=(
                log_queue,
                use_regional_nx_map,
                region_boundary_path,
                local_network_path,
                network_type,
            ),
        )
        with executor:
            for trace_dict in traces:
                future = executor.submit(
                    process_trace, trace_dict, use_regional_nx_map, geofence_buffer, network_type
                )
                # logging.debug(f"completed executor.submit; {future=}")
                futures.append(future)

                if len(futures) % 1000 == 0:
                    logging.debug(f"Submitted {len(futures)} traces")
            logging.info(f"Completed submitting {len(futures):,} traces")

            for future in as_completed(futures):
                if len(matched_traces) % 1000 == 0:
                    logging.debug(f"Retrieved {len(matched_traces):,} traces")
                matched_traces.append(future.result())
            logging.info(f"Completed acquiring {len(matched_traces):,} traces")
    return matched_traces


def concatenate_matched_gdfs(matched_traces, match_type="matched_gdf"):
    """Concatenate matched trace geodataframes into a single geodataframe.

    Args:
        matched_traces (List): List of dictionaries with matched trace geodataframes.
        match_type (String, optional): Type of match to concatenate. Defaults to "matched_gdf".
        Options are "matched_gdf", "matched_path_gdf", "trace_gdf".

    Returns:
        GeoDataFrame: Concatenated geodataframe.
    """
    matched_gdfs = []
    for trace_dict in matched_traces:
        # check if the match type is in the trace dictionary
        if match_type not in list(trace_dict.keys()):
            # logging.debug(f"Match type {match_type} not found in trace dictionary. Skipping.")
            continue
        else:
            # logging.debug(f"Match type {match_type} found in trace dictionary.")
            matched_gdfs.append(trace_dict[match_type])

    logging.info(
        f"concatenate_matched_gdfs() for {match_type=} " f"with {len(matched_gdfs)=} matched_gdfs"
    )
    # not sure why this would happen -- return empty geodataframe
    if len(matched_gdfs) == 0:
        return gpd.GeoDataFrame()

    matched_gdf = pd.concat(matched_gdfs)
    logging.debug(f"concatenate_matched_gdfs() for {match_type}: matched_gdf:\n{matched_gdf}")
    logging.debug(f"matched_gdf.dtypes:\n{matched_gdf.dtypes}")

    # if values in the matched_gdf are lists, convert to strings
    for col in matched_gdf.columns:
        if not matched_gdf.dtypes[col] == object:
            continue
        logging.debug(f"Checking column {col} for for list type")

        is_list = matched_gdf[col].apply(lambda x: isinstance(x, list))
        if is_list.any():
            logging.debug(f"list elements:\n{matched_gdf.loc[is_list, :]}")
            # Convert to string if the column is a list or int
            matched_gdf[col] = matched_gdf[col].apply(
                lambda x: str(x) if isinstance(x, list) else str(x) if isinstance(x, int) else x
            )
    return matched_gdf


def write_matched_gdfs(match_result, gpkg_file_path, shapefile_dir=None):
    """Write traces matched with the LCSS matcher to a geopackage.

    Args:
        match_result (List): List of dictionaries with matched trace geodataframes.
        gpkg_gpkg_file_path (String): path to the geopackage file.
    """
    trace_gdf = concatenate_matched_gdfs(match_result, match_type="trace_gdf")
    trace_line_gdf = concatenate_matched_gdfs(match_result, match_type="trace_line_gdf")
    matched_gdf = concatenate_matched_gdfs(match_result, match_type="matched_gdf")
    matched_path_gdf = concatenate_matched_gdfs(match_result, match_type="matched_path_gdf")

    # rename to 10 characters
    #   '1234567890'              :'1234567890'
    SHORT_COL_NAMES = {
        "coordinate_id": "coord_id",
        "distance_to_road": "dist_to_rd",
        "origin_junction_id": "origjuncid",
        "destination_junction_id": "destjuncid",
        "travel_time": "traveltime",
    }

    # write the trace_gdf, trace_line_gdf, matched_gdf, and matched_path_gdf to a geopackage
    if len(trace_gdf) > 0:
        trace_gdf.to_file(gpkg_file_path, layer="trace_gdf", driver="GPKG")
        logging.info(
            f"Wrote {len(trace_gdf):,} rows to {gpkg_file_path} layer trace_gdf "
            f"with columns {trace_gdf.columns.to_list()}"
        )
        # also write shapefile if requested
        if shapefile_dir:
            trace_gdf.to_file(str(shapefile_dir / "trace.shp"))

    if len(trace_line_gdf) > 0:
        trace_line_gdf.to_file(gpkg_file_path, layer="trace_line_gdf", driver="GPKG")
        logging.info(
            f"Wrote {len(trace_line_gdf):,} rows to {gpkg_file_path} layer trace_line_gdf "
            f"with columns {trace_line_gdf.columns.to_list()}"
        )
        # also write shapefile if requested
        if shapefile_dir:
            trace_line_gdf.to_file(str(shapefile_dir / "trace_line.shp"))

    if len(matched_gdf) > 0:
        # convert matched_gdf and matched_path_gdf "road_id" column from RoadId data type to string
        matched_gdf["road_id"] = matched_gdf["road_id"].astype(str)
        matched_path_gdf["road_id"] = matched_path_gdf["road_id"].astype(str)

        matched_gdf.to_file(gpkg_file_path, layer="matched_gdf", driver="GPKG")
        logging.info(
            f"Wrote {len(matched_gdf):,} rows to {gpkg_file_path} layer matched_gdf "
            f"with columns {matched_gdf.columns.to_list()}"
        )
        # also write shapefile if requested
        if shapefile_dir:
            # shorten column names if needed
            for col in SHORT_COL_NAMES.keys():
                if col in matched_gdf.columns.to_list():
                    matched_gdf.rename(columns={col: SHORT_COL_NAMES[col]}, inplace=True)
            logging.debug(f"matched_gdf columns shortened: {matched_gdf.columns.to_list()}")

            matched_gdf.to_file(str(shapefile_dir / "matched.shp"))

    if len(matched_path_gdf) > 0:
        matched_path_gdf.to_file(gpkg_file_path, layer="matched_path_gdf", driver="GPKG")
        logging.info(
            f"Wrote {len(matched_path_gdf):,} rows to {gpkg_file_path} layer matched_path_gdf "
            f"with columns {matched_path_gdf.columns.to_list()}"
        )
        # also write shapefile if requested
        if shapefile_dir:
            # shorten column names if needed
            for col in SHORT_COL_NAMES.keys():
                if col in matched_path_gdf.columns.to_list():
                    matched_path_gdf.rename(columns={col: SHORT_COL_NAMES[col]}, inplace=True)
            logging.debug(
                f"matched_path_gdf columns shortened: {matched_path_gdf.columns.to_list()}"
            )
            matched_path_gdf.to_file(str(shapefile_dir / "matched_path.shp"))


def read_and_merge_data(location_path, trip_path):
    """Read location and trip data and merge them on trip_id

    Args:
        location_path (String): Path to the location csv file.
        trip_path (String): Path to the trip csv file.

    Returns:
        DataFrame: Merged DataFrame with location and trip data.
    """
    location_df = pd.read_csv(location_path)
    trip_df = pd.read_csv(trip_path)
    trip_locations = pd.merge(
        location_df,
        trip_df[
            [
                "trip_id",
                "o_in_region",
                "d_in_region",
                "mode_type",
                "mode_1",
                "mode_2",
                "mode_3",
                "mode_4",
            ]
        ],
        on="trip_id",
    )
    return trip_locations


def filter_trips(trip_locations):
    """Filter trips to include only the following modes:
    5. Taxi
    6. TNC
    8. Car
    9. Carshare
    11. Shuttle/vanpool

    Args:
        trip_locations (DataFrame): DataFrame with location and trip data.

    Returns:
        DataFrame: Filtered DataFrame with trips that meet the criteria.
    """
    car_trips = trip_locations[
        ((trip_locations["mode_type"].isin([5, 6, 8, 9, 11])))
        & (trip_locations["o_in_region"] == 1)
        | (trip_locations["d_in_region"] == 1)
    ]
    return car_trips


def main(script_args):
    """Main function to process trip data and write matched traces to a geopackage.

    Args:
        script_args: argparse.parse_args() return
    Returns:
        None
    """
    global process_regional_nx_map

    # since we're logging to file, we can be verbose
    pd.options.display.width = 500
    pd.options.display.max_columns = 100

    NETWORK_TYPE = NetworkType.DRIVE
    output_dir = config.out_file_path
    gpkg_file_path = config.gpkg_path

    # if test mode, use local dir for output instead of config.out_file_path
    if script_args.test:
        output_dir = pathlib.Path.cwd() / "output"
        output_dir.mkdir(exist_ok=True)
        gpkg_file_path = output_dir / "tds_conflation_results.gpkg"

    # ================= Create logger =================
    # put arguments into log name to make it easier to inspect differences
    log_file_full_path = (pathlib.Path.cwd() if script_args.test else config.gpkg_path) / (
        "trip-trace-conflation"
        + f"_n{script_args.num_trip_ids if script_args.num_trip_ids else 'all'}"
        + f"_p{script_args.processes}"
        + f"_b{script_args.geofence_buffer}.log"
    )
    print(f"Writing to log file {log_file_full_path}")

    logger = logging.getLogger()
    logger.handlers.clear()
    logger.setLevel(logging.DEBUG)
    # console handler
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    ch.setFormatter(
        logging.Formatter(
            "%(asctime)s - %(process)d - %(levelname)s - %(message)s",
            datefmt="%m/%d/%Y %I:%M:%S %p",
        )
    )
    logger.addHandler(ch)
    # file handler
    fh = logging.FileHandler(log_file_full_path, mode="w")
    fh.setLevel(logging.DEBUG)
    fh.setFormatter(
        logging.Formatter(
            "%(asctime)s - %(process)d - %(levelname)s - %(message)s",
            datefmt="%m/%d/%Y %I:%M:%S %p",
        )
    )
    logger.addHandler(fh)

    # Set up the log listener.
    # The log_listener will loop forever (in its own thread), handling log
    # messages as they arrive on the log_queue. See the top-level docstring for
    # more detail on this.
    # Note that we do not need to actually get a Logger object to run this!
    log_queue = multiprocessing.Manager().Queue(-1)
    log_listener = QueueListener(log_queue, fh)

    # Start a background thread that listens for and handles log messages.
    log_listener.start()

    logging.info(f"{args=}")

    # read and merge location and trip data
    logging.info("Reading and merging data...")
    trip_locations = read_and_merge_data(config.location_path, config.trip_path)

    # filter trips
    logging.info("Filtering trips...")
    # get unique trip ids
    car_trips = filter_trips(trip_locations)
    unique_ids = car_trips["trip_id"].unique()
    logging.info(f"Number of unique trip ids: {len(unique_ids):,}")

    # for processes > 1, this is initialized in init_worker
    if (script_args.processes == 1) and (script_args.use_regional_nx_map):
        now = datetime.now()
        logging.info("use_regional_nx_map: Creating networkx map from geojson...")
        logging.info(f"{config.region_boundary_path=}")
        logging.info(f"{config.local_network_path=}")
        logging.info(f"{config.network_type=}")
        process_regional_nx_map = nx_map_from_geojson(
            config.region_boundary_path, local_network_path, network_type
        )
        later = datetime.now()
        logging.info(f"use_regional_nx_map: Creating networkx map took: {later - now}")

    # create a batch of traces
    logging.info("Creating batch traces...")

    if script_args.num_trip_ids:
        # test with a subset of trip ids
        unique_ids = unique_ids[: script_args.num_trip_ids]

    car_trips = car_trips[car_trips["trip_id"].isin(unique_ids)]
    batch_traces = create_batch_traces(car_trips, "trip_id")

    now = datetime.now()
    # process traces in parallel
    logging.info("Processing traces in parallel...")
    matched_traces = batch_process_traces_parallel(
        log_queue,
        batch_traces,
        processes=script_args.processes,
        use_regional_nx_map=script_args.use_regional_nx_map,
        region_boundary_path=config.region_boundary_path,
        local_network_path=config.local_network_path,
        network_type=NETWORK_TYPE,
        geofence_buffer=script_args.geofence_buffer,
    )
    later = datetime.now()
    logging.info(f"Multiprocessing took: {later - now}")

    log_listener.stop()

    # write the matched gdfs to a geopackage
    logging.info("Writing matched gdfs to geopackage...")
    write_matched_gdfs(matched_traces, gpkg_file_path, output_dir)

    # # delete the cache directory
    # cache_dir = "cache"
    # logging.info(f"Deleting cache directory at {cache_dir}...")
    # shutil.rmtree(cache_dir)
    # logging.info("Cache directory deleted.")

    logging.info("Processing complete.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=USAGE, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "--test", help="Run in test mode: output locally instead of to box", action="store_true"
    )
    parser.add_argument("--num_trip_ids", help="Run with a subset of trip ids", type=int)
    parser.add_argument("--processes", help="Number of processes to use", type=int, default=8)
    parser.add_argument(
        "--use_regional_nx_map",
        help="Use a single NxMap instance for the region",
        action="store_true",
        # default to True because otherwise OsMNx will pull down data for every trace and get throttled
        default=True,
    )
    parser.add_argument(
        "--geofence_buffer", help="Buffer around trace to use", type=int, default=1000
    )
    args = parser.parse_args()

    main(script_args=args)
