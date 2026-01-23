import pandas as pd
import numpy as np
import openmatrix as omx
import os
import functools
import subprocess
import gc
from scipy.stats import chi2
import geopandas as gpd
from shapely.geometry import Point

def convert_skim(config):
    print("Current working directory:", os.getcwd())
    if config['convert_skim']:
        # Delete existing .parquet and .omx files in the temp directory to avoid stale files
        if os.path.exists('temp'):
            for file in os.listdir('temp'):
                file_path = os.path.join('temp', file)
                if os.path.isfile(file_path) and (file.endswith('.parquet') or file.endswith('.omx')):
                    os.remove(file_path)
        else:
            os.makedirs('temp')

        # Write your .s script to disk
        with open('temp\\skim_converter.s', 'w') as f:
            f.write('CONVERTMAT FROM="{}\\base\\hnets\\HWYSKMIZ.SKM" TO="{}\\temp\\HWYSKMIZ.omx" FORMAT=OMX COMPRESSION=7\n'.format(config['skim_dir'], os.getcwd()))
            f.write('CONVERTMAT FROM="{}\\base\\transit\\AMTWLOS.DAT" TO="{}\\temp\\AMTWLOS.omx" FORMAT=OMX COMPRESSION=7\n'.format(config['skim_dir'], os.getcwd()))
            f.write('CONVERTMAT FROM="{}\\base\\transit\\MDTWLOS.DAT" TO="{}\\temp\\MDTWLOS.omx" FORMAT=OMX COMPRESSION=7\n'.format(config['skim_dir'], os.getcwd()))
            f.write('CONVERTMAT FROM="{}\\base\\transit\\AMSKM_TPP.BIN" TO="{}\\temp\\AMSKM_TPP.omx" FORMAT=OMX COMPRESSION=7\n'.format(config['skim_dir'], os.getcwd()))
            f.write('CONVERTMAT FROM="{}\\base\\transit\\MDSKM_TPP.BIN" TO="{}\\temp\\MDSKM_TPP.omx" FORMAT=OMX COMPRESSION=7\n'.format(config['skim_dir'], os.getcwd()))
            f.write('CONVERTMAT FROM="{}\\base\\NMT\\NMTIZ.SKM" TO="{}\\temp\\NMTIZ.omx" FORMAT=OMX COMPRESSION=7\n'.format(config['skim_dir'], os.getcwd()))

        # Path to your executable
        executable_path = r"C:\\Program Files\\Citilabs\\CubeVoyager\\VOYAGER.EXE"

        # Run the executable with the script as an argument
        result = subprocess.run([executable_path, 'temp\\skim_converter.s', r'/start'], cwd=os.getcwd(), capture_output=True, text=True)

        # Print output and errors
        print(result.stdout)
        print(result.stderr)
    elif not os.path.exists('temp\\HWYSKMIZ.omx'):   
        raise FileNotFoundError("Skim conversion is enabled but the omx file does not exist.")   
    return

def import_skim(file = None, skim_dict = None, prefix = None):
    
    with omx.open_file(file, 'r') as f:
        for i,matrix_name in enumerate(f.list_matrices()):  # Choose the matrix you want
            mat = f[matrix_name][:]  # Read the matrix as a numpy array

            # If there is a mapping (e.g., 'ZONE'), get the zone labels
            if 'ZONE' in f.list_mappings():
                zones = f.mapping('ZONE')[:]
            else:
                zones = range(mat.shape[0])  # Use indices if no mapping

            df = pd.DataFrame(mat, index=zones, columns=zones)
            df.index.name = 'origin'
            df.columns.name = 'destination'
            df = df.stack().reset_index()
            print("Processing {} matrix:".format(file), matrix_name)
            df.columns = ['origin','destination','{}{}'.format(prefix, skim_dict[matrix_name])]
            if i ==0:
                skm = df
            else:
                skm = skm.merge(df, on = ['origin','destination'], how='outer')
    return skm


def import_all_skims(config):
    """import skims - omx takes 8 mins to read so convert to parquet if they don't already exist and read
    from this format - takes only 1 min"""
    if config['convert_skim']:
        convert_skim(config)
    
    skim_files = [
        ("temp\\HWYSKMIZ.parquet", config['hwy_skim_dict'], ''),
        ("temp\\AMTWLOS.parquet", config['walk_transit_skim_dict'], 'pk_'),
        ("temp\\MDTWLOS.parquet", config['walk_transit_skim_dict'], 'ok_'),
        ("temp\\AMSKM_TPP.parquet", config['drive_transit_skim_dict'], 'pk_'),
        ("temp\\MDSKM_TPP.parquet", config['drive_transit_skim_dict'], 'ok_'),
        ("temp\\nmtiz.parquet", config['NMT_skim_dict'], '')
    ]
    skim_dfs = []
    for file, skim_dict, prefix in skim_files:
        if os.path.exists(file):
            df = pd.read_parquet(file)
        else:
            df = import_skim(file=file.replace('.parquet', '.omx'), skim_dict=skim_dict, prefix=prefix)
            df.to_parquet(file)
        df['OTAZ'] = df['origin'] + 1
        df['DTAZ'] = df['destination'] + 1
        skim_dfs.append(df.drop(columns=['origin', 'destination']))
    merged_skims = functools.reduce(lambda left, right: pd.merge(left, right, on=['OTAZ', 'DTAZ'], how='outer'), skim_dfs)
    del skim_dfs
    gc.collect()
    return merged_skims

def run_likelihood_ratio_test(model_0, model_1):
    # Suppose you have two models:
    # model_0: restricted (simpler)
    # model_1: full (more parameters)

    ll_0 = model_0.loglike()  # log-likelihood of restricted model
    ll_1 = model_1.loglike()  # log-likelihood of full model

    k_0 = len(model_0.parameters.param_name)  # number of parameters in restricted model
    k_1 = len(model_1.parameters.param_name)  # number of parameters in full model

    LR = 2 * (ll_1 - ll_0)
    df = k_1 - k_0
    p_value = chi2.sf(LR, df)

    print(f"Likelihood Ratio Statistic: {LR:.3f}")
    print(f"Degrees of Freedom: {df}")
    print(f"P-value: {p_value:.4g}")
    print(f"Significant at 0.05 level: {p_value < 0.05}")


def map_point_to_reference(
        data,
        reference_gdf,
        point_lat_col,
        point_lon_col,
        reference_field,
        point_field_name=None,
        crs="EPSG:4326"
    ):
    """
    Maps origin and destination points to a reference GeoDataFrame and tags each with the specified field.

    Args:
        reference_gdf (GeoDataFrame): Reference polygons with the field to tag.
        origin_lat_col (str): Column name for origin latitude.
        origin_lon_col (str): Column name for origin longitude.

        reference_field (str): Field in reference_gdf to tag.
        origin_field_name (str, optional): Name for output origin field (default: 'O{reference_field}').
        crs (str): CRS of the latitude/longitude columns (default: "EPSG:4326").

    Returns:
        pandas.DataFrame: Data with new columns for origin and destination tags.
    """


    if point_field_name is None:
        point_field_name = f"O{reference_field}"


    # Create GeoDataFrames for origin and destination points
    origin_points = gpd.GeoDataFrame(
        data.reset_index(),
        geometry=gpd.points_from_xy(data[point_lon_col], data[point_lat_col]),
        crs=crs
    )


    # Reproject to match reference_gdf if needed
    if origin_points.crs != reference_gdf.crs:
        origin_points = origin_points.to_crs(reference_gdf.crs)


    # Spatial join to tag origin and destination
    origin_join = gpd.sjoin(origin_points, reference_gdf[[reference_field, 'geometry']], how='left', predicate='within')

    # Drop duplicates so each point index appears only once (keep first match)
    origin_join = origin_join.drop_duplicates(subset='index')
    
    # Assign tags to the main DataFrame, preserving all original rows and order
    data = data.merge(origin_join[['index', reference_field]].set_index('index').rename(columns = {reference_field:point_field_name}), left_index=True, right_index=True, how='left')
    
    return data

def apply_transformations(data, transform_file, config=None):
    """
    Apply transformations to a DataFrame based on CSV instructions.
    Returns:
    - Transformed DataFrame
    """
    transform_df = pd.read_csv(transform_file)

    for _, row in transform_df.iterrows():
        action = row['action']
        # print("Applying row: ", row.to_dict()   )
        
        if action == 'merge':
            df_to_merge = None
            try:
                df_to_merge = pd.read_csv(row['source'])
            except Exception as e1:
                try:
                    eval_path = eval(row['source'], globals(), {'config': config})
                    df_to_merge = pd.read_csv(eval_path)
                except Exception as e2:
                    print(f"Error: Could not read merge file from '{row['source']}'. Tried direct and eval. Errors: {e1}, {e2}")
                    
            # Select columns if specified
            if 'merge_columns' in row and pd.notnull(row['merge_columns']):
                cols = [col.strip() for col in row['merge_columns'].split(';')]
                df_to_merge = df_to_merge[cols]
            # Rename columns if specified
            if 'rename_columns' in row and pd.notnull(row['rename_columns']):
                rename_map = dict(pair.split(':') for pair in row['rename_columns'].split(';'))
                df_to_merge = df_to_merge.rename(columns=rename_map)
            data = data.merge(df_to_merge, how=row['merge_how'], on=row['merge_on'])
        elif action == 'conditional_assign':
            # Coerce any 'True'/'False' string columns to boolean before evaluating condition
            for col in data.columns:
                if data[col].dtype == object and data[col].isin(['True', 'False']).any():
                    data[col] = data[col].map({'True': True, 'False': False})

            eval_globals = {"np": np, "pd": pd, "_extract_number": _extract_number, "_get_half_hour_bin": _get_half_hour_bin, "_get_half_hour_bin_label": _get_half_hour_bin_label}
            eval_locals = {'df': data, 'config': config}
            try:
                condition = eval(row['condition'], eval_globals, eval_locals)
            except Exception as e:
                print(f"Error evaluating condition: {row['condition']}")
                print(f"Transformation row: {row.to_dict()}")
                raise
            val = row['value']
            # Assign 'True'/'False' as booleans, handle quoted strings, otherwise eval
            if isinstance(val, str):
                v = val.strip()
                if v == 'True':
                    value = True
                elif v == 'False':
                    value = False
                # If value is quoted, strip quotes and assign directly
                elif (v.startswith('"') and v.endswith('"')) or (v.startswith("'") and v.endswith("'")):
                    value = v[1:-1]
                # Assign directly if value is a string and does not look like a Python expression
                elif not (v.startswith(("[", "{", "(", "np.", "pd.", "config", "df[")) or any(c in v for c in "=()[]{}+*/-<>&|%$^~") or v.isdigit()):
                    value = v
                else:
                    value = eval(v, eval_globals, eval_locals)
            else:
                value = val

            # Handle scalar values vs. array values
            if np.isscalar(value):
                data.loc[condition, row['target']] = value
            else:
                data.loc[condition, row['target']] = value[condition]

        elif action == 'assign':
            eval_globals = {"np": np, "pd": pd, "_extract_number": _extract_number, "_get_half_hour_bin": _get_half_hour_bin, "_get_half_hour_bin_label": _get_half_hour_bin_label}
            eval_locals = {'df': data, 'config': config}
            value = eval(row['value'], eval_globals, eval_locals)
            data[row['target']] = value

        elif action == 'drop_columns':
            cols = [col.strip() for col in row['target'].split(';')]
            data = data.drop(columns=cols, errors='ignore')
    return data

def _extract_number(s):
    """Extract number from string value"""
    import re
    match = re.search(r'[-+]?\d*\.?\d+', str(s))
    return float(match.group()) if match else None

def _get_half_hour_bin(hour, minute):
    """
    Returns the half-hour bin (1-48) for a given hour (0-23) and minute (0-59).
    Bin 1 starts at 3:00am, bin 48 ends at 2:59am next day.
    """
    # Convert to minutes since midnight
    total_minutes = hour * 60 + minute
    # Minutes since 3:00am
    minutes_since_3am = (total_minutes - 180) % 1440  # 180 = 3*60
    # Bin index (0-47), add 1 for bin label (1-48)
    bin_num = (minutes_since_3am // 30) + 1
    return bin_num

def _get_half_hour_bin_label(hour, minute):
    """
    Returns the start time of the half-hour bin as a string (e.g., '3:00am') for a given hour and minute.
    Bin 1 starts at 3:00am, bin 48 at 2:30am next day.
    """
    # Minutes since 3:00am
    total_minutes = hour * 60 + minute
    minutes_since_3am = (total_minutes - 180) % 1440
    bin_start_minutes = (minutes_since_3am // 30) * 30 + 180
    bin_start_minutes = bin_start_minutes % 1440
    bin_hour = bin_start_minutes // 60
    bin_minute = bin_start_minutes % 60
    suffix = 'am' if bin_hour < 12 or bin_hour == 24 else 'pm'
    display_hour = bin_hour if 1 <= bin_hour <= 12 else (bin_hour - 12 if bin_hour > 12 else 12)
    if bin_hour == 0:
        display_hour = 12
    return f"{display_hour}:{bin_minute:02d}{suffix}"

def compare_models(ref, ref_model, model_list, name_list= [0,1,2,3,4,5,6], ivt = 'coef_ivt'):
    compare = ref_model.summary().data.add_suffix(f"_{name_list[0]}")
    for i,model in enumerate(model_list):
        compare = compare.merge(model.summary().data.add_suffix(f"_{name_list[i+1]}"), how = 'outer', left_index = True, right_index = True)

    compare = compare.merge(ref, how = 'left', left_index = True, right_index = True, suffixes = ['','_ref'])

    for i,model in enumerate([ref_model] + model_list):
        compare.loc[('Model_Fit', 'log_likelihood'), f'Value_{name_list[i]}'] = model.model.loglike()
        compare.loc[('Model_Fit', 'Value of Time $/hr'), f'Value_{name_list[i]}'] = model.model.pf.loc[ivt].value/model.model.pf.loc['coef_cost'].value*60
    try:
        compare.loc[('Model_Fit', 'Value of Time $/hr'), f'Value'] = ref.reset_index()[ref.reset_index().Parameter == ivt]['Value'].item()/ref.reset_index()[ref.reset_index().Parameter == 'coef_cost']['Value'].item()*.60
    except Exception as e:
        print(f"Error calculating 'Value of Time $/hr': {e}")
    return compare[[col for col in compare if col.startswith('Value')  or 'Sig' in col]].rename(columns = {'Value':'Reference Value'})