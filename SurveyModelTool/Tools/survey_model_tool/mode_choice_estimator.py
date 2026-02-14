import larch as lx
import pandas as pd
import yaml
from larch import P, X, PX
import ast
import re
import numpy as np
import openmatrix as omx
import geopandas as gpd
import os
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.metrics import confusion_matrix
from . import utilities


class ModeChoiceEstimator:
    def __init__(self, data, config_path, spec_sheet=None):
        self.data = data.copy()
        self.config = self._load_config(config_path)
        self.model = None
        self.dataset = None
        self.alt_codes = {}
        self.idco_utility_functions = {}
        self.idca_utility_function = 'None'
        self.comparison = None
        self.spec_sheet = spec_sheet
        self._eval_cache = {}

    def _load_config(self, config_path):
        # Accepts a single path or a list of paths
        if isinstance(config_path, (list, tuple)):
            merged_config = {}
            for path in config_path:
                with open(path, 'r') as f:
                    config = yaml.safe_load(f)
                    merged_config.update(config)
            return merged_config
        else:
            with open(config_path, 'r') as f:
                return yaml.safe_load(f)

    

    def _read_apterm(self):
        los = pd.read_fwf(self.config['zone_los'],  header = None, widths = [8 for i in range(1,19)])
        los = los.rename(columns = {0:'ZONE', 1:'PCOST', 2:'PTERM', 3:'ATERM'})
        return los
    



    def _create_utility_functions(self, alternatives, alt_codes):
        util_spec_path = self.config['spec']
        if self.spec_sheet is not None:
            util_sheet = self.spec_sheet
        else:
            util_sheet = self.config.get('spec_sheet', None)
        if util_spec_path is not None:
            ext = os.path.splitext(util_spec_path)[1].lower()
            if ext in ['.xls', '.xlsx']:
                util_df = pd.read_excel(util_spec_path, sheet_name=util_sheet if util_sheet else 0)
            elif ext == '.csv':
                if util_sheet is not None:
                    print("Warning: 'spec_sheet' is ignored for CSV files.", util_sheet)
                util_df = pd.read_csv(util_spec_path)
            else:
                raise ValueError(f"Unsupported spec file type: {ext}")
            util_df = util_df.set_index('VARIABLE')

            # 1. Identify IDCO (case-level) variables
            idco_vars = []
            for var in util_df.index:
                if var.upper() == 'CONSTANT':
                    continue
                # Check if variable is constant across alternatives for each case_id
                nunique = self.data.groupby('case_id')[var].nunique()
                if (nunique <= 1).all():
                    idco_vars.append(var)
            # 2. Build utility functions
            idca_terms = set()
            for alt in alternatives:
                terms = []
                for var, coef in util_df[alt_codes[alt]].dropna().items():
                    if coef == '' or pd.isna(coef):
                        continue
                    if var.upper() == 'CONSTANT':
                        terms.append(f'P("{coef}")')
                    elif var in idco_vars:
                        terms.append(f'P("{coef}") * X("{var}")')
                    else:
                        idca_terms.add(f'PX("{var}")')
                self.idco_utility_functions[alt] = " + ".join(terms) if terms else "0"
            self.idca_utility_function = " + ".join(idca_terms) if idca_terms else "0"
        return

    def _compute_utility(self, row, terms, param_values):
        utility = 0
        for term in terms:
            param_name = term['param'].strip()
            data_name = term['data']
            param_value = param_values['value'].get(param_name, 1) if param_name else 1
            data_value = 1 if data_name == '1' else row[data_name]
            utility += param_value * data_value
        return utility

 
    def _reverse_lookup(self, d, value):
        for k, v in d.items():
            if v == value:
                return k
        raise ValueError(f"Value {value} not found in dictionary.")

    def _safe_eval(self, expr, row, config):
        cache_key = expr
        if cache_key not in self._eval_cache:
            # Replace config keys in the expression with their values
            for key, value in config.items():
                if isinstance(value, (int, float)):
                    expr = expr.replace(key, str(value))
            self._eval_cache[cache_key] = compile(expr, '<string>', 'eval')
        return eval(self._eval_cache[cache_key], {}, row.to_dict())
    
    def _create_comparison(self):
        if self.model is None:
            raise ValueError("Model must be estimated before applying.")


        # Predict probabilities for each alternative
        probs = pd.DataFrame(self.model.probability())

        # For each case, sample an alternative according to the predicted probabilities, 5 times, and take the mode
        def sample_choice_mode(row):
            samples = [np.random.choice(row.index, p=row.values) for _ in range(5)]
            # Compute the mode (most common value); if tie, pick the first
            return pd.Series(samples).mode().iloc[0]

        pred = pd.DataFrame(probs.apply(sample_choice_mode, axis=1), columns=['predicted'])
        pred['predicted'] = pred['predicted'].map(lambda x: self.alt_codes[x + 1])
        pred['case_id'] = self.data[self.data.chose ==1]['case_id'].values

        # Build comparison DataFrame
        comparison = self.data[
            (self.data['chose'] == 1) ][
            ['case_id', 'alternative']].merge(pred, how='left', on='case_id')
        comparison['match'] = comparison['alternative'] == comparison['predicted']
        self.comparison = comparison
        return

    def prepare_data(self):
        """Function to prepare data for mode choice model estimation."""
        self.data["{}_final".format(self.config['choice_col'])] = self.data[self.config['choice_col']].map(self.config['survey_to_spec'])
        # cases = len(self.dat)
        # if len(self.data[self.config['choice_col']].unique()) == 1:
        #     raise ValueError("3. Only one choice in the data. Please check your data.")
        # Extract variable names from utility functions using AST
        trip_len = len(self.data)
        if self.spec_sheet is not None:
            util_sheet = self.spec_sheet
        else:
            util_sheet = self.config.get('spec_sheet', None)
        util_spec_path = self.config['spec']
        if util_spec_path is not None:
            ext = os.path.splitext(util_spec_path)[1].lower()
            if ext in ['.xls', '.xlsx']:
                util_df = pd.read_excel(util_spec_path, sheet_name=util_sheet if util_sheet else 0)
            elif ext == '.csv':
                if util_sheet is not None:
                    print("Warning: 'spec_sheet' is ignored for CSV files.", util_sheet)
                util_df = pd.read_csv(util_spec_path)
            else:
                raise ValueError(f"Unsupported spec file type: {ext}")
            util_df = util_df.set_index('VARIABLE')
            # Collect all variable names (except CONSTANT) that are used in at least one alternative
            used_vars = set(util_df.index) - {"CONSTANT"}
        else:
            raise ValueError("No utility specification file provided in the conig.")

        los = self._read_apterm()
        if len(los) > len(los.ZONE.unique()):
            raise ValueError(f"LOS file has duplicate zones: expected {len(los.ZONE.unique())} rows, got {len(los)}")
        self.data = self.data.merge(los[['ZONE','PTERM']].rename(columns = {'ZONE':'OTAZ','PTERM':'PTERM'}), how = 'left', on = 'OTAZ')
        self.data = self.data.merge(los[['ZONE','ATERM']].rename(columns = {'ZONE':'DTAZ','PTERM':'ATERM'}), how = 'left', on = 'DTAZ')
        self.data = self.data.merge(los[['ZONE','PCOST']].rename(columns = {'ZONE':'DTAZ','PCOST':'parking_cost'}), how = 'left', on = 'DTAZ')
        del los
        self.data['ATERM'] = self.data['ATERM']/100
        self.data['PTERM'] = self.data['PTERM']/100
        self.data['parking_cost'] = self.data['parking_cost']/100
        self.data['total_parking_cost'] = self.data['parking_cost'] * self.data['dwell_mins']/60 #convert cents/min to dollars/hr
        if len(self.data) != trip_len:
            raise ValueError(f"Geocoding APTerm failed: expected {trip_len} rows, got {len(self.data)}")#geocode special destinations

        #format data into IDCA
        # if len(self.data[self.config['choice_col']].unique()) == 1:
        #     raise ValueError("4. Only one choice in the data. Please check your data.")
        # self.data['case_id'] = self.data.reset_index().index + 1
        alternatives = self.config['mode_choice_model_spec']['alternatives']
        self.data = self.data[self.data['{}_final'.format(self.config['choice_col'])].isin(alternatives)]
        
        
        self.data['case_id'] = self.data.reset_index().index + 1
        # alternatives = self.config['mode_choice_model_spec']['alternatives']

        # Create a DataFrame with all combinations of case_id and alternatives
        case_ids = self.data['case_id'].unique()
        all_combinations = pd.MultiIndex.from_product([case_ids, alternatives], names=['case_id', 'alternative']).to_frame(index=False)

        # Merge the original data with the all_combinations DataFrame
        self.data = all_combinations.merge(self.data, on=['case_id'], how='left')
        
        self.data.reset_index(drop=True, inplace=True)
        self.data['chose'] = self.data['alternative'] == self.data['{}_final'.format(self.config['choice_col'])]
        self.data['chose'] = self.data['chose'].astype(int)
        merged_skims = utilities.import_all_skims(self.config)
        self.data = self.data.merge(merged_skims, on=['OTAZ', 'DTAZ'], how='left')
        # if len(self.data['{}_final'.format(self.config['choice_col'])].unique()) == 1:
        #     raise ValueError("2. Only one choice in the data. Please check your data.")
        skim_map = pd.read_csv(self.config['skim_map'])
        skim_map['alternative'] = skim_map[self.config['choice_col']].map(self.config['survey_to_spec'])
        skim_map = skim_map.drop_duplicates(subset=['alternative','period']).drop(self.config['choice_col'], axis=1)

        
        # If depart_datetime is not already datetime type:
        self.data['depart_datetime'] = pd.to_datetime(self.data['depart_datetime'])
        self.data = self.data[~(self.data.OTAZ.isna())]
        self.data = self.data[~(self.data.DTAZ.isna())]
        # Extract the hour
        self.data['depart_hour'] = self.data['depart_datetime'].dt.hour
        self.data['period'] = 'offpeak'
        self.data.loc[self.data['depart_hour'].isin(self.config['peak_hours']), 'period'] = 'peak'
        
        
        self.data = self.data.merge(skim_map, on = ['alternative','period'], how = 'left')
        # Evaluate availability for each row if 'availability' column exists
        if 'availability' in self.data.columns:
            # This will create a boolean column 'available' based on the expression in 'availability'
            self.data['available'] = self.data.apply(
                lambda row: pd.eval(row['availability'], local_dict=row.to_dict()) if pd.notnull(row['availability']) else True,
                axis=1
            )
        else:
            self.data['available'] = True  # Default: all available

        for attr in ['dist','time', 'cost', 'wait', 'xfer', 'wlktm','drtm','pnr_node']:
            col_name = f'skim_{attr}'
            conversion_col = f'conversion_{attr}'
            debug_col = f'debug_{attr}'
            if conversion_col in self.data.columns:
                def _debug_eval(row):
                    try:
                        if pd.notnull(row[attr]) and pd.notnull(row[conversion_col]) and str(row[conversion_col]).strip() != '':
                            expr = f"({row[attr]}) {row[conversion_col]}"
                            result = self._safe_eval(expr, row, self.config)
                            debug = {'expr': expr, 'conversion': row[conversion_col], 'result': result}
                            return result, str(debug)
                        elif pd.notnull(row[attr]) and str(row[attr]).strip() != '':
                            expr = str(row[attr])
                            result = self._safe_eval(expr, row, self.config)
                            debug = {'expr': expr, 'conversion': None, 'result': result}
                            return result, str(debug)
                        else:
                            return np.nan, str({'expr': None, 'conversion': None, 'result': np.nan})
                    except Exception as e:
                        return np.nan, str({'expr': expr, 'conversion': row.get(conversion_col, None), 'error': str(e)})
                results = self.data.apply(_debug_eval, axis=1)
                # Always assign the converted value to skim_cost
                self.data[col_name] = results.apply(lambda x: x[0])
                self.data[debug_col] = results.apply(lambda x: x[1])
            else:
                    def _debug_eval_no_conv(row):
                        try:
                            if pd.notnull(row[attr]) and str(row[attr]).strip() != '':
                                expr = str(row[attr])
                                result = eval(expr, {}, row.to_dict())
                                debug = {'expr': expr, 'conversion': None, 'result': result}
                                return result, str(debug)
                            else:
                                return np.nan, str({'expr': None, 'conversion': None, 'result': np.nan})
                        except Exception as e:
                            return np.nan, str({'expr': expr, 'conversion': None, 'error': str(e)})
                    results = self.data.apply(_debug_eval_no_conv, axis=1)
                    self.data[col_name] = results.apply(lambda x: x[0])
                    self.data[debug_col] = results.apply(lambda x: x[1])
        
        # print(self.data[self.data.trip_id ==2300470601052][['alternative','available','OTAZ','DTAZ','total_parking_cost']+[col for col in self.data.columns if 'skim_cost' in col]])
        # print("Using utility method")
        self.data = utilities.apply_transformations(self.data, self.config['data_transforms_file'], self.config)
        
       
        # Remove cases where chosen alternative is not available
        bad_cases = self.data[~(self.data.available) & (self.data.chose ==1)].case_id.unique()
        self.data = self.data[~self.data.case_id.isin(bad_cases)]      

        # For each used column, if it's object or string dtype, recode as categorical codes
        for col in used_vars:
            if col in self.data.columns and col != self.config['choice_col']:
                if self.data[col].dtype == object or pd.api.types.is_string_dtype(self.data[col]):
                    try:
                        self.data[col] = self.data[col].astype(np.float64)
                    except:
                        self.data[col] = self.data[col].astype('category').cat.codes

        # Optionally, reset index after cleaning
        self.data = self.data.reset_index(drop=True)

        # Scale weights for modeling
        self.data['scaled_weight'] = self.data[self.config['weight_col']] / self.data[self.config['weight_col']].mean()

        # Build list of columns for modeling (used variables + identifiers)
        cols = [col for col in used_vars ] + ['case_id','alternative','chose', 'available','scaled_weight', self.config['weight_col'], self.config['choice_col'], 'trip_id']


        # Create wide-format skim table for each case and alternative
        skim_table = self.data[[col for col in self.data.columns if col.startswith('skim_')] + ['case_id','alternative']].copy()
        skim_table = skim_table.set_index(['case_id','alternative']).unstack()
        skim_table.columns = ['{}_{}'.format(str(a), str(b)) for a, b in skim_table.columns]

        # Merge wide skim table back to main data
        self.data = self.data.merge(skim_table.reset_index(), how = 'left', on = 'case_id')
        del skim_table
        # Print missing columns if any
        missing = [col for col in used_vars if col not in self.data.columns]
        if missing:
            print("Missing columns before dataset creation:", missing)
        # self.data = self.data[cols]
      
        
        return

    
    def specify_model(self):
        #format dataset for larch model
        dataset = lx.Dataset.dc.from_idca(self.data.set_index(['case_id','alternative']))
        self.dataset = dataset
        spec = self.config.get("mode_choice_model_spec", {})
        alternatives = self.dataset['alternative'].values
        alt_names = self.dataset['alt_names'].values

        self.alt_codes = dict(zip(alternatives, alt_names))
        # alternatives = spec['alternatives']
        nests = spec.get("nests", {})
        model = lx.Model(self.dataset)
        model.choice_ca_var = "chose"
        order = self.config['ordering']
        order = tuple(tuple(group) for group in order)
        model.ordering = order
        model.availability_ca_var = "available"
        # model.weight_co_var = "scaled_weight"
        self._create_utility_functions(alternatives,self.alt_codes)

        for alt in alternatives:
            util_expr = self.idco_utility_functions.get(alt, "0")
            model.utility_co[alt] = eval(util_expr, {"P": P, "X": X})
        model.utility_ca = eval(self.idca_utility_function, {"PX": PX})
        nest_nodes = {}
        try:
            for nest_name, nest_info in nests.items():
                children = []
                for alt in nest_info['alternatives']:
                    if alt in alt_names:
                        children.append(self._reverse_lookup(self.alt_codes, alt))
                    elif alt in nest_nodes:
                        children.append(nest_nodes[alt])
                    else:
                        raise ValueError(f"Unknown alternative or nest: {alt}")
                node = model.graph.new_node(
                    parameter=f"mu_{nest_name.lower()}",
                    children=children,
                    name=nest_name
                )
                nest_nodes[nest_name] = node
        except:
            raise ValueError("Error in specifying nests. Please check the configuration and specify lowest nests first.")
        # model.availability_co_vars = { int(i): "time > 0" for i in self.alt_codes.keys() }
        self.model = model
        return 

    def estimate(self, method='SLSQP', maxiter=250):
        if self.model is None:
            self.specify_model()
        self.model.maximize_loglike(method=method, maxiter=maxiter)

    def summary(self):
        if self.model is not None:
            self.model.calculate_parameter_covariance()
            return self.model.parameter_summary()
        return None

    def confusion_matrix(self):
        """
        Evaluate the estimated model to compare actual vs predicted choices.
        
        """
            # Suppose y_true are actual choices, y_pred are predicted choices
        if self.comparison is None:
            self._create_comparison()

        comparison = self.comparison.copy()

        y_true = comparison['alternative'].tolist()
        y_pred = comparison['predicted'].tolist()
        code_to_label = self.alt_codes
        labels = list(code_to_label.values())

        cm = confusion_matrix(y_true, y_pred, labels=labels)

        plt.figure(figsize=(8, 6))
        sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', xticklabels=labels, yticklabels=labels)
        plt.xlabel('Predicted')
        plt.ylabel('Actual')
        plt.title('Confusion Matrix')
        plt.show()

        return

    def choice_barplot(self):
        """
        Evaluate the estimated model to compare actual vs predicted choices using barplot.

        """
        if self.comparison is None:
            self._create_comparison()

        comparison = self.comparison.copy()

        y_true = comparison['alternative'].tolist()
        y_pred = comparison['predicted'].tolist()
        actual_counts = pd.Series(y_true).value_counts().sort_index()
        pred_counts = pd.Series(y_pred).value_counts().sort_index()

        df = pd.DataFrame({'Actual': actual_counts, 'Predicted': pred_counts}).fillna(0)
        df.plot(kind='bar')
        plt.xlabel('Class/Alternative')
        plt.ylabel('Count')
        plt.title('Actual vs. Predicted Counts')
        plt.show()
        return

    def compute_utilities(self):
        """return dataframe of computed utilities"""
        if self.model is None:
            raise ValueError("Model must be estimated before applying.")
        utils = self.data.copy()
        param_values = self.model.pf[['value']].to_dict()
        for i in self.alt_codes.keys():
            lf_ca = self.model.utility_ca
            lf_co = self.model.utility_co[i]
            terms = []
            for term in lf_co:
                terms.append(term.to_dict())
            for term in lf_ca:
                terms.append(term.to_dict())
            mask = utils['alternative'] == self.alt_codes[i]
            utils.loc[mask, 'util'] = utils.loc[mask].apply(
                self._compute_utility, axis=1, args=(terms, param_values)
            )



      

        return utils
