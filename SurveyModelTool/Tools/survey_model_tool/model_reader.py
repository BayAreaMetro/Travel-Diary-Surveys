import pandas as pd
import numpy as np
from . import utilities
import yaml
import os

class ModelReader:
    def __init__(self, config_path):
        self.config = self._load_config(config_path)
    
    
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


    # ...existing code...
    # ...existing code...
    def summarize_mode_choice(self, out_dir=None, max_workers=8,convert_cube_files = True):
        import os
        import concurrent.futures
        import numpy as np

        if convert_cube_files:
            utilities.convert_mode_choice(self.config)
        # purps = [file.split('MC')[0] for file in self.config['mode_choice_files']]
        purps = list(dict.fromkeys(file.split('MC')[0] for file in self.config['mode_choice_files']))
        print(purps)
        out_dir = out_dir or r"temp\mode_choice\chunks"
        os.makedirs(out_dir, exist_ok=True)

        specs = []
        for purp in purps:
            if purp == 'HBW':
                for i in range(1, 5):
                    specs.append(("PK", rf"temp\mode_choice\HBWMC{i}PK.omx", purp, i))
                    specs.append(("OP", rf"temp\mode_choice\HBWMC{i}OP.omx", purp, i))
            elif purp in ['HBCOLL', 'HBGS']:
                specs.append(("PK", rf"temp\mode_choice\{purp}MCPK.omx", purp, None))
         
            else:
                specs.append(("PK", rf"temp\mode_choice\{purp}MCPK.omx", purp, None))
                specs.append(("OP", rf"temp\mode_choice\{purp}MCOPK.omx", purp, None))

        def _read_and_process(spec):
            period, path, purp, income = spec
            df = utilities.import_skim(file=path)
            df['income_bin'] = income if income is not None else np.nan
            df['purp'] = purp
            df['origin'] = df['origin'] + 1
            df['destination'] = df['destination'] + 1
            df['period'] = period
            return df

        out_paths = []
        # ThreadPoolExecutor usually helps for IO-bound import_skim; switch to ProcessPoolExecutor if CPU-bound.
        with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as ex:
            for idx, df in enumerate(ex.map(_read_and_process, specs), start=1):
                out_path = os.path.join(out_dir, f"chunk_{idx:04d}.parquet")
                # requires pyarrow or fastparquet
                df.to_parquet(out_path, index=False)
                out_paths.append(out_path)
                del df

        # return list of chunk files (fast to read later with pyarrow/dask)
        return out_paths

    def read_whhaown(self):
        df = pd.read_csv(os.path.join(self.config['model_dir'],self.config['whhaown']), delim_whitespace=True)

        return df