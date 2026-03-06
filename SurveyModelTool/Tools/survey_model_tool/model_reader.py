from . import utilities

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

    def summarize_mode_choice(self):
        utilities.convert_mode_choice(self.config)
        
        pass