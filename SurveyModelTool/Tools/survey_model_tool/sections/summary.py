# surveyplotter/sections/mode_share.py
import yaml

class SummarySectionBuilder:
    def __init__(self, plotter, survey, config, map = None):
        self.survey = survey
        self.map = map
        self.plotter = plotter
        # If config is a path, load the YAML
        if isinstance(config, str):
            with open(config, 'r') as f:
                self.config = yaml.safe_load(f)
        else:
            self.config = config

    def build(self, ):
        """
        build mode share summary tab
        """


        # --- Figures (direct translation from your notebook) ---
        if self.map is None:
            overview_column = [[(self.config['sections']['overview']['title'], True)],
                               self.plotter.summary_bar(self.survey, weighted = False),
                                 self.plotter.summary_bar(self.survey, weighted = True),
                                 [("Overview",True)],
                               [self.config['sections']['overview']['text']['overall']],
                             
                           ]
        else:
            overview_column = [[(self.config['sections']['overview']['title'], True)],
                               self.plotter.summary_bar(self.survey, weighted = False),
                                 self.plotter.summary_bar(self.survey, weighted = True),
                                 [("Overview",True)],
                               [self.config['sections']['overview']['text']['overall']],
                               [(self.config['sections']['overview']['map_title'],True)],
                               [self.map],
                           ]
        
        return overview_column