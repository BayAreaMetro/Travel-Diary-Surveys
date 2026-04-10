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

        # Initialize plots to None
        summary_bar_unweighted = None
        summary_bar_weighted = None

        # --- Figures (with error handling) ---
        try:
            summary_bar_unweighted = self.plotter.summary_bar(self.survey, weighted=False)
        except Exception as e:
            print(f"Skipping summary_bar_unweighted due to error: {e}")

        try:
            summary_bar_weighted = self.plotter.summary_bar(self.survey, weighted=True, trip_weight = self.config['global']['weights']['trip'], hh_weight = self.config['global']['weights']['hh'], person_weight = self.config['global']['weights']['person'], tour_weight = self.config['global']['weights']['tour'])
        except Exception as e:
            print(f"Skipping summary_bar_weighted due to error: {e}")

        # --- Layout ---
  
        try:
            title_txt = self.config['sections']['overview']['title']
        except:
            title_txt = "Overview"

        overview_column = [[(title_txt, True)]]

        # Add summary bars if available
        if summary_bar_unweighted is not None:
            overview_column.append(summary_bar_unweighted)
        if summary_bar_weighted is not None:
            overview_column.append(summary_bar_weighted)

        # Overview section
        try:
            overview_txt = self.config['sections']['overview']['text']['overall']
        except:
            overview_txt = "Overview"
        overview_column.append([overview_txt])

        # Map section if available
        if self.map is not None:
            try:
                map_title = self.config['sections']['overview']['map_title']
            except:
                map_title = "Map"
            overview_column.append([(map_title, True)])
            overview_column.append([self.map])

        return overview_column