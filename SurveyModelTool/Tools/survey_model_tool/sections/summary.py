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
        hh_by_income = None
        hh_by_vehicles = None
        hh_by_size = None
        hh_by_workers = None
        pop_by_ptype = None
        pop_by_age = None

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


        try:
            hh_by_income = self.plotter.plot_share_hue(
                df=self.survey.hh, group_cols=[self.config['global']['columns']['income_label']], share_is_bin=False,
                axis_order=self.config['global']['orders']['income_order'], weight_col=self.config['global']['weights']['hh'],
                x_label='Household Income', plt_title='Household Distribution by Income',                
            )
        except Exception as e:
            print(f"Skipping hh_by_income due to error: {e}")
        try:            hh_by_vehicles = self.plotter.plot_share_hue(
                df=self.survey.hh, group_cols=[self.config['global']['columns']['auto_own']], share_is_bin=False,
                axis_order=self.config['global']['orders']['auto_own_order'], weight_col=self.config['global']['weights']['hh'],
                x_label='Number of Vehicles', plt_title='Household Distribution by Number of Vehicles',                
            )
        except Exception as e:
            print(f"Skipping hh_by_vehicles due to error: {e}")

        try:            hh_by_size = self.plotter.plot_share_hue(
                df=self.survey.hh, group_cols=[self.config['global']['columns']['auto_own']], share_is_bin=False,
                axis_order=self.config['global']['orders']['hhsize_order'], weight_col=self.config['global']['weights']['hh'],
                x_label='Household Size', plt_title='Household Distribution by Size',                
            )
        except Exception as e:
            print(f"Skipping hh_by_size due to error: {e}")
        
        
        try:
            hh_summary = self.config['sections']['overview']['text']['hh_summary_1']
        except:
            hh_summary = "Household Characteristics"

        hh_row_1 = [h for h in [hh_by_income, hh_by_vehicles] if h is not None]
        if hh_row_1:
            overview_column.append([hh_summary])
            overview_column.append(hh_row_1)
        


        return overview_column