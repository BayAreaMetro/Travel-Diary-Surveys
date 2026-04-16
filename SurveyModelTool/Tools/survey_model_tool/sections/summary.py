# surveyplotter/sections/mode_share.py
import yaml
import pandas as pd
class SummarySectionBuilder:
    def __init__(self, plotter = None, survey = None, config = None, map = None, survey2 = None):
        self.survey = survey
        self.survey2 = survey2
        self.map = map
        self.plotter = plotter
        self.compare_order = ['', '']

        # If config is a path, load the YAML
        if isinstance(config, str):
            with open(config, 'r') as f:
                self.config = yaml.safe_load(f)
        else:
            self.config = config
        self.compare = self.config['global'].get('compare', False)
        if self.compare and self.survey2 is None:
            raise ValueError("Comparison is enabled in config but survey2 is not provided.")
        if self.compare:
            self.compare_col = self.config['global'].get('compare_col', None)
            self.compare_order = [self.survey.trips[self.compare_col].iloc[0], self.survey2.trips[self.compare_col].iloc[0]]
            self.survey.hh[self.compare_col] = self.compare_order[0]
            self.survey2.hh[self.compare_col] = self.compare_order[1]
            self.survey.person[self.compare_col] = self.compare_order[0]
            self.survey2.person[self.compare_col] = self.compare_order[1]
            
    def build(self, ):
        """
        build mode share summary tab
        """

        # Initialize plots to None
        summary_bar_unweighted = None
        summary_bar_weighted = None
        summary_bar2_unweighted = None
        summary_bar2_weighted = None
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
        if self.survey2 is not None and self.compare:
            try:
                summary_bar2_unweighted = self.plotter.summary_bar(self.survey2, weighted=False)
            except Exception as e:
                print(f"Skipping summary_bar2_unweighted due to error: {e}")

        try:
            summary_bar_weighted = self.plotter.summary_bar(self.survey, weighted=True, trip_weight = self.config['global']['weights']['trip'], hh_weight = self.config['global']['weights']['hh'], person_weight = self.config['global']['weights']['person'], tour_weight = self.config['global']['weights']['tour'])
        except Exception as e:
            print(f"Skipping summary_bar_weighted due to error: {e}")
        if self.survey2 is not None:
            try:
                summary_bar2_weighted = self.plotter.summary_bar(self.survey2, weighted=True, trip_weight = self.config['global']['weights']['trip'], hh_weight = self.config['global']['weights']['hh'], person_weight = self.config['global']['weights']['person'], tour_weight = self.config['global']['weights']['tour'])
            except Exception as e:
                print(f"Skipping summary_bar2_weighted due to error: {e}")

        # --- Layout ---
  
        try:
            title_txt = self.config['sections']['overview']['title']
        except:
            title_txt = "Overview"

        overview_column = [[(title_txt, True)]]
        if self.compare:
            overview_column.append([self.compare_order[0]])
        # Add summary bars if available
        if summary_bar_unweighted is not None:
            overview_column.append(summary_bar_unweighted)
        if summary_bar_weighted is not None:
            overview_column.append(summary_bar_weighted)
        
        if self.compare and summary_bar2_unweighted is not None:
            overview_column.append([self.compare_order[1]])
            overview_column.append(summary_bar2_unweighted)
        if self.compare and summary_bar2_weighted is not None:
            overview_column.append(summary_bar2_weighted)
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

        if self.compare:
            try:
                hh_by_income = self.plotter.plot_share_hue(
                    df=pd.concat([s.hh[s.hh[self.config['global']['columns']['income_label']].isin(self.config['global']['orders']['income_order'])] for s in [self.survey, self.survey2] if s is not None], ignore_index=True), group_col=self.config['global']['columns']['income_label'],
                    axis_order=self.config['global']['orders']['income_order'], weight_col=self.config['global']['weights']['hh'], hue_col=self.compare_col, hue_order=self.compare_order,
                    x_label='Household Income',y_label = 'Share of Households', plt_title='Household Distribution by Income',                
                )
            except Exception as e:
                print(f"Skipping hh_by_income due to error: {e}")
        else:
            try:
                hh_by_income = self.plotter.plot_share(
                    df=self.survey.hh[self.survey.hh[self.config['global']['columns']['income_label']].isin(self.config['global']['orders']['income_order'])], group_cols=[self.config['global']['columns']['income_label']],
                    axis_order=self.config['global']['orders']['income_order'], weight_col=self.config['global']['weights']['hh'],
                    x_label='Household Income',y_label = 'Share of Households', plt_title='Household Distribution by Income',                
                )
            except Exception as e:
                print(f"Skipping hh_by_income due to error: {e}")
        
        if self.compare:
            try:            hh_by_vehicles = self.plotter.plot_share_hue(
                    df=pd.concat([s.hh[s.hh[self.config['global']['columns']['auto_own']].isin(self.config['global']['orders']['auto_own_order'])] for s in [self.survey, self.survey2] if s is not None], ignore_index=True), group_col=self.config['global']['columns']['auto_own'], 
                    axis_order=self.config['global']['orders']['auto_own_order'], weight_col=self.config['global']['weights']['hh'], hue_col=self.compare_col, hue_order=self.compare_order,
                    x_label='Number of Vehicles', y_label='Share of Households', plt_title='Household Distribution by Number of Vehicles',                
                )
            except Exception as e:
                print(f"Skipping hh_by_vehicles due to error: {e}")

        
        else:
            try:            hh_by_vehicles = self.plotter.plot_share(
                    df=self.survey.hh, group_cols=[self.config['global']['columns']['auto_own']], 
                    axis_order=self.config['global']['orders']['auto_own_order'], weight_col=self.config['global']['weights']['hh'],
                    x_label='Number of Vehicles', y_label='Share of Households', plt_title='Household Distribution by Number of Vehicles',                
                )
            except Exception as e:
                print(f"Skipping hh_by_vehicles due to error: {e}")

        if self.compare:
            try:            hh_by_size = self.plotter.plot_share_hue(
                    df=pd.concat([s.hh[s.hh[self.config['global']['columns']['hh_size']].isin(self.config['global']['orders']['hh_size_order'])] for s in [self.survey, self.survey2] if s is not None], ignore_index=True), group_col=self.config['global']['columns']['hh_size'], 
                    axis_order=self.config['global']['orders']['hh_size_order'], weight_col=self.config['global']['weights']['hh'], hue_col=self.compare_col, hue_order=self.compare_order,
                    x_label='Household Size', y_label='Share of Households', plt_title='Household Distribution by Size',                
                )
            except Exception as e:
                print(f"Skipping hh_by_size due to error: {e}")
        
        else:
            try:            hh_by_size = self.plotter.plot_share(
                    df=self.survey.hh, group_cols=[self.config['global']['columns']['hh_size']], 
                    axis_order=self.config['global']['orders']['hh_size_order'], weight_col=self.config['global']['weights']['hh'],
                    x_label='Household Size', y_label='Share of Households', plt_title='Household Distribution by Size',                
                )
            except Exception as e:
                print(f"Skipping hh_by_size due to error: {e}")
        

        if self.compare:
            try:            hh_by_workers = self.plotter.plot_share_hue(
                    df=pd.concat([s.hh[s.hh[self.config['global']['columns']['hh_workers']].isin(self.config['global']['orders']['hh_workers_order'])] for s in [self.survey, self.survey2] if s is not None], ignore_index=True), group_col=self.config['global']['columns']['hh_workers'], 
                    axis_order=self.config['global']['orders']['hh_workers_order'], weight_col=self.config['global']['weights']['hh'], hue_col=self.compare_col, hue_order=self.compare_order,
                    x_label='Household Workers', y_label='Share of Households', plt_title='Household Distribution by Number of Workers',                
                )
            except Exception as e:
                print(f"Skipping hh_by_workers due to error: {e}")

        
        else:
            try:            hh_by_workers = self.plotter.plot_share(
                    df=self.survey.hh, group_cols=[self.config['global']['columns']['hh_workers']], 
                    axis_order=self.config['global']['orders']['hh_workers_order'], weight_col=self.config['global']['weights']['hh'],
                    x_label='Household Workers', y_label='Share of Households', plt_title='Household Distribution by Number of Workers',                
                )
            except Exception as e:
                print(f"Skipping hh_by_workers due to error: {e}")


        if self.compare:
            try:            pop_by_ptype = self.plotter.plot_share_hue(
                    df=pd.concat([s.person[s.person[self.config['global']['columns']['person_type']].isin(self.config['global']['orders']['person_type_order'])] for s in [self.survey, self.survey2] if s is not None], ignore_index=True), group_col=self.config['global']['columns']['person_type'], 
                    axis_order=self.config['global']['orders']['person_type_order'], weight_col=self.config['global']['weights']['person'], hue_col=self.compare_col, hue_order=self.compare_order,
                    x_label='Person Type', y_label='Share of Persons', plt_title='Person Distribution by Type',                
                )
            except Exception as e:
                print(f"Skipping pop_by_ptype due to error: {e}")
        
        else:
            try:            pop_by_ptype = self.plotter.plot_share(
                    df=self.survey.person, group_cols=[self.config['global']['columns']['person_type']], 
                    axis_order=self.config['global']['orders']['person_type_order'], weight_col=self.config['global']['weights']['person'],
                    x_label='Person Type', y_label='Share of Persons', plt_title='Person Distribution by Type',                
                )
            except Exception as e:
                print(f"Skipping pop_by_ptype due to error: {e}")
        
        if self.compare:
            try:            pop_by_age = self.plotter.plot_share_hue(
                    df=pd.concat([s.person[s.person[self.config['global']['columns']['age']].isin(self.config['global']['orders']['age_order'])] for s in [self.survey, self.survey2] if s is not None], ignore_index=True), group_col=self.config['global']['columns']['age'], 
                    axis_order=self.config['global']['orders']['age_order'], weight_col=self.config['global']['weights']['person'], hue_col=self.compare_col, hue_order=self.compare_order,
                    x_label='Age', y_label='Share of Persons', plt_title='Person Distribution by Age',                
                )
            except Exception as e:
                print(f"Skipping pop_by_age due to error: {e}")
        else:
            try:            pop_by_age = self.plotter.plot_share(
                    df=self.survey.person, group_cols=[self.config['global']['columns']['age']], 
                    axis_order=self.config['global']['orders']['age_order'], weight_col=self.config['global']['weights']['person'],
                    x_label='Age', y_label='Share of Persons', plt_title='Person Distribution by Age',                
                )
            except Exception as e:
                print(f"Skipping pop_by_age due to error: {e}")




        try:
            hh_summary = self.config['sections']['overview']['text']['hh_summary_1']
        except:
            hh_summary = "Household Characteristics"

        hh_row_1 = [h for h in [hh_by_income, hh_by_vehicles] if h is not None]
        hh_row_2 = [h for h in [hh_by_size, hh_by_workers] if h is not None]


        if hh_row_1:
            overview_column.append([hh_summary])
            overview_column.append(hh_row_1)
        if hh_row_2:
            overview_column.append(hh_row_2)
        pop_row = [p for p in [pop_by_ptype, pop_by_age] if p is not None]
        if pop_row:
            try:
                pop_summary = self.config['sections']['overview']['text']['pop_summary_1']
            except:
                pop_summary = "Population Characteristics"
            overview_column.append([pop_summary])
            overview_column.append(pop_row)

        return overview_column