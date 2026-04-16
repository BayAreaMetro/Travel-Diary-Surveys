# surveyplotter/sections/trip_tod.py
import yaml
import pandas as pd
class TripTODSectionBuilder:
    def __init__(self, plotter = None, survey = None, survey2 = None, config = None):
        self.plotter = plotter
        self.survey = survey
        self.survey2 = survey2
        self.compare_col = None
        self.compare_order = None
        # If config is a path, load the YAML
        if isinstance(config, str):
            with open(config, 'r') as f:
                self.config = yaml.safe_load(f)
        else:
            self.config = config

        self.data = self.survey.trips.merge(self.survey.hh, how = 'left', on=[self.config['global']['id']['hh']]).merge(self.survey.person, how = 'left', on=[self.config['global']['id']['hh'], self.config['global']['id']['person']])
        self.compare = self.config['global'].get('compare', False)
        self.data2 = None
        if self.compare and self.survey2 is None:
            raise ValueError("Comparison is enabled in config but survey2 is not provided.")
        if self.survey2 is not None and self.compare:
            self.data2 = self.survey2.trips.merge(self.survey2.hh, how = 'left', on=[self.config['global']['id']['hh']]).merge(self.survey2.person, how = 'left', on=[self.config['global']['id']['hh'], self.config['global']['id']['person']])
            self.compare_col = self.config['global'].get('compare_col', None)
            self.compare_order = [self.data[self.compare_col].iloc[0], self.data2[self.compare_col].iloc[0]]


    def build(self, ):
        """
        build mode share summary tab
        """

        # Initialize all figures to None
        fig_time_purp = None
        fig_time_purp_gender = None
        fig_time_purp_gender2 = None
        fig_time_purp_ptype = None
        fig_time_purp_ptype2 = None
        fig_time_mode_gender = None
        fig_time_mode_gender2 = None
        
        time_label_list = list(self.data.sort_values(self.config['global']['columns']['time_bin'], ascending= True).drop_duplicates(subset = self.config['global']['columns']['time_bin'])[self.config['global']['columns']['time_label']])

        # --- Figures (with error handling) ---
        try:
            fig_time_purp = self.plotter.plot_grouped_dist_with_dropdown(df=pd.concat([d for d in [self.data, self.data2] if d is not None], ignore_index = True), 
                                                                         dropdown_col=self.config['global']['columns']['purpose'], 
                                                                         value_col=self.config['global']['columns']['time_bin'], 
                                                                         hue_col = self.compare_col, hue_order = self.compare_order,
                                                                        weight_col = self.config['global']['weights']['trip'],

                                                                         x_labels = time_label_list )

        except Exception as e:
            print(f"Skipping fig_time_purp due to error: {e}")

        try:
            fig_time_purp_gender = self.plotter.plot_grouped_dist_with_dropdown(df=self.data, 
                                                                                 dropdown_col=self.config['global']['columns']['purpose'], 
                                                                                 hue_col=self.config['global']['columns']['gender'], hue_order = self.config['global']['orders']['gender'],
                                                                                 value_col=self.config['global']['columns']['time_bin'], 
                                                                                plt_title = 'TOD by Purpose and Gender {}'.format(self.compare_order[0]) if self.compare_order is not None else None,
                                                                                weight_col = self.config['global']['weights']['trip'],
                                                                                 x_labels = time_label_list)
        except Exception as e:
            print(f"Skipping fig_time_purp_gender due to error: {e}")
        
        if self.compare:
            try:
                fig_time_purp_gender2 = self.plotter.plot_grouped_dist_with_dropdown(df=self.data2, 
                                                                                        dropdown_col=self.config['global']['columns']['purpose'], 
                                                                                        hue_col=self.config['global']['columns']['gender'], hue_order = self.config['global']['orders']['gender'],
                                                                                        value_col=self.config['global']['columns']['time_bin'], 
                                                                                        plt_title = 'TOD by Purpose and Gender {}'.format(self.compare_order[1]),
                                                                                weight_col = self.config['global']['weights']['trip'],

                                                                                        x_labels = time_label_list)
            except Exception as e:
                print(f"Skipping fig_time_purp_gender due to error: {e}")

        try:
            fig_time_mode_gender = self.plotter.plot_grouped_dist_with_dropdown(df=self.data[self.data[self.config['global']['columns']['mode']].isin(self.config['global']['orders']['mode_order'])], 
                                                                                 dropdown_col=self.config['global']['columns']['mode'], 
                                                                                 hue_col=self.config['global']['columns']['gender'], hue_order = self.config['global']['orders']['gender'],
                                                                                 value_col=self.config['global']['columns']['time_bin'], 
                                                                                plt_title = 'TOD by Purpose and Gender {}'.format(self.compare_order[0]) if self.compare_order is not None else None,
                                                                                weight_col = self.config['global']['weights']['trip'],
                                                                                 x_labels = time_label_list)
        except Exception as e:
            print(f"Skipping fig_time_purp_gender due to error: {e}")
        
        if self.compare:
            try:
                fig_time_mode_gender2 = self.plotter.plot_grouped_dist_with_dropdown(df=self.data2[self.data2[self.config['global']['columns']['mode']].isin(self.config['global']['orders']['mode_order'])], 
                                                                                        dropdown_col=self.config['global']['columns']['mode'], 
                                                                                        hue_col=self.config['global']['columns']['gender'], hue_order = self.config['global']['orders']['gender'],
                                                                                        value_col=self.config['global']['columns']['time_bin'], 
                                                                                        plt_title = 'TOD by Purpose and Gender {}'.format(self.compare_order[1]),
                                                                                weight_col = self.config['global']['weights']['trip'],

                                                                                        x_labels = time_label_list)
            except Exception as e:
                print(f"Skipping fig_time_purp_gender due to error: {e}")


        try:
            fig_time_purp_ptype = self.plotter.plot_grouped_dist_with_dropdown(df=self.data, 
                                                                                 dropdown_col=self.config['global']['columns']['purpose'], 
                                                                                 hue_col=self.config['global']['columns']['person_type'], 
                                                                                 value_col=self.config['global']['columns']['time_bin'], 
                                                                                weight_col = self.config['global']['weights']['trip'],
                                                                                plt_title = 'TOD by Purpose and Person Type {}'.format(self.compare_order[0]) if self.compare_order is not None else None,
                                                                                 x_labels = time_label_list)
        except Exception as e:
            print(f"Skipping fig_time_purp_ptype due to error: {e}")

        if self.compare:
            try:
                fig_time_purp_ptype2 = self.plotter.plot_grouped_dist_with_dropdown(df=self.data2, 
                                                                                    dropdown_col=self.config['global']['columns']['purpose'], 
                                                                                    hue_col=self.config['global']['columns']['person_type'], 
                                                                                    value_col=self.config['global']['columns']['time_bin'], 
                                                                                weight_col = self.config['global']['weights']['trip'],
                                                                                    plt_title = 'TOD by Purpose and Person Type {}'.format(self.compare_order[1]),
                                                                                    x_labels = time_label_list)
            except Exception as e:
                print(f"Skipping fig_time_purp_ptype due to error: {e}")


        # --- Layout equivalence to your `trip_tod_column` ---
        trip_tod_column = [
            [("Trip Time of Day",True)],
        ]

        # Row 1: overall share
        try:
            txt_time_of_day_purp = [self.config['sections']['trip_tod']['text']['time_of_day_purp']]
        except:
            txt_time_of_day_purp = ['Time of Day by Purpose']
        if fig_time_purp is not None:
            trip_tod_column.append(txt_time_of_day_purp)
            trip_tod_column.append([fig_time_purp])

        # Time of Day
        try:
            txt_time_of_day_gender = [self.config['sections']['trip_tod']['text']['time_of_day_gender']]
        except:
            txt_time_of_day_gender = ['Time of Day by Purpose and Gender']
        purp_gender_col = [pg for pg in [fig_time_purp_gender, fig_time_purp_gender2] if pg is not None]
        if fig_time_purp_gender is not None:
            trip_tod_column.append(txt_time_of_day_gender)
            trip_tod_column.append(purp_gender_col)

        # Time of Day
        try:
            txt_time_of_day_ptype = [self.config['sections']['trip_tod']['text']['time_of_day_ptype']]
        except:
            txt_time_of_day_ptype = ['Time of Day by Purpose and Person Type']
        
        purp_ptype_col = [pp for pp in [fig_time_purp_ptype, fig_time_purp_ptype2] if pp is not None]
        if fig_time_purp_ptype is not None:
            trip_tod_column.append(txt_time_of_day_ptype)
            trip_tod_column.append(purp_ptype_col)

        try:
            txt_time_of_day_mode_gender = [self.config['sections']['trip_tod']['text']['time_of_day_mode_gender']]
        except:
            txt_time_of_day_mode_gender = ['Time of Day by Mode and Gender']
        
        mode_gender_col = [pp for pp in [fig_time_mode_gender, fig_time_mode_gender2] if pp is not None]
        if fig_time_purp_ptype is not None:
            trip_tod_column.append(txt_time_of_day_mode_gender)
            trip_tod_column.append(mode_gender_col)
        

        return trip_tod_column