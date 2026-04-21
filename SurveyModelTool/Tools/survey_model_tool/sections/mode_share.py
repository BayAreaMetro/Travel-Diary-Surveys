# surveyplotter/sections/mode_share.py
import yaml
import pandas as pd

class ModeShareSectionBuilder:
    def __init__(self, plotter = None, config = None, survey = None, survey2 = None):
        self.plotter = plotter
        self.survey = survey
        self.survey2 = survey2
        self.compare_order = ['', '']
        # If config is a path, load the YAML
        if isinstance(config, str):
            with open(config, 'r') as f:
                self.config = yaml.safe_load(f)
        else:
            self.config = config
        self.data = self.survey.trips.merge(self.survey.hh, how = 'left', on=[self.config['global']['id']['hh']]).merge(self.survey.person, how = 'left', on=[self.config['global']['id']['hh'], self.config['global']['id']['person']])
        self.data = self.data[self.data[self.config['global']['columns']['mode']].isin(self.config['global']['orders']['mode_order'])]  # Filter out rows with NA mode
        self.compare = self.config['global'].get('compare', False)
        self.data2 = None
        if self.compare and self.survey2 is None:
            raise ValueError("Comparison is enabled in config but survey2 is not provided.")
        if self.survey2 is not None and self.compare:
            self.data2 = self.survey2.trips.merge(self.survey2.hh, how = 'left', on=[self.config['global']['id']['hh']]).merge(self.survey2.person, how = 'left', on=[self.config['global']['id']['hh'], self.config['global']['id']['person']])
            self.data2 = self.data2[self.data2[self.config['global']['columns']['mode']].isin(self.config['global']['orders']['mode_order'])]  # Filter out rows with NA mode  
            self.compare_col = self.config['global'].get('compare_col', None)
            self.compare_order = [self.data[self.compare_col].iloc[0], self.data2[self.compare_col].iloc[0]]

    def build(self, ):
        """
        build mode share summary tab
        """

        # Initialize all figures to None
        fig_by_period_3 = None
        fig_by_period_3_2 = None
        fig_by_period_stack = None
        fig_by_period_stack_2 = None
        fig_by_purpose_1 = None
        fig_by_purpose_1_2 = None
        fig_by_purpose_2 = None
        fig_by_purpose_2_2 = None
        fig_by_income_stack = None
        fig_by_income_stack_2 = None
        fig_by_auto = None
        fig_by_auto_2 = None
        fig_by_auto_stack = None
        fig_by_auto_stack_2 = None
        fig_by_distance_stack = None
        fig_by_distance_stack_2 = None
        fig_by_dist = None
        fig_by_dist_2 = None
        purpose_mode = None
        county_mode = None

        # --- Figures (with error handling) ---
        try:
            fig_by_period_3 = self.plotter.plot_share_hue(
                df=self.data, group_col=self.config['global']['columns']['mode'], hue_col = self.config['global']['columns']['time_period'], share_is_bin=False,
                hue_order=self.config['global']['orders']['period_order'], weight_col=self.config['global']['weights']['trip'],
                x_label='Trip Mode', y_label='Share of Trips',
                axis_order=self.config['global']['orders']['mode_order'], plt_title='Mode Share by Time of Day {}'.format(self.compare_order[0])
            )
        except Exception as e:
            print(f"Skipping fig_by_period_3 due to error: {e}")
        
        if self.survey2 is not None and self.compare:
            try:
                fig_by_period_3_2 = self.plotter.plot_share_hue(
                    df=self.data2, group_col=self.config['global']['columns']['mode'], hue_col = self.config['global']['columns']['time_period'], share_is_bin=False,
                    hue_order=self.config['global']['orders']['period_order'], weight_col=self.config['global']['weights']['trip'],
                    x_label='Trip Mode', y_label='Share of Trips',
                    axis_order=self.config['global']['orders']['mode_order'], plt_title='Mode Share by Time of Day {}'.format(self.compare_order[1])
                )
            except Exception as e:
                print(f"Skipping fig_by_period_3_2 due to error: {e}")
        
        
        try:
            fig_by_period_stack = self.plotter.plot_stacked_bar(
                df=self.data, group_col=self.config['global']['columns']['mode'], stack_col='period',
                stack_order=self.config['global']['orders']['period_order'], weight_col=self.config['global']['weights']['trip'],
                axis_order=self.config['global']['orders']['mode_order'], plt_title='Mode Time of Day Share {}'.format(self.compare_order[0]),
                add_ref=True, x_label='Trip Mode'
            )
        except Exception as e:
            print(f"Skipping fig_by_period_stack due to error: {e}")

        if self.compare and self.survey2 is not None:
            try:
                fig_by_period_stack_2 = self.plotter.plot_stacked_bar(
                    df=self.data2, group_col=self.config['global']['columns']['mode'], stack_col='period',
                    stack_order=self.config['global']['orders']['period_order'], weight_col=self.config['global']['weights']['trip'],
                    axis_order=self.config['global']['orders']['mode_order'], plt_title='Mode Time of Day Share {}'.format(self.compare_order[1]),
                    add_ref=True, x_label='Trip Mode'
                )
            except Exception as e:
                print(f"Skipping fig_by_period_stack_2 due to error: {e}")


        try:
            fig_by_purpose_1 = self.plotter.plot_share_hue(
                df=self.data, group_col=self.config['global']['columns']['mode'],hue_col = self.config['global']['columns']['work_purpose'], share_is_bin=False,
                weight_col=self.config['global']['weights']['trip'], x_label='Trip Mode',
                hue_order=["Work",'Non-Work'], plt_title='Mode Share by Trip Work Purpose {}'.format(self.compare_order[0]),
                axis_order=self.config['global']['orders']['mode_order']
            )
        except Exception as e:
            print(f"Skipping fig_by_purpose_1 due to error: {e}")

        if self.compare and self.survey2 is not None:
            try:
                fig_by_purpose_1_2 = self.plotter.plot_share_hue(
                    df=self.data2, group_col=self.config['global']['columns']['mode'], hue_col = self.config['global']['columns']['work_purpose'], share_is_bin=False,
                    weight_col=self.config['global']['weights']['trip'], x_label='Trip Mode',
                    hue_order=["Work",'Non-Work'], plt_title='Mode Share by Trip Work Purpose {}'.format(self.compare_order[1]),
                    axis_order=self.config['global']['orders']['mode_order']
                )
            except Exception as e:
                print(f"Skipping fig_by_purpose_1_2 due to error: {e}")
        
        try:
            fig_by_purpose_2 = self.plotter.plot_stacked_bar(
                df=self.data, group_col=self.config['global']['columns']['mode'], add_ref=True, stack_col=self.config['global']['columns']['work_purpose'],
                weight_col=self.config['global']['weights']['trip'], x_label='Trip Mode',
                stack_order=self.config['global']['orders']['work_purp_order'], plt_title='Mode Share by Trip Work Purpose {}'.format(self.compare_order[0]),
                axis_order=self.config['global']['orders']['mode_order']
            )
        except Exception as e:
            print(f"Skipping fig_by_purpose_2 due to error: {e}")

        if self.compare and self.survey2 is not None:
            try:
                fig_by_purpose_2_2 = self.plotter.plot_stacked_bar(
                    df=self.data2, group_col=self.config['global']['columns']['mode'], add_ref=True, stack_col=self.config['global']['columns']['work_purpose'],
                    weight_col=self.config['global']['weights']['trip'], x_label='Trip Mode',
                    stack_order=self.config['global']['orders']['work_purp_order'], plt_title='Mode Share by Trip Work Purpose {}'.format(self.compare_order[1]),
                    axis_order=self.config['global']['orders']['mode_order']
                )
            except Exception as e:
                print(f"Skipping fig_by_purpose_2_2 due to error: {e}")
        
        
        try:
            fig_by_income_stack = self.plotter.plot_stacked_bar(
                df=self.data[self.data[self.config['global']['columns']['income_label']].isin(self.config['global']['orders']['income_order'])], add_ref=True,
                stack_order=self.config['global']['orders']['income_order'],
                group_col=self.config['global']['columns']['mode'], stack_col=self.config['global']['columns']['income_label'], weight_col=self.config['global']['weights']['trip'],
                plt_title='Mode Share by Income {}'.format(self.compare_order[0]), axis_order=self.config['global']['orders']['mode_order'], x_label='Trip Mode {}'.format(self.compare_order[0])
            )
        except Exception as e:
            print(f"Skipping fig_by_income_stack due to error: {e}")

        if self.compare and self.survey2 is not None:
            try:
                fig_by_income_stack_2 = self.plotter.plot_stacked_bar(
                    df=self.data2[self.data2[self.config['global']['columns']['income_label']].isin(self.config['global']['orders']['income_order'])], add_ref=True,
                    stack_order=self.config['global']['orders']['income_order'],
                    group_col=self.config['global']['columns']['mode'], stack_col=self.config['global']['columns']['income_label'], weight_col=self.config['global']['weights']['trip'],
                    plt_title='Mode Share by Income {}'.format(self.compare_order[1]), axis_order=self.config['global']['orders']['mode_order'], x_label='Trip Mode {}'.format(self.compare_order[1])
                )
            except Exception as e:
                print(f"Skipping fig_by_income_stack_2 due to error: {e}")
        
        try:
            fig_by_auto = self.plotter.plot_share_hue(
                df=self.data[~self.data[self.config['global']['columns']['auto_own']].isna()], group_col=self.config['global']['columns']['mode'],hue_col = self.config['global']['columns']['auto_own'], share_is_bin=False,
                hue_order=self.config['global']['orders']['auto_own_order'], weight_col=self.config['global']['weights']['trip'],
                x_label='Trip Mode', plt_title='Mode Share by Auto Ownership {}'.format(self.compare_order[0]),
                axis_order=self.config['global']['orders']['mode_order']
            )
        except Exception as e:
            print(f"Skipping fig_by_auto due to error: {e}")
        if self.compare and self.survey2 is not None:
            try:
                fig_by_auto_2 = self.plotter.plot_share_hue(
                    df=self.data2[~self.data2[self.config['global']['columns']['auto_own']].isna()], group_col=self.config['global']['columns']['mode'], hue_col=self.config['global']['columns']['auto_own'], share_is_bin=False,
                    hue_order=self.config['global']['orders']['auto_own_order'], weight_col=self.config['global']['weights']['trip'],
                    x_label='Trip Mode', plt_title='Mode Share by Auto Ownership {}'.format(self.compare_order[1]),
                    axis_order=self.config['global']['orders']['mode_order']
                )
            except Exception as e:
                print(f"Skipping fig_by_auto_2 due to error: {e}")
        
        try:
            fig_by_auto_stack = self.plotter.plot_stacked_bar(
                df=self.data, group_col=self.config['global']['columns']['mode'], stack_col=self.config['global']['columns']['auto_own'],
                stack_order=self.config['global']['orders']['auto_own_order'], weight_col=self.config['global']['weights']['trip'],
                plt_title='Mode Share by Auto Ownership {}'.format(self.compare_order[0]), add_ref=True,
                axis_order=self.config['global']['orders']['mode_order'], x_label='Trip Mode'
            )
        except Exception as e:
            print(f"Skipping fig_by_auto_stack due to error: {e}")
        if self.compare and self.survey2 is not None:
            try:
                fig_by_auto_stack_2 = self.plotter.plot_stacked_bar(
                    df=self.data2, group_col=self.config['global']['columns']['mode'], stack_col=self.config['global']['columns']['auto_own'],
                    stack_order=self.config['global']['orders']['auto_own_order'], weight_col=self.config['global']['weights']['trip'],
                    plt_title='Mode Share by Auto Ownership {}'.format(self.compare_order[1]), add_ref=True,
                    axis_order=self.config['global']['orders']['mode_order'], x_label='Trip Mode'
                )
            except Exception as e:
                print(f"Skipping fig_by_auto_stack_2 due to error: {e}")
        
        try:
            fig_by_distance_stack = self.plotter.plot_stacked_bar(
                df=self.data[self.data['Distance Bin'].notnull()], group_col=self.config['global']['columns']['mode'], stack_col='Distance Bin',
                weight_col=self.config['global']['weights']['trip'], plt_title='Mode Share by Distance {}'.format(self.compare_order[0]), add_ref=True,
                stack_order=self.config['global']['orders']['distance_order'], axis_order=self.config['global']['orders']['mode_order'], x_label='Trip Mode'
            )
        except Exception as e:
            print(f"Skipping fig_by_distance_stack due to error: {e}")

        if self.compare and self.survey2 is not None:
            try:
                fig_by_distance_stack_2 = self.plotter.plot_stacked_bar(
                    df=self.data2[self.data2['Distance Bin'].notnull()], group_col=self.config['global']['columns']['mode'], stack_col='Distance Bin',
                    weight_col=self.config['global']['weights']['trip'], plt_title='Mode Share by Distance {}'.format(self.compare_order[1]), add_ref=True,
                    stack_order=self.config['global']['orders']['distance_order'], axis_order=self.config['global']['orders']['mode_order'], x_label='Trip Mode'
                )
            except Exception as e:
                print(f"Skipping fig_by_distance_stack_2 due to error: {e}")
        try:
            fig_by_dist = self.plotter.plot_dist(
                df=self.data.dropna(subset=[self.config['global']['columns']['distance']]), x_label='Distance (mi)',
                value_col='Distance', hue_order=self.config['global']['orders']['mode_order'], plt_title='Trip Distance Distribution by Mode {}'.format(self.compare_order[0]),
                weight_col=self.config['global']['weights']['trip'], hue=self.config['global']['columns']['mode']
            )
        except Exception as e:
            print(f"Skipping fig_by_dist due to error: {e}")

        if self.compare and self.survey2 is not None:
            try:
                fig_by_dist_2 = self.plotter.plot_dist(
                    df=self.data2.dropna(subset=[self.config['global']['columns']['distance']]), x_label='Distance (mi)',
                    value_col='Distance', hue_order=self.config['global']['orders']['mode_order'], plt_title='Trip Distance Distribution by Mode {}'.format(self.compare_order[1]),
                    weight_col=self.config['global']['weights']['trip'], hue=self.config['global']['columns']['mode']
                )
            except Exception as e:
                print(f"Skipping fig_by_dist_2 due to error: {e}")
        

        try:
            purpose_mode = self.plotter.plot_grouped_bar_with_dropdown(
                df=pd.concat([d for d in [self.data, self.data2] if d is not None], ignore_index=True), axis_order=self.config['global']['orders']['mode_order'], group_col=self.config['global']['columns']['mode'],
                x_label='Trip Mode', value_col=self.config['global']['weights']['trip'], dropdown_col=self.config['global']['columns']['purpose'],
                hue_col=self.compare_col if self.compare else None, hue_order=self.compare_order if self.compare else None
            )
        except Exception as e:
            print(f"Skipping purpose_mode due to error: {e}")
    
   
        try:
            county_mode = self.plotter.plot_grouped_bar_with_dropdown(
                df=pd.concat([d for d in [self.data, self.data2] if d is not None], ignore_index=True), axis_order=self.config['global']['orders']['mode_order'], group_col=self.config['global']['columns']['mode'],
                x_label='Trip Mode', value_col=self.config['global']['weights']['trip'], dropdown_col=self.config['global']['columns']['home_county'], 
                hue_col=self.compare_col if self.compare else None, hue_order=self.compare_order if self.compare else None
            )
        except Exception as e:
            print(f"Skipping county_mode due to error: {e}")
       

        # --- Layout equivalence to your `mode_share_column` ---
        mode_share_column = [
            [("Regional Mode Share",True)],
        ]

        # Row 1: overall share
        try:
            txt_county_mode = [self.config['sections']['mode_share']['text']['county_mode']]
        except:
            txt_county_mode = ['Mode Share by County']
        if county_mode is not None:
            mode_share_column.append(txt_county_mode)
            mode_share_column.append([county_mode])

        # Time of Day
        try:
            txt_period = [self.config['sections']['mode_share']['text']['period']]
        except:
            txt_period = ['Time of Day']
        figs_period = [f for f in [fig_by_period_3, fig_by_period_3_2, fig_by_period_stack, fig_by_period_stack_2] if f is not None]
        if len(figs_period) > 0:
            mode_share_column.append([("Time of Day", True)])
            mode_share_column.append(txt_period)
            if len(figs_period) == 4:
                mode_share_column.append([figs_period[0], figs_period[1]])
                mode_share_column.append([figs_period[2], figs_period[3]])
            else:
                mode_share_column.append(figs_period)

        # Trip Purpose
        try:
            txt_work_purp = [self.config['sections']['mode_share']['text']['work_purpose'], self.config['sections']['mode_share']['text']['work_purpose_2']]
        except:
            txt_work_purp = ['Work and Non-Work Trip Mode']
        
        figs_purpose = [f for f in [fig_by_purpose_1, fig_by_purpose_1_2, fig_by_purpose_2, fig_by_purpose_2_2] if f is not None]
        if len(figs_purpose) > 0:
            mode_share_column.append([("Trip Purpose", True)])
            mode_share_column.append(txt_work_purp)
            if len(figs_purpose) == 4:
                mode_share_column.append([figs_purpose[0], figs_purpose[1]])
                mode_share_column.append([figs_purpose[2], figs_purpose[3]])
            else:
                mode_share_column.append(figs_purpose)

        try:
            txt_purp = [self.config['sections']['mode_share']['text']['purp']]
        except:
            txt_purp = ['Trip Purpose']
        if purpose_mode is not None:
            mode_share_column.append(txt_purp)
            mode_share_column.append(purpose_mode)

        # Household Income
        try:
            txt_income = [self.config['sections']['mode_share']['text']['income']]
        except:
            txt_income = ['Household Income']
        if fig_by_income_stack is not None:
            mode_share_column.append([("Household Income", True)])
            mode_share_column.append(txt_income)
            mode_share_column.append([ f for f in [fig_by_income_stack, fig_by_income_stack_2] if f is not None])

        # Auto Ownership
        try:
            txt_auto = [self.config['sections']['mode_share']['text']['auto']]
        except:
            txt_auto = ['Auto Ownership']
        figs_auto = [f for f in [fig_by_auto, fig_by_auto_2, fig_by_auto_stack, fig_by_auto_stack_2] if f is not None]
        if len(figs_auto) > 0:
            mode_share_column.append([("Auto Ownership", True)])
            mode_share_column.append(txt_auto)
            if len(figs_auto) == 4:
                mode_share_column.append([figs_auto[0], figs_auto[1]])
                mode_share_column.append([figs_auto[2], figs_auto[3]])
            else:
                mode_share_column.append(figs_auto)

        # Trip Distance
        try:
            txt_dist = [self.config['sections']['mode_share']['text']['distance']]
        except:
            txt_dist = ['Trip Distance']
        figs_dist = [f for f in [fig_by_distance_stack, fig_by_distance_stack_2, fig_by_dist, fig_by_dist_2] if f is not None]
        if len(figs_dist) > 0:
            mode_share_column.append([("Trip Distance", True)])
            mode_share_column.append(txt_dist)
            if len(figs_dist) == 4:
                mode_share_column.append([figs_dist[0], figs_dist[1]])
                mode_share_column.append([figs_dist[2], figs_dist[3]])
            else:
                mode_share_column.append(figs_dist)

        return mode_share_column