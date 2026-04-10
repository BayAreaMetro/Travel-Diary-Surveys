# surveyplotter/sections/mode_share.py
import yaml

class ModeShareSectionBuilder:
    def __init__(self, plotter, config):
        self.plotter = plotter
        # If config is a path, load the YAML
        if isinstance(config, str):
            with open(config, 'r') as f:
                self.config = yaml.safe_load(f)
        else:
            self.config = config
        self.plotter.data_source = self.plotter.data_source[self.plotter.data_source[self.config['global']['columns']['mode']].isin(self.config['global']['orders']['mode_order'])]  # Filter out rows with NA mode

    def build(self, ):
        """
        build mode share summary tab
        """

        # Initialize all figures to None
        fig_by_period_3 = None
        fig_by_period_stack = None
        fig_by_purpose_1 = None
        fig_by_purpose_2 = None
        fig_by_income_stack = None
        fig_by_auto = None
        fig_by_auto_stack = None
        fig_by_distance_stack = None
        fig_by_dist = None
        purpose_mode = None
        county_mode = None

        # --- Figures (with error handling) ---
        try:
            fig_by_period_3 = self.plotter.plot_share_hue(
                df=self.plotter.data_source, group_cols=[self.config['global']['columns']['mode'],self.config['global']['columns']['time_period']], share_is_bin=False,
                sub_axis_order=self.config['global']['orders']['period_order'], weight_col=self.config['global']['weights']['trip'],
                x_label='Trip Mode', y_label='Share of Trips',
                axis_order=self.config['global']['orders']['mode_order'], plt_title='Mode Share by Time of Day'
            )
        except Exception as e:
            print(f"Skipping fig_by_period_3 due to error: {e}")

        try:
            fig_by_period_stack = self.plotter.plot_stacked_bar(
                df=self.plotter.data_source, group_col=self.config['global']['columns']['mode'], stack_col='period',
                stack_order=self.config['global']['orders']['period_order'], weight_col=self.config['global']['weights']['trip'],
                axis_order=self.config['global']['orders']['mode_order'], plt_title='Mode Time of Day Share',
                add_ref=True, x_label='Trip Mode'
            )
        except Exception as e:
            print(f"Skipping fig_by_period_stack due to error: {e}")

        try:
            fig_by_purpose_1 = self.plotter.plot_share_hue(
                df=self.plotter.data_source, group_cols=[self.config['global']['columns']['mode'],self.config['global']['columns']['work_purpose']], share_is_bin=False,
                weight_col=self.config['global']['weights']['trip'], x_label='Trip Mode',
                sub_axis_order=["Work",'Non-Work'], plt_title='Mode Share by Trip Work Purpose',
                axis_order=self.config['global']['orders']['mode_order']
            )
        except Exception as e:
            print(f"Skipping fig_by_purpose_1 due to error: {e}")

        try:
            fig_by_purpose_2 = self.plotter.plot_stacked_bar(
                df=self.plotter.data_source, group_col=self.config['global']['columns']['mode'], add_ref=True, stack_col=self.config['global']['columns']['work_purpose'],
                weight_col=self.config['global']['weights']['trip'], x_label='Trip Mode',
                stack_order=self.config['global']['orders']['work_purp_order'], plt_title='Mode Share by Trip Work Purpose',
                axis_order=self.config['global']['orders']['mode_order']
            )
        except Exception as e:
            print(f"Skipping fig_by_purpose_2 due to error: {e}")

        try:
            fig_by_income_stack = self.plotter.plot_stacked_bar(
                df=self.plotter.data_source[self.plotter.data_source[self.config['global']['columns']['income_label']].isin(self.config['global']['orders']['income_order'])], add_ref=True,
                stack_order=self.config['global']['orders']['income_order'],
                group_col=self.config['global']['columns']['mode'], stack_col=self.config['global']['columns']['income_label'], weight_col=self.config['global']['weights']['trip'],
                plt_title='Mode Share by Income', axis_order=self.config['global']['orders']['mode_order'], x_label='Trip Mode'
            )
        except Exception as e:
            print(f"Skipping fig_by_income_stack due to error: {e}")

        try:
            fig_by_auto = self.plotter.plot_share_hue(
                df=self.plotter.data_source, group_cols=[self.config['global']['columns']['mode'],self.config['global']['columns']['auto_own']], share_is_bin=False,
                sub_axis_order=self.config['global']['orders']['auto_own_order'], weight_col=self.config['global']['weights']['trip'],
                x_label='Trip Mode', plt_title='Mode Share by Auto Ownership',
                axis_order=self.config['global']['orders']['mode_order']
            )
        except Exception as e:
            print(f"Skipping fig_by_auto due to error: {e}")

        try:
            fig_by_auto_stack = self.plotter.plot_stacked_bar(
                df=self.plotter.data_source, group_col=self.config['global']['columns']['mode'], stack_col=self.config['global']['columns']['auto_own'],
                stack_order=self.config['global']['orders']['auto_own_order'], weight_col=self.config['global']['weights']['trip'],
                plt_title='Mode Share by Auto Ownership', add_ref=True,
                axis_order=self.config['global']['orders']['mode_order'], x_label='Trip Mode'
            )
        except Exception as e:
            print(f"Skipping fig_by_auto_stack due to error: {e}")

        try:
            fig_by_distance_stack = self.plotter.plot_stacked_bar(
                df=self.plotter.data_source[self.plotter.data_source['Distance Bin'].notnull()], group_col=self.config['global']['columns']['mode'], stack_col='Distance Bin',
                weight_col=self.config['global']['weights']['trip'], plt_title='Mode Share by Distance', add_ref=True,
                stack_order=self.config['global']['orders']['distance_order'], axis_order=self.config['global']['orders']['mode_order'], x_label='Trip Mode'
            )
        except Exception as e:
            print(f"Skipping fig_by_distance_stack due to error: {e}")

        try:
            fig_by_dist = self.plotter.plot_dist(
                df=self.plotter.data_source.dropna(subset=[self.config['global']['columns']['distance_crop']]), x_label='Distance (mi)',
                value_col='Distance', hue_order=self.config['global']['orders']['mode_order'],
                weight_col=self.config['global']['weights']['trip'], hue=self.config['global']['columns']['mode']
            )
        except Exception as e:
            print(f"Skipping fig_by_dist due to error: {e}")

        try:
            purpose_mode = self.plotter.plot_grouped_bar_with_dropdown(
                df=self.plotter.data_source, axis_order=self.config['global']['orders']['mode_order'], group_col=self.config['global']['columns']['mode'],
                x_label='Trip Mode', value_col=self.config['global']['weights']['trip'], dropdown_col=self.config['global']['columns']['purpose']
            )
        except Exception as e:
            print(f"Skipping purpose_mode due to error: {e}")

        try:
            county_mode = self.plotter.plot_grouped_bar_with_dropdown(
                df=self.plotter.data_source, axis_order=self.config['global']['orders']['mode_order'], group_col=self.config['global']['columns']['mode'],
                x_label='Trip Mode', value_col=self.config['global']['weights']['trip'], dropdown_col=self.config['global']['columns']['home_county']
            )
        except Exception as e:
            print(f"Skipping county_mode due to error: {e}")

        # --- Layout equivalence to your `mode_share_column` ---
        mode_share_column = [
            [("Regional Mode Share",True)],
        ]

        # Row 1: overall share
        txt_county_mode = [self.config['sections']['mode_share']['text']['county_mode']]
        if county_mode is not None:
            mode_share_column.append(txt_county_mode)
            mode_share_column.append([county_mode])

        # Time of Day
        try:
            txt_period = [self.config['sections']['mode_share']['text']['period']]
        except:
            txt_period = ['Time of Day']
        figs_period = [f for f in [fig_by_period_3, fig_by_period_stack] if f is not None]
        if len(figs_period) > 0:
            mode_share_column.append([("Time of Day", True)])
            mode_share_column.append(txt_period)
            mode_share_column.append(figs_period)

        # Trip Purpose
        try:
            txt_work_purp = [self.config['sections']['mode_share']['text']['work_purpose'], self.config['sections']['mode_share']['text']['work_purpose_2']]
        except:
            txt_work_purp = ['Work and Non-Work Trip Mode']
        
        figs_purpose = [f for f in [fig_by_purpose_1, fig_by_purpose_2] if f is not None]
        if len(figs_purpose) > 0:
            mode_share_column.append([("Trip Purpose", True)])
            mode_share_column.append(txt_work_purp)
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
            mode_share_column.append([fig_by_income_stack])

        # Auto Ownership
        try:
            txt_auto = [self.config['sections']['mode_share']['text']['auto']]
        except:
            txt_auto = ['Auto Ownership']
        figs_auto = [f for f in [fig_by_auto, fig_by_auto_stack] if f is not None]
        if len(figs_auto) > 0:
            mode_share_column.append([("Auto Ownership", True)])
            mode_share_column.append(txt_auto)
            mode_share_column.append(figs_auto)

        # Trip Distance
        try:
            txt_dist = [self.config['sections']['mode_share']['text']['distance']]
        except:
            txt_dist = ['Trip Distance']
        figs_dist = [f for f in [fig_by_distance_stack, fig_by_dist] if f is not None]
        if len(figs_dist) > 0:
      
            mode_share_column.append([("Trip Distance", True)])
            mode_share_column.append(txt_dist)
            mode_share_column.append(figs_dist)

        return mode_share_column