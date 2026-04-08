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
        self.plotter.data = self.plotter.data[self.plotter.data[self.config['global']['columns']['mode']].isin(self.config['sections']['mode_share']['axis_mode'])]  # Filter out rows with NA mode

    def build(self, ):
        """
        build mode share summary tab
        """


        # --- Figures (direct translation from your notebook) ---
        fig_overall = self.plotter.plot_share(
            df=self.plotter.data, group_cols=self.config['global']['columns']['mode'], weight_col=self.config['global']['weights']['trip'],
            axis_order=self.config['sections']['mode_share']['axis_mode'], x_label='Trip Mode', plt_title='Overall Mode Share'
        )

        fig_by_period_3 = self.plotter.plot_share_hue(
            df=self.plotter.data, group_cols=[self.config['globals']['columns']['mode'],self.config['globals']['columns']['time_period']], share_is_bin=False,
            sub_axis_order=self.config['sections']['mode_share']['period_order'], weight_col=self.config['global']['weights']['trip'],
            x_label='Trip Mode', y_label='Share of Trips',
            axis_order=self.config['sections']['mode_share']['axis_mode'], plt_title='Mode Share by Time of Day'
        )

        fig_by_period_stack = self.plotter.plot_stacked_bar(
            df=self.plotter.data, group_col=self.config['globals']['columns']['mode'], stack_col='period',
            stack_order=self.config['sections']['mode_share']['period_order'], weight_col=self.config['global']['weights']['trip'],
            axis_order=self.config['sections']['mode_share']['axis_mode'], plt_title='Mode Time of Day Share',
            add_ref=True, x_label='Trip Mode'
        )

        fig_by_purpose_1 = self.plotter.plot_share_hue(
            df=self.plotter.data, group_cols=[self.config['globals']['columns']['mode'],self.config['globals']['columns']['work_purpose']], share_is_bin=False,
            weight_col=self.config['global']['weights']['trip'], x_label='Trip Mode',
            sub_axis_order=["Work",'Non-Work'], plt_title='Mode Share by Trip Work Purpose',
            axis_order=self.config['sections']['mode_share']['axis_mode']
        )

        fig_by_purpose_2 = self.plotter.plot_stacked_bar(
            df=self.plotter.data, group_col=self.config['globals']['columns']['mode'], add_ref=True, stack_col=self.config['globals']['columns']['work_purpose'],
            weight_col=self.config['global']['weights']['trip'], x_label='Trip Mode',
            stack_order=self.config['sections']['mode_share']['work_purp_order'], plt_title='Mode Share by Trip Work Purpose',
            axis_order=self.config['sections']['mode_share']['axis_mode']
        )

        fig_by_income_stack = self.plotter.plot_stacked_bar(
            df=self.plotter.data[self.plotter.data[self.config['globals']['columns']['income_bin'].isin(self.config['sections']['mode_share']['income_order'])]], add_ref=True,
            stack_order=self.config['sections']['mode_share']['income_order'],
            group_col=self.config['globals']['columns']['mode'], stack_col=self.config['globals']['columns']['income_bin'], weight_col=self.config['global']['weights']['trip'],
            plt_title='Mode Share by Income', axis_order=self.config['sections']['mode_share']['axis_mode'], x_label='Trip Mode'
        )

        fig_by_auto = self.plotter.plot_share_hue(
            df=self.plotter.data, group_cols=[self.config['globals']['columns']['mode'],self.config['globals']['columns']['auto_own']], share_is_bin=False,
            sub_axis_order=self.config['sections']['mode_share']['auto_own_order'], weight_col=self.config['global']['weights']['trip'],
            x_label='Trip Mode', plt_title='Mode Share by Auto Ownership',
            axis_order=self.config['sections']['mode_share']['axis_mode']
        )

        fig_by_auto_stack = self.plotter.plot_stacked_bar(
            df=self.plotter.data, group_col=self.config['globals']['columns']['mode'], stack_col=self.config['globals']['columns']['auto_own'],
            stack_order=self.config['sections']['mode_share']['auto_own_order'], weight_col=self.config['global']['weights']['trip'],
            plt_title='Mode Share by Auto Ownership', add_ref=True,
            axis_order=self.config['sections']['mode_share']['axis_mode'], x_label='Trip Mode'
        )

        fig_by_distance_stack = self.plotter.plot_stacked_bar(
            df=self.plotter.data[self.plotter.data['Distance Bin'].notnull()], group_col=self.config['globals']['columns']['mode'], stack_col='Distance Bin',
            weight_col=self.config['global']['weights']['trip'], plt_title='Mode Share by Distance', add_ref=True,
            stack_order=self.config['sections']['mode_share']['distance_order'], axis_order=self.config['sections']['mode_share']['axis_mode'], x_label='Trip Mode'
        )

        fig_by_dist = self.plotter.plot_dist(
            df=self.plotter.data.dropna(subset=[self.config['globals']['columns']['distance_crop']]), x_label='Distance (mi)',
            value_col='Distance', hue_order=self.config['sections']['mode_share']['axis_mode'],
            weight_col=self.config['global']['weights']['trip'], hue=self.config['globals']['columns']['mode']
        )

        purpose_mode = self.plotter.plot_grouped_bar_with_dropdown(
            df=self.plotter.data, axis_order=self.config['sections']['mode_share']['axis_mode'], group_col=self.config['globals']['columns']['mode'],
            x_label='Trip Mode', value_col=self.config['global']['weights']['trip'], dropdown_col=self.config['globals']['columns']['purpose']
        )

        county_mode = self.plotter.plot_grouped_bar_with_dropdown(
            df=self.plotter.data, axis_order=self.config['sections']['mode_share']['axis_mode'], group_col=self.config['globals']['columns']['mode'],
            x_label='Trip Mode', value_col=self.config['global']['weights']['trip'], dropdown_col=self.config['globals']['columns']['home_county']
        )

        
        # The sc_short & sc_short_drive_purpose visuals you referenced would be added here once available
        # sc_short, sc_short_drive_purpose_table = ...

        # --- Layout equivalence to your `mode_share_column` ---
        mode_share_column = [
                    [("Regional Mode Share",True)],
                        # Row 1: overall share
                        [self.config['sections']['mode_share']['text']['overall']],
                        [county_mode],
                        [("Time of Day", True)],
                        # Row 2: by time period
                        [self.config['sections']['mode_share']['text']['period']],
                        [fig_by_period_3,fig_by_period_stack],
                        [("Trip Purpose", True)],
                        # Row 3: by purpose
                        [self.config['sections']['mode_share']['text']['purpose_1'] , self.config['sections']['mode_share']['text']['purpose_2']],
                        [fig_by_purpose_1 , fig_by_purpose_2],
                        [self.config['sections']['mode_share']['text']['purp']],
                        [purpose_mode],
                        [("Household Income", True)],
                        # Row 4: income and auto ownership (two visuals)
                        [self.config['sections']['mode_share']['text']['income']],
                        [fig_by_income_stack],
                        [("Auto Ownership", True)],
                        [self.config['sections']['mode_share']['text']['auto_1']],

                        [fig_by_auto, fig_by_auto_stack], 
                        [("Trip Distance", True)],
                        # Row 5: by distance
                        [self.config['sections']['mode_share']['text']['distance']],
                        [fig_by_distance_stack, fig_by_dist]]
        return mode_share_column