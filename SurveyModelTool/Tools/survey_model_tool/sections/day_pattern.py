# surveyplotter/sections/day_pattern.py
import yaml

from dashboard import plotter

class DayPatternSectionBuilder:
    def __init__(self, plotter, survey, summarizer, config):
        self.survey = survey
        self.plotter = plotter
        self.summarizer = summarizer
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
        overall_trip_rates = self.summarizer.trip_rates(trips = self.plotter.data_source,
                                                         pop = self.survey.person, 
                                                         person_group_col = self.config['global']['columns']['person_type'],
                                                         person_weight_col = self.config['global']['weights']['person'],
                                                         trip_weight_col = self.config['global'][''weights'']['trip'])


        # --- Figures (direct translation from your notebook) ---
        txt_trip_rate_ptype = "Trip Rates vary by person type with workers showing the highest trip rates followed by university students. Non-working adults show a trip rate not far below full-time workers. This may represent non-paid domestic work."
        trip_rates_gender = plotter.plot_simple_bar_with_dropdown(df  = total_telework, axis_order = list(config['income_label'].values()),dropdown_col = 'gender_brief', group_col = 'income_label', value_col = 'trip_rate', hue_col = 'telework', x_label = 'Income Bin', y_label = 'Trips per Person', plt_title = 'Trip Rate by Income Bin')
        trip_rates_purp = plotter.plot_simple_bar_with_dropdown(df  = total_telework_purp, axis_order = list(config['income_label'].values()),dropdown_col = 'd_purpose_category', group_col = 'income_label', value_col = 'trip_rate', hue_col = 'telework', x_label = 'Income Bin', y_label = 'Trips per Person', plt_title = 'Trip Rate by Income Bin')
        txt_trip_rate_income = "Pre-pandemic trends used to show that trip rates increase with income. Current trends show a flatter distribution of trip rates by income. This may be due to a higher share of teleworkers among higher income households, or the increase in e-commerce allowing more errands to be done online."
        txt_trip_rate_purp = "We can explore trip rates among workers who teleworked and those who did not by income and purpose. Those who teleworked tend to make more trips for errands, meals, social/recreation, and escort purposes."
        txt_trip_rate_gender = "We can explore trip rates among workers who teleworked and those who did not by income and gender. Women in higher income households who teleworked had a higher trip rate than those who did not. For males, the trend reverses, with males in lower income households making more trips on telework days. Teleworking is defined as working at least one hour from home on the survey day."
        trip_rate_column = [
                    [("Trip Rates", True)],
                    [self.config['sections']['day_pattern']['text']['trip_rate_overview']],
                    [trip_rate_by_ptype],
                    [self.config['sections']['day_pattern']['text']['trip_rate_income']],
                    [self.config['sections']['day_pattern']['text']['trip_rate_income']],
                    [self.config['sections']['day_pattern']['text']['txt_trip_rate_gender']],
                    [trip_rates_gender],
                    [self.config['sections']['day_pattern']['text']['trip_rate_purp']],
                    [trip_rates_purp]
                ]
        
        return trip_rate_column