# surveyplotter/sections/day_pattern.py
import yaml


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

        if self.survey.tours is not None:
             tour_rate = self.survey.tours[self.config['global']['weights']['tour']].sum()/self.survey.person[self.config['global']['weights']['person']].sum()
             tour_rate_bubble = self.plotter.stat_bubble(value = tour_rate, label = "Tour Rate", icon_svg = 'rate')
        trip_rate = self.survey.trips[self.config['global']['weights']['trip']].sum()/self.survey.person[self.config['global']['weights']['person']].sum()
        trip_rate_bubble = self.plotter.stat_bubble(value = trip_rate, label = "Trip Rate", icon_svg = 'rate')
        
        overall_trip_rates = self.summarizer.trip_rates(trips = self.plotter.data_source,
                                                         pop = self.survey.person, 
                                                         person_group_col = self.config['global']['columns']['person_type'],
                                                         person_weight_col = self.config['global']['weights']['person'],
                                                         trip_weight_col = self.config['global']['weights']['trip'])
        overall_trip_rates_income = self.summarizer.trip_rates(trips = self.plotter.data_source, 
                                                               pop = self.survey.person.merge(self.survey.hh[[self.config['global']['id']['hh'],self.config['global']['columns']['income_label']]], 
                                                                                              how = 'left', on = self.config['global']['id']['hh']), 
                                                                                              person_group_col = self.config['global']['columns']['income_label'])
        overall_trip_rates_purp = self.summarizer.trip_rates(trips = self.plotter.data_source,pop = self.survey.person, 
                                                             trip_group_col = self.config['global']['columns']['purpose'],  
                                                             person_weight_col = self.config['global']['weights']['person'], 
                                                             trip_weight_col = self.config['global']['weights']['trip'])
        
        trip_rate_by_ptype = self.plotter.plot_simple_bar(df = overall_trip_rates, group_col = self.config['global']['columns']['person_type'],value_col = 'trip_rate',x_label='Person Type',y_label = 'Trips per Person', axis_order = self.config['global']['orders']['person_type_order'], plt_title = 'Trip Rate by Person Type')
        trip_rate_by_income = self.plotter.plot_simple_bar(df = overall_trip_rates_income, group_col=self.config['global']['columns']['income_label'],value_col = 'trip_rate',x_label='Income Bin',y_label = 'Trips per Person', axis_order = self.config['global']['orders']['income_order'], plt_title = 'Trip Rate by Income')
        trip_rate_by_purp = self.plotter.plot_simple_bar(df = overall_trip_rates_purp, group_col=self.config['global']['columns']['purpose'],value_col = 'trip_rate',x_label='Purpose',y_label = 'Trips per Person', plt_title = 'Trip Rate by Purpose')


        # --- Figures (direct translation from your notebook) ---
        trip_rate_title = "Trip Rates"
        trip_rate_column = [
                    [(trip_rate_title, True)]]
        if self.survey.tours is not None:
                    rate_bubbles = [tour_rate_bubble, trip_rate_bubble]
        else:         rate_bubbles = [trip_rate_bubble]

        trip_rate_column.append(rate_bubbles)
        trip_rate_column.append(trip_rate_by_ptype)
        trip_rate_column.append(trip_rate_by_income)        
        trip_rate_column.append(trip_rate_by_purp)
        
        return trip_rate_column