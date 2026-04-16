# surveyplotter/sections/day_pattern.py
import yaml
import pandas as pd

class DayPatternSectionBuilder:
    def __init__(self, plotter = None, survey = None, summarizer = None, config = None, survey2 = None):
        ## TODO: Refactor to reduce number of arguments, maybe by passing in a context object that contains all of these (do we need two plotters or is two survey objects sufficient?)
        # If config is a path, load the YAML
        if isinstance(config, str):
            with open(config, 'r') as f:
                self.config = yaml.safe_load(f)
        else:
            self.config = config
        
        self.survey = survey
        self.plotter = plotter
        self.summarizer = summarizer
        self.data = self.survey.trips.merge(self.survey.hh, how = 'left', on=[self.config['global']['id']['hh']]).merge(self.survey.person, how = 'left', on=[self.config['global']['id']['hh'], self.config['global']['id']['person']])
        
        self.data2 = None

        self.survey2 = survey2
        self.compare = self.config['global'].get('compare', False)
        
        
        if self.compare and self.survey2 is None:
            raise ValueError("Comparison is enabled in config but survey2 is not provided.")
        self.compare_order = ['', '']
        if self.survey2 is not None and self.compare:
            self.data2 = self.survey2.trips.merge(self.survey2.hh, how = 'left', on=[self.config['global']['id']['hh']]).merge(self.survey2.person, how = 'left', on=[self.config['global']['id']['hh'], self.config['global']['id']['person']])
            self.data2 = self.data2[self.data2[self.config['global']['columns']['mode']].isin(self.config['global']['orders']['mode_order'])]  # Filter out rows with NA mode  
            self.compare_col = self.config['global'].get('compare_col', None)
            self.compare_order = [self.data[self.compare_col].iloc[0], self.data2[self.compare_col].iloc[0]]

    def build(self, ):
        """
        build mode share summary tab
        """

        # Initialize objects to None
        tour_rate_bubble = None
        trip_per_tour_bubble = None
        trip_rate_bubble = None
        overall_trip_rates = None
        overall_trip_rates_income = None
        overall_trip_rates_purp = None
        trip_rate_by_ptype = None
        trip_rate_by_income = None
        trip_rate_by_purp = None

        tour_rate_bubble2 = None
        trip_per_tour_bubble2 = None
        trip_rate_bubble2 = None
        overall_trip_rates2 = None
        overall_trip_rates_income2 = None
        overall_trip_rates_purp2 = None
        
        # --- Figures (with error handling) ---
        if self.survey.tours is not None:
            try:
                tour_rate = self.survey.tours[self.config['global']['weights']['tour']].sum() / self.survey.person[self.config['global']['weights']['person']].sum()
                tour_rate_bubble = self.plotter.stat_bubble(value=tour_rate, label="Tour Rate", icon_svg='rate')
                if self.compare and self.survey2 is not None:
                    tour_rate2 = self.survey2.tours[self.config['global']['weights']['tour']].sum() / self.survey2.person[self.config['global']['weights']['person']].sum()
                    tour_rate_bubble2 = self.plotter.stat_bubble(value=tour_rate2, label="Tour Rate", icon_svg='rate')
            except Exception as e:
                print(f"Skipping tour_rate_bubble due to error: {e}")

            try:
                trips_per_tour = self.survey.trips[self.config['global']['weights']['trip']].sum() / self.survey.tours[self.config['global']['weights']['tour']].sum()
                trip_per_tour_bubble = self.plotter.stat_bubble(value=trips_per_tour, label="Trips per Tour", icon_svg='rate')
                if self.compare and self.survey2 is not None:
                    trips_per_tour2 = self.survey2.trips[self.config['global']['weights']['trip']].sum() / self.survey2.tours[self.config['global']['weights']['tour']].sum()
                    trip_per_tour_bubble2 = self.plotter.stat_bubble(value=trips_per_tour2, label="Trips per Tour", icon_svg='rate')
            except Exception as e:
                print(f"Skipping trip_per_tour_bubble due to error: {e}")

        try:
            trip_rate = self.survey.trips[self.config['global']['weights']['trip']].sum() / self.survey.person[self.config['global']['weights']['person']].sum()
            trip_rate_bubble = self.plotter.stat_bubble(value=trip_rate, label="Trip Rate", icon_svg='rate')
            if self.compare and self.survey2 is not None:
                trip_rate2 = self.survey2.trips[self.config['global']['weights']['trip']].sum() / self.survey2.person[self.config['global']['weights']['person']].sum()
                trip_rate_bubble2 = self.plotter.stat_bubble(value=trip_rate2, label="Trip Rate", icon_svg='rate')
        except Exception as e:
            print(f"Skipping trip_rate_bubble due to error: {e}")

        try:
            overall_trip_rates = self.summarizer.trip_rates(
                trips=self.data,
                pop=self.survey.person,
                person_group_col=self.config['global']['columns']['person_type'],
                person_weight_col=self.config['global']['weights']['person'],
                trip_weight_col=self.config['global']['weights']['trip']
            )
            if self.compare and self.survey2 is not None:
                overall_trip_rates2 = self.summarizer.trip_rates(
                    trips=self.data2,
                    pop=self.survey2.person,
                    person_group_col=self.config['global']['columns']['person_type'],
                    person_weight_col=self.config['global']['weights']['person'],
                    trip_weight_col=self.config['global']['weights']['trip']
                )
                overall_trip_rates2[self.compare_col] = self.compare_order[1]
                overall_trip_rates[self.compare_col] = self.compare_order[0]
        except Exception as e:
            print(f"Skipping overall_trip_rates due to error: {e}")

        try:
            overall_trip_rates_income = self.summarizer.trip_rates(
                trips=self.data,
                pop=self.survey.person.merge(
                    self.survey.hh[[self.config['global']['id']['hh'], self.config['global']['columns']['income_label']]],
                    how='left',
                    on=self.config['global']['id']['hh']
                ),
                person_group_col=self.config['global']['columns']['income_label'],
                person_weight_col=self.config['global']['weights']['person'],
                trip_weight_col=self.config['global']['weights']['trip']
            )
            if self.compare and self.survey2 is not None:
                overall_trip_rates_income2 = self.summarizer.trip_rates(
                    trips=self.data2,
                    pop=self.survey2.person.merge(
                        self.survey2.hh[[self.config['global']['id']['hh'], self.config['global']['columns']['income_label']]],
                        how='left',
                        on=self.config['global']['id']['hh']
                    ),
                    person_group_col=self.config['global']['columns']['income_label'],
                    person_weight_col=self.config['global']['weights']['person'],
                    trip_weight_col=self.config['global']['weights']['trip']
                )
                overall_trip_rates_income2[self.compare_col] = self.compare_order[1]
                overall_trip_rates_income[self.compare_col] = self.compare_order[0]
        except Exception as e:
            print(f"Skipping overall_trip_rates_income due to error: {e}")

        try:
            overall_trip_rates_purp = self.summarizer.trip_rates(
                trips=self.data,
                pop=self.survey.person,
                trip_group_col=self.config['global']['columns']['purpose'],
                person_weight_col=self.config['global']['weights']['person'],
                trip_weight_col=self.config['global']['weights']['trip']
            )
            if self.compare and self.survey2 is not None:
                overall_trip_rates_purp2 = self.summarizer.trip_rates(
                    trips=self.data2,
                    pop=self.survey2.person,
                    trip_group_col=self.config['global']['columns']['purpose'],
                    person_weight_col=self.config['global']['weights']['person'],
                    trip_weight_col=self.config['global']['weights']['trip']
                )
                overall_trip_rates_purp2[self.compare_col] = self.compare_order[1]
                overall_trip_rates_purp[self.compare_col] = self.compare_order[0]
        except Exception as e:
            print(f"Skipping overall_trip_rates_purp due to error: {e}")

        try:
            if overall_trip_rates is not None:
                trip_rate_by_ptype = self.plotter.plot_simple_bar(
                    df=pd.concat([tr.reset_index(drop=True) for tr in [overall_trip_rates, overall_trip_rates2] if tr is not None],ignore_index=True),
                    group_col=self.config['global']['columns']['person_type'],
                    value_col='trip_rate',
                    x_label='Person Type',
                    y_label='Trips per Person',
                    axis_order=self.config['global']['orders']['person_type_order'],
                    hue_col=self.compare_col if self.compare else None,
                    hue_order=self.compare_order if self.compare else None,
                    plt_title='Trip Rate by Person Type'
                )
        except Exception as e:
            print(f"Skipping trip_rate_by_ptype due to error: {e}")

        try:
            if overall_trip_rates_income is not None:
                trip_rate_by_income = self.plotter.plot_simple_bar(
                    df=pd.concat([tr for tr in [overall_trip_rates_income, overall_trip_rates_income2] if tr is not None],ignore_index=True),
                    group_col=self.config['global']['columns']['income_label'],
                    value_col='trip_rate',
                    x_label='Income Bin',
                    y_label='Trips per Person',
                    axis_order=self.config['global']['orders']['income_order'],
                    hue_col=self.compare_col if self.compare else None,
                    hue_order=self.compare_order if self.compare else None,
                    plt_title='Trip Rate by Income'
                )
        except Exception as e:
            print(f"Skipping trip_rate_by_income due to error: {e}")

        try:
            if overall_trip_rates_purp is not None:
                trip_rate_by_purp = self.plotter.plot_simple_bar(
                    df=pd.concat([tr.reset_index(drop=True) for tr in [overall_trip_rates_purp, overall_trip_rates_purp2] if tr is not None],ignore_index=True),
                    group_col=self.config['global']['columns']['purpose'],
                    value_col='trip_rate',
                    x_label='Purpose',
                    y_label='Trips per Person',
                    hue_col=self.compare_col if self.compare else None,
                    hue_order=self.compare_order if self.compare else None,
                    plt_title='Trip Rate by Purpose'
                )
        except Exception as e:
            print(f"Skipping trip_rate_by_purp due to error: {e}")

        # --- Layout ---
        trip_rate_title = "Day Pattern"
        day_pattern_column = [[(trip_rate_title, True)]]

        rate_bubbles = [b for b in [tour_rate_bubble, trip_rate_bubble, trip_per_tour_bubble] if b is not None]
        rate_bubbles2 = [b for b in [tour_rate_bubble2, trip_rate_bubble2, trip_per_tour_bubble2] if b is not None]
        
        if self.compare:
            if rate_bubbles:
                title1 = "{} Rates".format(self.compare_order[0])
                day_pattern_column.append([title1])
                day_pattern_column.append(rate_bubbles)
            if rate_bubbles2:
                title2 = "{} Rates".format(self.compare_order[1])
                day_pattern_column.append([title2])
                day_pattern_column.append(rate_bubbles2)
        else:
            if rate_bubbles:
                day_pattern_column.append(rate_bubbles)

        try:
            ptype_txt = self.config['sections']['day_pattern']['text']['trip_rate_by_ptype']
        except Exception:
            ptype_txt = "Trip Rate by Person Type"
        if trip_rate_by_ptype is not None:
            day_pattern_column.append([ptype_txt])
            day_pattern_column.append(trip_rate_by_ptype)

        try:
            income_txt = self.config['sections']['day_pattern']['text']['trip_rate_by_income']
        except Exception:
            income_txt = "Trip Rate by Income"
        if trip_rate_by_income is not None:
            day_pattern_column.append([income_txt])
            day_pattern_column.append(trip_rate_by_income)

        try:
            purp_txt = self.config['sections']['day_pattern']['text']['trip_rate_by_purpose']
        except Exception:
            purp_txt = "Trip Rate by Purpose"
        if trip_rate_by_purp is not None:
            day_pattern_column.append([purp_txt])
            day_pattern_column.append(trip_rate_by_purp)

        return day_pattern_column