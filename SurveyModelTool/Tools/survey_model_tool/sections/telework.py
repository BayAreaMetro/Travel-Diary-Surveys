# surveyplotter/sections/telework.py
import yaml
import pandas as pd
class TeleworkSectionBuilder:
    def __init__(self, plotter = None, summarizer = None, survey = None, survey2 = None, config = None):
        self.plotter = plotter
        self.summarizer = summarizer
        self.survey = survey
        self.survey2 = survey2
        self.compare_col = None
        self.compare_order = ['','']
        # If config is a path, load the YAML
        if isinstance(config, str):
            with open(config, 'r') as f:
                self.config = yaml.safe_load(f)
        else:
            self.config = config

        self.data = self.survey.trips.merge(self.survey.hh, how = 'left', on=[self.config['global']['id']['hh']]).merge(self.survey.person, how = 'left', on=[self.config['global']['id']['hh'], self.config['global']['id']['person']]).merge(self.survey.day, how = 'left', on = [self.config['global']['id']['hh'],self.config['global']['id']['person'],self.config['global']['id']['day']])
        self.compare = self.config['global'].get('compare', False)
        self.data2 = None
        if self.compare and self.survey2 is None:
            raise ValueError("Comparison is enabled in config but survey2 is not provided.")
        if self.survey2 is not None and self.compare:
            self.data2 = self.survey2.trips.merge(self.survey2.hh, how = 'left', on=[self.config['global']['id']['hh']]).merge(self.survey2.person, how = 'left', on=[self.config['global']['id']['hh'], self.config['global']['id']['person']]).merge(self.survey2.day, how = 'left', on = [self.config['global']['id']['hh'],self.config['global']['id']['person'],self.config['global']['id']['day']])
            self.compare_col = self.config['global'].get('compare_col', None)
            self.compare_order = [self.data[self.compare_col].iloc[0], self.data2[self.compare_col].iloc[0]]
            self.survey.person[self.compare_col] = self.compare_order[0]
            self.survey2.person[self.compare_col] = self.compare_order[1]

    def build(self, ):
        """
        build mode share summary tab
        """

        # Initialize all figures to None
        fig_telework_ptype_stack = None
        fig_telework_county_stack = None
        fig_telework_income_stack = None
        fig_telework_gender_stack = None
        fig_telework_age_stack = None
        fig_telework_dist_purp = None
        fig_telework_ptype_stack2 = None
        fig_telework_county_stack2 = None
        fig_telework_income_stack2 = None
        fig_telework_gender_stack2 = None
        fig_telework_age_stack2 = None
        fig_telework_dist_purp2 = None
        trip_rate_by_purp = None
        trip_rate_by_purp2 = None

        # --- Figures (with error handling) ---
        try:
            fig_telework_ptype_stack = self.plotter.plot_stacked_bar(df=self.survey.person[self.survey.person[self.config['global']['columns']['worker_flag']] ==1], 
                                                                        weight_col = self.config['global']['weights']['person'],
                                                                        group_col = self.config['global']['columns']['person_type'],
                                                                        stack_col = self.config['global']['columns']['teleworker_flag'],
                                                                        stack_order = self.config['global']['orders']['teleworker'],
                                                                        add_ref = True, x_label = 'Person Type', y_label = "Share of Workers",
                                                                        plt_title = "Telework Status by Person Type {}".format(self.compare_order[0])
                                                                     )

        except Exception as e:
            print(f"Skipping fig_telework_ptype_stack due to error: {e}")

        if self.compare and self.survey2 is not None:
            try:
                fig_telework_ptype_stack2 = self.plotter.plot_stacked_bar(df=self.survey2.person[self.survey2.person[self.config['global']['columns']['worker_flag']] ==1], 
                                                                            weight_col = self.config['global']['weights']['person'],
                                                                            group_col = self.config['global']['columns']['person_type'],
                                                                            stack_col = self.config['global']['columns']['teleworker_flag'],
                                                                            stack_order = self.config['global']['orders']['teleworker'],
                                                                            add_ref = True, x_label = 'Person Type', y_label = "Share of Workers",
                                                                            plt_title = "Telework Status by Person Type {}".format(self.compare_order[1])
                                                                        )

            except Exception as e:
                print(f"Skipping fig_telework_ptype_stack2 due to error: {e}")
            
        try:
            fig_telework_county_stack = self.plotter.plot_stacked_bar(df=self.survey.person[self.survey.person[self.config['global']['columns']['worker_flag']] ==1].merge(self.survey.hh[[self.config['global']['id']['hh'], self.config['global']['columns']['home_county']]], how = 'left', on = self.config['global']['id']['hh']), 
                                                                        weight_col = self.config['global']['weights']['person'],
                                                                        group_col = self.config['global']['columns']['home_county'],
                                                                        stack_col = self.config['global']['columns']['teleworker_flag'],
                                                                        stack_order = self.config['global']['orders']['teleworker'],
                                                                         x_label = 'Person Type', y_label = "Share of Workers",
                                                                        plt_title = "Telwork Status by Person Type {}".format(self.compare_order[0])
                                                                     )

        except Exception as e:
            print(f"Skipping fig_telework_county_stack due to error: {e}")

        if self.compare and self.survey2 is not None:
            try:
                fig_telework_county_stack2 = self.plotter.plot_stacked_bar(df=self.survey2.person[self.survey.person[self.config['global']['columns']['worker_flag']] ==1].merge(self.survey2.hh[[self.config['global']['id']['hh'], self.config['global']['columns']['home_county']]], how = 'left', on = self.config['global']['id']['hh']), 
                                                                            weight_col = self.config['global']['weights']['person'],
                                                                            group_col = self.config['global']['columns']['home_county'],
                                                                            stack_col = self.config['global']['columns']['teleworker_flag'],
                                                                            stack_order = self.config['global']['orders']['teleworker'],
                                                                             x_label = 'Person Type', y_label = "Share of Workers",
                                                                            plt_title = "Telwork Status by Person Type {}".format(self.compare_order[1])
                                                                        )

            except Exception as e:
                print(f"Skipping fig_telework_county_stack2 due to error: {e}")
        
        try:
            fig_telework_income_stack = self.plotter.plot_stacked_bar(df=self.survey.person[self.survey.person[self.config['global']['columns']['worker_flag']] ==1].merge(self.survey.hh[[self.config['global']['id']['hh'], self.config['global']['columns']['income_label']]], how = 'left', on = self.config['global']['id']['hh']),  
                                                                        weight_col = self.config['global']['weights']['person'],
                                                                        group_col = self.config['global']['columns']['income_label'],
                                                                        stack_col = self.config['global']['columns']['teleworker_flag'],
                                                                        stack_order = self.config['global']['orders']['teleworker'],
                                                                        add_ref = True, x_label = 'Person Type', y_label = "Share of Workers",
                                                                        plt_title = "Telwork Status by Income {}".format(self.compare_order[0])
                                                                     )
        except Exception as e:
            print(f"Skipping fig_telework_income_stack due to error: {e}")
        
        if self.compare and self.survey2 is not None:
            try:
                fig_telework_income_stack2 = self.plotter.plot_stacked_bar(df=self.survey2.person[self.survey2.person[self.config['global']['columns']['worker_flag']] ==1].merge(self.survey2.hh[[self.config['global']['id']['hh'], self.config['global']['columns']['income_label']]], how = 'left', on = self.config['global']['id']['hh']), 
                                                                            weight_col = self.config['global']['weights']['person'],
                                                                            group_col = self.config['global']['columns']['income_label'],
                                                                            stack_col = self.config['global']['columns']['teleworker_flag'],
                                                                            stack_order = self.config['global']['orders']['teleworker'],
                                                                            add_ref = True, x_label = 'Person Type', y_label = "Share of Workers",
                                                                            plt_title = "Telwork Status by Income {}".format(self.compare_order[1])
                                                                        )
            except Exception as e:
                print(f"Skipping fig_telework_income_stack2 due to error: {e}")
        
        

        try:
            fig_telework_gender_stack = self.plotter.plot_stacked_bar(df=self.survey.person[self.survey.person[self.config['global']['columns']['worker_flag']] ==1], 
                                                                        weight_col = self.config['global']['weights']['person'],
                                                                        group_col = self.config['global']['columns']['gender'],
                                                                        stack_col = self.config['global']['columns']['teleworker_flag'],
                                                                        stack_order = self.config['global']['orders']['teleworker'],
                                                                        add_ref = True, x_label = 'Person Type', y_label = "Share of Workers",
                                                                        plt_title = "Telwork Status by Gender {}".format(self.compare_order[0])
                                                                     )
        except Exception as e:
            print(f"Skipping fig_telework_income_stack due to error: {e}")
        
        if self.compare and self.survey2 is not None:
            try:
                fig_telework_gender_stack2 = self.plotter.plot_stacked_bar(df=self.survey2.person[self.survey2.person[self.config['global']['columns']['worker_flag']] ==1], 
                                                                            weight_col = self.config['global']['weights']['person'],
                                                                            group_col = self.config['global']['columns']['gender'],
                                                                            stack_col = self.config['global']['columns']['teleworker_flag'],
                                                                            stack_order = self.config['global']['orders']['teleworker'],
                                                                            add_ref = True, x_label = 'Person Type', y_label = "Share of Workers",
                                                                            plt_title = "Telwork Status by Gender {}".format(self.compare_order[1])
                                                                        )
            except Exception as e:
                print(f"Skipping fig_telework_gender_stack2 due to error: {e}")
        
        



        try:
            fig_telework_age_stack = self.plotter.plot_stacked_bar(df=self.survey.person[(self.survey.person[self.config['global']['columns']['age']].isin(self.config['global']['orders']['age_order'])) & (self.survey.person[self.config['global']['columns']['worker_flag']] ==1)],  
                                                                        weight_col = self.config['global']['weights']['person'],
                                                                        group_col = self.config['global']['columns']['age'],
                                                                        stack_col = self.config['global']['columns']['teleworker_flag'],
                                                                        axis_order = self.config['global']['orders']['age_order'],
                                                                        stack_order = self.config['global']['orders']['teleworker'],
                                                                        add_ref = True, x_label = 'Person Type', y_label = "Share of Workers",
                                                                        plt_title = "Telwork Status by Income {}".format(self.compare_order[0])
                                                                     )
        except Exception as e:
            print(f"Skipping fig_telework_age_stack due to error: {e}")
        
        if self.compare and self.survey2 is not None:
            try:
                fig_telework_age_stack2 = self.plotter.plot_stacked_bar(df=self.survey2.person[(self.survey2.person[self.config['global']['columns']['age']].isin(self.config['global']['orders']['age_order'])) & (self.survey2.person[self.config['global']['columns']['worker_flag']] ==1)], 
                                                                            weight_col = self.config['global']['weights']['person'],
                                                                            group_col = self.config['global']['columns']['age'],
                                                                            stack_col = self.config['global']['columns']['teleworker_flag'],
                                                                            axis_order = self.config['global']['orders']['age_order'],
                                                                            stack_order = self.config['global']['orders']['teleworker'],
                                                                            add_ref = True, x_label = 'Person Type', y_label = "Share of Workers",
                                                                            plt_title = "Telwork Status by Income {}".format(self.compare_order[1])
                                                                        )
            except Exception as e:
                print(f"Skipping fig_telework_age_stack2 due to error: {e}")
        
        

        try:
            fig_telework_dist_purp = self.plotter.plot_grouped_dist_with_dropdown(df=self.data[(self.data[self.config['global']['columns']['worker_flag']] ==1)], 
                                                                             dropdown_col=self.config['global']['columns']['purpose'], 
                                                                             hue_col = self.config['global']['columns']['telecommute_flag'],
                                                                             value_col=self.config['global']['columns']['distance'], 
                                                                             plt_title= "Trip Distance by Purpose and Telecommute Flag {}".format(self.compare_order[0]),
                                                                             weight_col = self.config['global']['weights']['trip'])
        except Exception as e:
            print(f"Skipping fig_telework_dist_purp due to error: {e}")

        if self.compare and self.survey2 is not None:
            try:
                fig_telework_dist_purp2 = self.plotter.plot_grouped_dist_with_dropdown(df=self.data2[(self.data2[self.config['global']['columns']['worker_flag']] ==1)], 
                                                                                dropdown_col=self.config['global']['columns']['purpose'], 
                                                                                hue_col = self.config['global']['columns']['telecommute_flag'],
                                                                                value_col=self.config['global']['columns']['distance'], 
                                                                                plt_title= "Trip Distance by Purpose and Telecommute Flag {}".format(self.compare_order[1]),
                                                                                weight_col = self.config['global']['weights']['trip'])
            except Exception as e:
                print(f"Skipping fig_telework_dist_purp2 due to error: {e}")


        try:
            telework_days = self.data[(self.data[self.config['global']['columns']['worker_flag']] == 1) & (self.data[self.config['global']['columns']['telecommute_flag']] == 'Yes')]
            overall_trip_rates_purp_tw = self.summarizer.trip_rates(
                trips= telework_days,
                pop=self.survey.person[self.survey.person[self.config['global']['id']['person']].isin(telework_days[self.config['global']['id']['person']].unique())],
                trip_group_col=self.config['global']['columns']['purpose'],
                person_weight_col=self.config['global']['weights']['person'],
                trip_weight_col=self.config['global']['weights']['trip']
            )
            non_telework_days = self.data[(self.data[self.config['global']['columns']['worker_flag']] == 1) & (self.data[self.config['global']['columns']['telecommute_flag']] == 'No')]
            overall_trip_rates_purp_ntw = self.summarizer.trip_rates(
                trips=non_telework_days,
                pop=self.survey.person[self.survey.person[self.config['global']['id']['person']].isin(non_telework_days[self.config['global']['id']['person']].unique())],
                trip_group_col=self.config['global']['columns']['purpose'],
                person_weight_col=self.config['global']['weights']['person'],
                trip_weight_col=self.config['global']['weights']['trip']
            )
            overall_trip_rates_purp_ntw['Teleworked'] = "No"
            overall_trip_rates_purp_tw['Teleworked'] = "Yes"

            trip_rate_by_purp = self.plotter.plot_simple_bar(
                df=pd.concat([overall_trip_rates_purp_tw,overall_trip_rates_purp_ntw],ignore_index=True),
                group_col=self.config['global']['columns']['purpose'],
                value_col='trip_rate',
                x_label='Purpose',
                y_label='Trips per Person',
                hue_col="Teleworked",
                hue_order=["Yes","No"],
                plt_title='Trip Rate by Purpose'
            )
            
        except Exception as e:
            print(f"Skipping overall_trip_rates_purp due to error: {e}")

        if self.compare and self.survey2 is not None:
                try:
                    telework_days2 = self.data2[(self.data2[self.config['global']['columns']['worker_flag']] == 1) & (self.data2[self.config['global']['columns']['telecommute_flag']] == 'Yes')]
                    overall_trip_rates_purp_tw2 = self.summarizer.trip_rates(
                        trips= telework_days2,
                        pop=self.survey2.person[self.survey2.person[self.config['global']['id']['person']].isin(telework_days2[self.config['global']['id']['person']].unique())],
                        trip_group_col=self.config['global']['columns']['purpose'],
                        person_weight_col=self.config['global']['weights']['person'],
                        trip_weight_col=self.config['global']['weights']['trip']
                    )
                    non_telework_days2 = self.data2[(self.data2[self.config['global']['columns']['worker_flag']] == 1) & (self.data2[self.config['global']['columns']['telecommute_flag']] == 'No')]
                    overall_trip_rates_purp_ntw2 = self.summarizer.trip_rates(
                        trips=non_telework_days,
                        pop=self.survey2.person[self.survey2.person[self.config['global']['id']['person']].isin(non_telework_days2[self.config['global']['id']['person']].unique())],
                        trip_group_col=self.config['global']['columns']['purpose'],
                        person_weight_col=self.config['global']['weights']['person'],
                        trip_weight_col=self.config['global']['weights']['trip']
                    )
                    overall_trip_rates_purp_ntw2['Teleworked'] = "No"
                    overall_trip_rates_purp_tw2['Teleworked'] = "Yes"

                    trip_rate_by_purp2 = self.plotter.plot_simple_bar(
                        df=pd.concat([overall_trip_rates_purp_tw2,overall_trip_rates_purp_ntw2],ignore_index=True),
                        group_col=self.config['global']['columns']['purpose'],
                        value_col='trip_rate',
                        x_label='Purpose',
                        y_label='Trips per Person',
                        hue_col="Teleworked",
                        hue_order=["Yes","No"],
                        plt_title='Trip Rate by Purpose'
                    )
                    
                except Exception as e:
                    print(f"Skipping overall_trip_rates_purp2 due to error: {e}")



        # --- Layout equivalence to your `telework_column` ---
        telework_column = [
            [("Telework Trends",True)],
        ]

        # Row 1: ptype income
        try:
            txt_telework_income = [self.config['sections']['telework']['text']['telework_income']]
        except:
            txt_telework_income = ['Telework Status by Household Income']

        try:
            txt_telework_ptype = [self.config['sections']['telework']['text']['telework_ptype']]
        except:
            txt_telework_ptype = ['Telework Status by Household Income']
        ptype_column = [c for c in [fig_telework_ptype_stack, fig_telework_ptype_stack2] if c is not None]
        if fig_telework_income_stack is not None:
            telework_column.append(txt_telework_ptype)
            telework_column.append(ptype_column)       
        income_column = [c for c in [fig_telework_income_stack, fig_telework_income_stack2] if c is not None]
        if fig_telework_income_stack is not None:
            telework_column.append(txt_telework_income)
            telework_column.append(income_column)
        


        # Row 2: county
        try:
            txt_telework_county = [self.config['sections']['telework']['text']['telework_by_county']]
        except:
            txt_telework_county = ['Telework Status by Home County']
        
        county = [c for c in [fig_telework_county_stack, fig_telework_county_stack2] if c is not None]
        if fig_telework_income_stack is not None:
            telework_column.append(txt_telework_county)
            telework_column.append(county)

        # Row 2: gender/age
        try:
            txt_telework_gender = self.config['sections']['telework']['text']['telework_by_gender']
        except:
            txt_telework_gender = 'Telework Status by Gender'
        try:
            txt_telework_age = self.config['sections']['telework']['text']['telework_by_age']
        except:
            txt_telework_age = 'Telework Status by Age'      
        
        age_gender = [c for c in [fig_telework_gender_stack, fig_telework_gender_stack2, fig_telework_age_stack, fig_telework_age_stack2] if c is not None]
        if len(age_gender) == 4:
            telework_column.append([txt_telework_gender])
            telework_column.append([age_gender[0], age_gender[1]])
            telework_column.append([txt_telework_age])
            telework_column.append([age_gender[2], age_gender[3]])
        elif len(age_gender) ==2:
            telework_column.append([txt_telework_gender, txt_telework_age])
            telework_column.append(age_gender)
        
        # row 3: dist purp

        try:
            txt_dist_purp = [self.config['sections']['telework']['text']['telework_dist_purp']]
        except:
            txt_dist_purp = ['Trip Distance Distribution by Telework Day']
        
        dist_purp = [c for c in [fig_telework_dist_purp, fig_telework_dist_purp2] if c is not None]
        if dist_purp is not None:
            telework_column.append(txt_dist_purp)
            telework_column.append(dist_purp)

        #row 4: trip rates

        try:
            txt_telework_trip_rate = [self.config['sections']['telework']['text']['telework_trip_rates']]
        except:
            txt_telework_trip_rate = ["Trip Rates by Telework Day"]

        trip_rate = [c for c in [trip_rate_by_purp, trip_rate_by_purp2] if c is not None]
        if trip_rate is not None:
            telework_column.append(txt_telework_trip_rate)
            telework_column.append(trip_rate)

        return telework_column