from bokeh.models import ColumnDataSource, Select, CustomJS
from bokeh.plotting import figure
from bokeh.layouts import column
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import pandas as pd
import numpy as np
import io
import base64
from matplotlib.patches import ConnectionPatch
from IPython.display import display
from bokeh.models import ColumnDataSource, HoverTool, FactorRange, Select, CustomJS
from bokeh.plotting import figure   
from bokeh.layouts import column
import scipy

# Default palette used when none is provided
DEFAULT_PALETTE = ['#00CFBB','#417178','#AEE9E8',
                  # '#E74536','#9E483F','#FFDDE2',
                   '#CE57B3','#7F6282','#EFC3DD',
                   '#6E5E52','#C1A67F','#E6D5AB',
                   '#4DB938','#397E58','#BAD739']
"""DEFAULT_PALETTE = [
                   '#417178','#9E483F','#7F6282','#C1A67F','#397E58',
                   '#AEE9E8','#FFDDE2','#EFC3DD','#E6D5AB','#BAD739',
                   '#00CFBB','#E74536', '#CE57B3','#6E5E52', '#4DB938']"""
"""DEFAULT_PALETTE = [
                    '#417178','#C1A67F','#4DB938','#E74536', '#CE57B3','#6E5E52','#EFC3DD', '#7A7D81', '#BAD739','#00CFBB'
          
                   ]"""

"""DEFAULT_PALETTE = [
    '#417178',  # deep teal
    '#E74536',  # vivid red
    '#00CFBB',  # bright aqua
    '#FFDDE2',  # pale pink
    '#4DB938',  # bright green
    '#CE57B3',  # magenta
    '#BAD739',  # lime yellow
    '#7F6282',  # muted purple
    '#EFC3DD',  # light lavender
    '#AEE9E8',  # light aqua
    '#9E483F',  # brick red
    '#C1A67F',  # tan
    '#E6D5AB',  # cream
    '#397E58',  # forest green
    '#6E5E52',  # brown
]"""
"""DEFAULT_PALETTE = [
    '#E74536',  # vivid red
    '#00CFBB',  # bright aqua
    '#BAD739',  # lime yellow
    '#417178',  # deep teal
    '#FFDDE2',  # pale pink
    '#4DB938',  # bright green
    '#CE57B3',  # magenta
    '#AEE9E8',  # light aqua
    '#9E483F',  # brick red
    '#EFC3DD',  # light lavender
    '#397E58',  # forest green
    '#7F6282',  # muted purple
    '#C1A67F',  # tan
    '#E6D5AB',  # cream
    '#6E5E52',  # brown
]"""
class Plotter:
    def plot_simple_bar_with_dropdown(
        self, df, value_col=None, group_col=None, hue_col=None, dropdown_col=None,
        axis_order=None, hue_order=None, dropdown_order=None,
        plt_title=None, x_label=None, y_label=None, palette=None,
        width=800, height=400
    ):
        """
        Create a Bokeh barplot with a dropdown to select dropdown_col values,
        updating the plot via JS callback. Supports grouped bars if hue_col is provided.
        """
        from bokeh.plotting import figure
        from bokeh.models import ColumnDataSource, Select, CustomJS, HoverTool
        from bokeh.layouts import column
        from itertools import islice, cycle
        from matplotlib.colors import to_hex
        from bokeh.models import FactorRange

        data = df.copy()

        if value_col is None:
            raise ValueError("value_col must be specified")
        if group_col is None:
            data = data.reset_index()
            group_col = data.columns[0]
        if dropdown_col is None:
            dropdown_col = group_col

        if axis_order is None:
            axis_order = list(map(str, data[group_col].unique().tolist()))
        if dropdown_order is None:
            dropdown_order = list(map(str, data[dropdown_col].unique().tolist()))
        if plt_title is None:
            plt_title = f"Barplot of {value_col}" if not hue_col else f"Barplot of {value_col} by {hue_col}"
        if x_label is None:
            x_label = group_col.replace('_', ' ').title()
        if y_label is None:
            y_label = value_col.replace('_', ' ').title()
        if palette is None:
            palette = self.default_palette


        initial_dropdown = dropdown_order[0]
        dropdown = Select(
            title=f"Select {dropdown_col.replace('_', ' ').title()}",
            value=initial_dropdown,
            options=[str(o) for o in dropdown_order]
            )
        # ------------------------------------------------------------------
        # Case 1: Grouped bars with hue
        # ------------------------------------------------------------------
        if hue_col:
            if hue_order is None:
                hue_order = list(map(str, data[hue_col].unique().tolist()))
            colors = [to_hex(c) for c in islice(cycle(palette), len(hue_order))]
            color_map = {str(h): colors[i % len(colors)] for i, h in enumerate(hue_order)}

            # Prepare full data with all (group, hue, dropdown) combos
            all_records = []
            for dval in dropdown_order:
                sub = data[data[dropdown_col] == dval]
                for g in axis_order:
                    for h in hue_order:
                        match = sub[(sub[group_col] == g) & (sub[hue_col] == h)]
                        val = match[value_col].iloc[0] if not match.empty else 0
                        all_records.append({
                            dropdown_col: str(dval),
                            group_col: str(g),
                            hue_col: str(h),
                            value_col: val,
                            'color': color_map[str(h)]
                        })

            # Initial selection
            initial_dropdown = dropdown_order[0]
            filtered = [r for r in all_records if r[dropdown_col] == initial_dropdown]

            # Factors = (group, hue)
            x_vals_ordered = [(str(g), str(h)) for g in axis_order for h in hue_order]
            top_vals_ordered = []
            color_vals_ordered = []
            for g, h in x_vals_ordered:
                rec = next((r for r in filtered if r[group_col] == g and r[hue_col] == h), None)
                top_vals_ordered.append(rec[value_col] if rec else 0)
                color_vals_ordered.append(color_map[str(h)])

            # Sources
            source = ColumnDataSource(data=dict(x=x_vals_ordered, top=top_vals_ordered, color=color_vals_ordered))
            all_source = ColumnDataSource(data={
                dropdown_col: [r[dropdown_col] for r in all_records],
                group_col: [r[group_col] for r in all_records],
                hue_col: [r[hue_col] for r in all_records],
                value_col: [r[value_col] for r in all_records],
                'color': [r['color'] for r in all_records]
            })

            # Plot
            p = figure(x_range=FactorRange(*x_vals_ordered), width=width, height=height,
                    title=plt_title, toolbar_location=None)
            p.vbar(x='x', top='top', width=0.7, color='color', source=source)
            # Use x_label for axis and tooltip
            hover = HoverTool(tooltips=[(x_label, '@x'), (y_label, '@top')])
            p.add_tools(hover)
            p.xaxis.axis_label = x_label
            p.yaxis.axis_label = y_label
            p.xaxis.major_label_orientation = 0.7

            # JS callback for hue case
            js_code = """
                var data = source.data;
                var selected = dropdown.value;
                var all_data = all_source.data;
                var axis_order = axis_order_arg;
                var hue_order = hue_order_arg;
                var x = [];
                var top = [];
                var color = [];

                for (var i = 0; i < axis_order.length; i++) {
                    for (var j = 0; j < hue_order.length; j++) {
                        var g = axis_order[i];
                        var h = hue_order[j];
                        var found = false;
                        for (var k = 0; k < all_data[dropdown_col].length; k++) {
                            if (all_data[dropdown_col][k] == selected &&
                                all_data[group_col][k] == g &&
                                all_data[hue_col][k] == h) {
                                x.push([g, h]);
                                top.push(all_data[value_col][k]);
                                color.push(all_data['color'][k]);
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            x.push([g, h]);
                            top.push(0);
                            color.push("lightgray");
                        }
                    }
                }
                data['x'] = x;
                data['top'] = top;
                data['color'] = color;
                source.change.emit();
                p.x_range.factors = x;
            """

            callback = CustomJS(
                args=dict(
                    source=source,
                    all_source=all_source,
                    dropdown=dropdown,
                    p=p,
                    dropdown_col=dropdown_col,
                    group_col=group_col,
                    hue_col=hue_col,
                    value_col=value_col,
                    axis_order_arg=axis_order,
                    hue_order_arg=hue_order
                ),
                code=js_code
            )

        # ------------------------------------------------------------------
        # Case 2: Simple bars (no hue)
        # ------------------------------------------------------------------
        else:
            colors = [to_hex(c) for c in islice(cycle(palette), len(axis_order))]
            all_colors = [colors[axis_order.index(str(x)) % len(colors)] for x in data[group_col]]

            # Initial selection
            initial_dropdown = dropdown_order[0]
            filtered = data[data[dropdown_col] == initial_dropdown]

            # Order x and y
            x_vals_ordered = [str(x) for x in axis_order]
            y_map = dict(zip([str(x) for x in filtered[group_col]], filtered[value_col]))
            y_vals_ordered = [y_map.get(str(x), 0) for x in axis_order]
            bar_colors_ordered = [colors[axis_order.index(str(x)) % len(colors)] for x in axis_order]

            # Sources
            source = ColumnDataSource(data=dict(x=x_vals_ordered, top=y_vals_ordered, color=bar_colors_ordered))
            all_source = ColumnDataSource(data={
                dropdown_col: data[dropdown_col].astype(str).tolist(),
                group_col: data[group_col].astype(str).tolist(),
                value_col: data[value_col].tolist(),
                'color': all_colors
            })

            # Plot
            p = figure(x_range=axis_order, width=width, height=height,
                    title=plt_title, toolbar_location=None)
            p.vbar(x='x', top='top', width=0.7, color='color', source=source)
            # Use x_label for axis and tooltip
            hover = HoverTool(tooltips=[(x_label, '@x'), (y_label, '@top')])
            p.add_tools(hover)
            p.xaxis.axis_label = x_label
            p.yaxis.axis_label = y_label
            p.xaxis.major_label_orientation = 0.7

            # JS callback for simple case
            js_code = """
                var data = source.data;
                var selected = dropdown.value;
                var all_data = all_source.data;
                var axis_order = axis_order_arg;
                var x = [];
                var top = [];
                var color = [];
                var y_map = {};
                var color_map = {};

                for (var i = 0; i < all_data[dropdown_col].length; i++) {
                    if (all_data[dropdown_col][i] == selected) {
                        y_map[all_data[group_col][i]] = all_data[value_col][i];
                        color_map[all_data[group_col][i]] = all_data['color'][i];
                    }
                }
                for (var j = 0; j < axis_order.length; j++) {
                    var key = axis_order[j];
                    x.push(key);
                    top.push(y_map[key] || 0);
                    color.push(color_map[key] || all_data['color'][0]);
                }
                data['x'] = x;
                data['top'] = top;
                data['color'] = color;
                source.change.emit();
                p.x_range.factors = axis_order;
            """

            callback = CustomJS(
                args=dict(
                    source=source,
                    all_source=all_source,
                    dropdown=dropdown,
                    p=p,
                    dropdown_col=dropdown_col,
                    group_col=group_col,
                    value_col=value_col,
                    axis_order_arg=axis_order
                ),
                code=js_code
            )


        dropdown.js_on_change('value', callback)

        return column(dropdown, p)

    
    def plot_simple_bar(self, df, value_col=None, group_col=None, hue_col=None, axis_order=None, hue_order=None, plt_title=None, x_label=None, y_label=None, palette=None, ax=None):
        """
        Plot a simple barplot from a DataFrame with one or two columns (group, value, optional hue).
        Parameters:
            df: DataFrame (index or columns as group, value, optional hue)
            value_col: str, column for bar height (required)
            group_col: str, column for x-axis (if None, use index)
            hue_col: str, column for hue (optional)
            axis_order: list, order of x-axis categories
            hue_order: list, order of hue categories
            plt_title: str, plot title
            x_label: str, x-axis label
            y_label: str, y-axis label
            palette: str or list, color palette
            ax: matplotlib axis (optional)
        Returns:
            fig: matplotlib Figure
        """
        import matplotlib.pyplot as plt
        import seaborn as sns
        import numpy as np
        from itertools import islice, cycle
        from matplotlib.colors import to_hex
        # Prepare data
        data = df.copy()
        if value_col is None:
            raise ValueError("value_col must be specified")
        if group_col is None:
            data = data.reset_index()
            group_col = data.columns[0]
        if axis_order is None:
            axis_order = data[group_col].unique().tolist()
        if plt_title is None:
            plt_title = f"Barplot of {value_col}" if not hue_col else f"Barplot of {value_col} by {hue_col}"
        if x_label is None:
            x_label = group_col.replace('_', ' ').title()
        if y_label is None:
            y_label = value_col.replace('_', ' ').title()
        if palette is None:
            palette = self.default_palette
        # Plot
        if ax is None:
            fig, ax = plt.subplots(figsize=(8, 5))
        else:
            fig = ax.get_figure()
        if hue_col:
            # Ensure hue_col is in the DataFrame
            if hue_col not in data.columns:
                raise ValueError(f"hue_col '{hue_col}' not found in DataFrame")
            # Set up hue order
            if hue_order is None:
                hue_order = data[hue_col].unique().tolist()
            # Set up axis order
            if axis_order is None:
                axis_order = data[group_col].unique().tolist()
            # Set up colors for hue
            colors = list(islice(cycle(palette), len(hue_order)))
            # Plot with hue
            sns.barplot(
                data=data,
                x=group_col,
                y=value_col,
                hue=hue_col,
                order=axis_order,
                hue_order=hue_order,
                ax=ax,
                palette=colors
            )
        else:
            colors = list(islice(cycle(palette), len(axis_order)))
            sns.barplot(
                data=data,
                x=group_col,
                y=value_col,
                order=axis_order,
                ax=ax,
                palette=colors
            )
        ax.set_title(plt_title)
        ax.set_xlabel(x_label)
        ax.set_ylabel(y_label)
        ax.set_xticklabels(axis_order, rotation=45, ha='right')
        # Annotate bars
        for bar in ax.patches:
            height = bar.get_height()
            if height > 0:
                ax.annotate(f'{height:.2f}',
                            xy=(bar.get_x() + bar.get_width() / 2, height),
                            xytext=(0, 3),
                            textcoords="offset points",
                            ha='center', va='bottom')
        # Attach Bokeh spec for dashboard conversion
        try:
            palette_hex = [to_hex(c) for c in colors]
            if hue_col is not None:
                # Prepare data for grouped bar (with hue)
                axis_order_str = [str(x) for x in axis_order]
                hue_order_str = [str(h) for h in hue_order]
                data_records = data[[group_col, hue_col, value_col]].copy()
                data_records[group_col] = data_records[group_col].astype(str)
                data_records[hue_col] = data_records[hue_col].astype(str)
                spec = {
                    'type': 'grouped_bar',
                    'group_col': group_col,
                    'hue_col': hue_col,
                    'axis_order': axis_order_str,
                    'hue_order': hue_order_str,
                    'palette': palette_hex,
                    'title': plt_title,
                    'x_label': x_label,
                    'y_label': y_label,
                    'data': data_records.to_dict('records')
                }
            else:
                spec = {
                    'type': 'bar',
                    'group_col': group_col,
                    'axis_order': axis_order,
                    'palette': palette_hex,
                    'title': plt_title,
                    'x_label': x_label,
                    'y_label': y_label,
                    'data': data[[group_col, value_col]].to_dict('records')
                }
            fig._bokeh_spec = spec
        except Exception:
            pass
        return fig
    
    
    def __init__(self, data_source=None):
        self.data_source = data_source
        self.default_palette = list(DEFAULT_PALETTE)
    
    def plot_grouped_bar_hue(self, df, group_col, hue_col, value_col, axis_order=None, hue_order=None, plt_title=None, x_label=None, y_label=None, palette='tab10'):
        """
        Visualize a pre-grouped DataFrame as a barplot with hue.
        Parameters:
            df: DataFrame (already grouped, weights accounted for)
            group_col: str or list, column(s) for x-axis
            hue_col: str, column for hue
            value_col: str, column for bar height
            axis_order: list, order of x-axis categories
            hue_order: list, order of hue categories
            plt_title: str, plot title
            x_label: str, x-axis label
            y_label: str, y-axis label
            palette: str or list, color palette
        """

        plt.figure(figsize=(10, 6))
        ax = sns.barplot(
            data=df,
            x=group_col,
            y=value_col,
            hue=hue_col,
            order=axis_order,
            hue_order=hue_order,
            palette=palette
        )
        if plt_title:
            ax.set_title(plt_title)
        if x_label:
            ax.set_xlabel(x_label)
        if y_label:
            ax.set_ylabel(y_label)
        # Add percentage labels above bars
        for bar in ax.patches:
            height = bar.get_height()
            if height > 0:
                ax.annotate(f'{height:.2f}',
                            xy=(bar.get_x() + bar.get_width() / 2, height),
                            xytext=(0, 3),
                            textcoords="offset points",
                            ha='center', va='bottom')
        # Set legend position to upper left outside plot
        ax.legend(title=hue_col, loc='upper left', bbox_to_anchor=(1, 1))
        # Rotate x-axis labels for readability
        ax.set_xticklabels(ax.get_xticklabels(), rotation=45, ha='right')
        plt.tight_layout()
        return ax

    def _weighted_share_se(self, df, group_cols, weight_col, subset_filter=None, value_filter=None, value_filter_col=None,  conf_level=0.95, hue_is_tot=True):
        """
        Calculate weighted share, standard error, confidence intervals, and CV for groups in survey data.

        Returns:
            pd.DataFrame: DataFrame with group(s), share, standard error, CI, CV, and reliability.
        """
        # Ensure group_cols is a list
        if isinstance(group_cols, str):
            group_cols = [group_cols]

        temp_df = df.copy()
        if subset_filter is not None:
            temp_df = temp_df[subset_filter(temp_df)]
        if value_filter is not None and value_filter_col is not None:
            temp_df = temp_df[temp_df[value_filter_col] == value_filter]

        temp_df = temp_df[temp_df[weight_col] > 0].copy()

        # Calculate total weight for each "main" group (e.g., income_broad)
        main_group = group_cols[0]
        sub_group = group_cols[1] if len(group_cols) > 1 else None
        if len(group_cols) == 1:
            # Only one group column: share is from total weight
            total_weight = temp_df[weight_col].sum()
            grouped = temp_df.groupby(group_cols)[weight_col].sum().reset_index()
            grouped['share'] = grouped[weight_col] / total_weight
        elif hue_is_tot:
            # Multiple group columns: share is from group weight
            total_weights = temp_df.groupby(main_group)[weight_col].sum().rename('total_weight').reset_index()
            grouped = temp_df.groupby(group_cols)[weight_col].sum().reset_index()
            grouped = grouped.merge(total_weights, on=main_group, how='left')
            grouped['share'] = grouped[weight_col] / grouped['total_weight']
        else:
            total_weights = temp_df.groupby(sub_group)[weight_col].sum().rename('total_weight').reset_index()
            grouped = temp_df.groupby(group_cols)[weight_col].sum().reset_index()
            grouped = grouped.merge(total_weights, on=sub_group, how='left')
            grouped['share'] = grouped[weight_col] / grouped['total_weight']
        # For each subgroup, calculate error_summation within the main group
        error_stats = []
        for _, row in grouped.iterrows():
            mask = temp_df[main_group] == row[main_group]
            sub_df = temp_df[mask].copy()
            sub_df['standard_weight'] = sub_df[weight_col] / sub_df[weight_col].sum()
            error_summation = (sub_df['standard_weight'] ** 2).sum()
            error_stats.append(error_summation)
        grouped['error_summation'] = error_stats

        z = 1.96 if conf_level == 0.95 else scipy.stats.norm.ppf(1 - (1 - conf_level) / 2)
        grouped['standard_error'] = np.sqrt(grouped['share'] * (1 - grouped['share']) * grouped['error_summation'])
        grouped['ci'] = z * grouped['standard_error']
        grouped['lower_bound'] = np.maximum(grouped['share'] - grouped['ci'], 0)
        grouped['upper_bound'] = grouped['share'] + grouped['ci']
        grouped['cv'] = (grouped['standard_error'] / grouped['share']) * 100

        def reliability(cv):
            if np.isnan(cv):
                return "Low"
            elif cv < 15:
                return "High"
            elif 15 <= cv < 30:
                return "Medium"
            else:
                return "Low"
        grouped['est_reliability'] = grouped['cv'].apply(reliability)

        return grouped[group_cols + ['share', 'standard_error', 'ci', 'lower_bound', 'upper_bound', 'cv', 'est_reliability']]

    def plot_share_hue(self, df = None, group_cols = ['income_broad','managed_lane_use'], hue = None, share_is_bin =False, weight_col = 'trip_weight',
                        axis_order = None, sub_axis_order = None,
                        plt_title = None, x_label = None, y_label = None, ax = None, rel_legend = True, palette = None):
        # Calculate weighted sums by income and managed lane use
        hatch_map = {
            "High": "",
            "Medium": "//",
            "Low": "xx"
        }
        if df is None:
            filtered_trips = self.data_source
        else:
            filtered_trips = df.copy()
        if axis_order is None:
            axis_order = df[group_cols[0]].unique().tolist()
        if sub_axis_order is None:
            sub_axis_order = df[group_cols[1]].unique().tolist()
        if hue is None:
            hue = group_cols[1]
        if x_label is None:
            x_label = group_cols[0].replace('_', ' ').title()

        # If share_is_bin is True, we will plot the share of each bin within the main group
        hue_is_tot = False
        if share_is_bin:
            weighted_tot = group_cols[0]
            hue_is_tot = True
            if y_label is None:
                y_label = 'Share of Trips in each {}'.format(group_cols[0].replace('_', ' ').title())
            if palette is None:
                palette = self.default_palette
            if plt_title is None:
                plt_title = f"Share of {group_cols[1].replace('_', ' ').title()} in each {group_cols[0].replace('_', ' ').title()}"    
        else:
            weighted_tot = group_cols[1]
            if y_label is None:
                y_label = 'Share of Trips'
            if palette is None:
                palette = self.default_palette
            if plt_title is None:
                plt_title = f"{group_cols[0].replace('_', ' ').title()} Distribution by {group_cols[1].replace('_', ' ').title()}"    

        # --- Ensure all combinations exist using MultiIndex ---
        # Get all possible values for each group column
        all_groups = [axis_order if group_cols[0] == 'income_broad' else sorted(filtered_trips[group_cols[0]].unique())]
        all_groups += [sorted(filtered_trips[group_cols[1]].unique())]
        all_combos = pd.MultiIndex.from_product(all_groups, names=group_cols)
        # Group and aggregate, then reindex to fill missing combos with zero
        weighted = filtered_trips.groupby(group_cols)[weight_col].sum().reindex(all_combos, fill_value=0).reset_index()
        # Calculate total weighted trips per income group
        totals = weighted.groupby(weighted_tot)[weight_col].sum().reset_index()
        totals = totals.rename(columns={weight_col: 'total_weight'})
        # Merge totals back to main df and calculate share
        weighted = weighted.merge(totals, on=weighted_tot)
        weighted['share'] = weighted[weight_col] / weighted['total_weight']
        order = axis_order

        raw_counts = filtered_trips.groupby(group_cols).size().reindex(all_combos, fill_value=0).reset_index(name='count')
        print("Number of Survey Records")
        print(raw_counts.pivot(index=group_cols[0], columns=group_cols[1], values='count').fillna(0).reindex(order))

        results = self._weighted_share_se(
            df=filtered_trips,
            group_cols=group_cols,
            weight_col=weight_col,
            hue_is_tot=hue_is_tot
        )
        results[group_cols[0]] = pd.Categorical(results[group_cols[0]], categories=order, ordered=True)
        results = results.sort_values(by=group_cols).reset_index(drop=True)
        # --- End MultiIndex step ---

        # Plot normalized shares
        if ax is None:
            fig, ax = plt.subplots(figsize=(10, 5))
        sns.barplot(
            data=weighted,
            x=group_cols[0],
            y='share',
            hue=hue,
            hue_order=sub_axis_order,
            order=order,
            ax=ax,
            palette=palette
        )
        hue_order = [t.get_text() for t in ax.get_legend().texts]
        # Get the data used for plotting
        plot_data = weighted.copy()
        plot_data[group_cols[0]] = plot_data[group_cols[0]].astype(str)
        plot_data[group_cols[1]] = plot_data[group_cols[1]].astype(str)

        hatches = []
        
        for val in sub_axis_order:
            hatches = hatches + list(results[results[group_cols[1]] == val].est_reliability.map(hatch_map))

        # Loop through bars and match to data by position
        num_main = len(axis_order)
        num_hue = len(hue_order)
        for i, bar in enumerate(ax.patches):
            height = bar.get_height()
            if height == 0:
                continue  # Skip empty bars
            x = bar.get_x() + bar.get_width() /2
            main_idx = i // num_hue
            hue_idx = i % num_hue
            main_cat = axis_order[main_idx]
            hue_cat = hue_order[hue_idx]
            # print(f"Bar {i}: main_cat={main_cat}, hue_cat={hue_cat}")

            # Convert hue_cat to boolean if needed
            if pd.api.types.is_bool_dtype(results[group_cols[1]]):
                if hue_cat == "True":
                    hue_val_match = True
                elif hue_cat == "False":
                    hue_val_match = False
                else:
                    hue_val_match = hue_cat
            else:
                hue_val_match = hue_cat

            # Find the matching row in results
            results['match'] = results['share'].map(lambda x: np.abs(x - height) < 1e-5)
            match = results[results['match']]
            # print(f"Matching for main_cat: {main_cat}, hue_val: {hue_val_match}")
            # print(f"Match found: {match}")
            if not match.empty:
                ci = match['ci'].values[0]
                ax.errorbar(x, height, yerr=ci, color='black', capsize=4, fmt='none')
                # Set hatch based on reliability, fallback to empty string if not found
                est_rel = match['est_reliability'].values[0]
                # print(f"Setting hatch for bar at x={x}, height={height}, est_rel={est_rel}")
                bar.set_hatch(hatch_map.get(est_rel, ""))
                bar.set_edgecolor('grey')
            else:
                print(f"No match found for cat: {height}, hue: {hue_val_match}")
                bar.set_hatch("")  # Default hatch for missing data
            # Annotate bar only if not zero
            ax.annotate(f'{height*100:.1f}%',
                        xy=(x, height + ci + 0.001),  # Offset above the bar
                        xytext=(0, 3),
                        textcoords="offset points",
                        ha='center', va='bottom')

        ax.set_title(plt_title)
        ax.set_xlabel(x_label)
        ax.set_ylabel(y_label)
        ax.set_xticklabels(axis_order, rotation=45, ha='right')

        # Combine hue and reliability legend handles into a single legend
        handles, labels = ax.get_legend_handles_labels()
        # Build clean hue legend (no hatches) using proxy patches
        try:
            if palette is None:
                from itertools import islice, cycle
                hue_colors = list(islice(cycle(self.default_palette), len(hue_order)))
            elif isinstance(palette, str):
                hue_colors = sns.color_palette(palette, n_colors=len(hue_order))
            else:
                from itertools import islice, cycle
                hue_colors = list(islice(cycle(palette), len(hue_order)))
        except Exception:
            from itertools import islice, cycle
            hue_colors = list(islice(cycle(self.default_palette), len(hue_order)))
        hue_handles = [
            mpatches.Patch(facecolor=hue_colors[i % len(hue_colors)], edgecolor='grey', label=str(hue_order[i]))
            for i in range(len(hue_order))
        ]
        # Reliability legend entries (hatched)
        reliability_patches = [
            mpatches.Patch(facecolor='gray', hatch=hatch_map[rel], label=f'{rel} Reliability', edgecolor='k')
            for rel in ['High', 'Medium', 'Low']
        ]
        # Replace seaborn's legend with our custom combined legend
        if ax.get_legend() is not None:
            try:
                ax.legend_.remove()
            except Exception:
                pass
        combined_handles = hue_handles + (reliability_patches if rel_legend else [])
        ax.legend(handles=combined_handles, loc='upper left', bbox_to_anchor=(1, 1), title=hue.replace('_', ' ').title())
        # Attach Bokeh spec for interactive conversion in dashboard (include reliability)

        try:
            # Add error bars to bokeh spec for plot_share_hue only
            data_for_spec = results[[group_cols[0], group_cols[1], 'share', 'lower_bound', 'upper_bound']].copy()
            rel_cols = [group_cols[0], group_cols[1], 'est_reliability']
            rel_map = results[rel_cols].rename(columns={'est_reliability': 'reliability'})
            data_for_spec = data_for_spec.merge(rel_map, on=[group_cols[0], group_cols[1]], how='left')
            # Build a palette list for the hue values
            from matplotlib.colors import to_hex
            if palette is None:
                from itertools import islice, cycle
                hue_colors = list(islice(cycle(self.default_palette), len(sub_axis_order)))
            elif isinstance(palette, str):
                hue_colors = sns.color_palette(palette, n_colors=len(sub_axis_order))
            else:
                hue_colors = palette
            palette_hex = [to_hex(c) for c in hue_colors]
            fig._bokeh_spec = {
                'type': 'grouped_bar',
                'group_col': group_cols[0],
                'hue_col': hue,
                'axis_order': order,
                'hue_order': sub_axis_order,
                'data': data_for_spec.to_dict('records'),
                'palette': palette_hex,
                'title': plt_title or '',
                'x_label': x_label or group_cols[0],
                'y_label': y_label or 'Share of Trips',
            }
        except Exception:
            pass
        # if ax is None:
        #     plt.show()
        return fig


            
    def plot_share(self, df=None, group_cols=['income_broad'], weight_col='trip_weight',
                  axis_order=None, plt_title=None, x_label=None, y_label='Share of Trips', ax=None, rel_legend=True, palette = None):
        """
        Plot share of each group normalized to the total sum (all bars sum to 100%).
        This version is for a single categorical variable (no hue).
        """
        hatch_map = {
            "High": "",
            "Medium": "//",
            "Low": "xx"
        }
        # Ensure group_cols is a list with one element
        if isinstance(group_cols, str):
            group_cols = [group_cols]
        if len(group_cols) != 1:
            raise ValueError("group_cols must contain exactly one column for this plot type.")

        # Filter trips based on the provided DataFrame or use the data source
        if df is None:
            filtered_trips = self.data_source
        else:
            filtered_trips = df.copy()
        if axis_order is None:
            axis_order = filtered_trips[group_cols[0]].unique().tolist()

        if plt_title is None:
            plt_title = f"Share of Trips by {group_cols[0].replace('_', ' ').title()}"
        if x_label is None:
            x_label = group_cols[0].replace('_', ' ').title()
        
        if palette is None:
            palette = self.default_palette

        # --- Ensure all combinations exist using MultiIndex ---
        all_groups = [axis_order if group_cols[0] == 'income_broad' else sorted(filtered_trips[group_cols[0]].unique())]
        # all_combos = pd.MultiIndex.from_product(all_groups, names=group_cols)
        # Group and aggregate, then reindex to fill missing combos with zero
        group_weighted = filtered_trips.groupby(group_cols)[weight_col].sum().reset_index()
        total_weight = group_weighted[weight_col].sum()
        group_weighted['share'] = group_weighted[weight_col] / total_weight
        group_weighted['share'] = group_weighted['share'].fillna(0)
        order = axis_order

        # Print raw counts for reference
        group_raw_counts = filtered_trips.groupby(group_cols).size().reset_index(name='count')
        print("Number of Survey Records")
        print(group_raw_counts)

        # Use overall sample for CI and reliability
        results = self._weighted_share_se(
            df=filtered_trips,
            group_cols=group_cols,
            weight_col=weight_col
        )
        results[group_cols[0]] = pd.Categorical(results[group_cols[0]], categories=order, ordered=True)
        results = results.sort_values(by=group_cols).reset_index(drop=True)

        # Plot normalized shares
        if ax is None:
            fig, ax = plt.subplots(figsize=(10, 5))
        sns.barplot(
            data=group_weighted,
            x=group_cols[0],
            y='share',
            order=order,
            ax=ax,
            palette = palette
        )

        # Get the data used for plotting
        plot_data = group_weighted.copy()
        plot_data[group_cols[0]] = plot_data[group_cols[0]].astype(str)

        # Set hatches based on reliability
        hatches = []
        for val in results[group_cols[0]].unique():
            hatches = hatches + list(results[results[group_cols[0]] == val].est_reliability.map(hatch_map))

        # Loop through bars and match to data by position
        for i, bar in enumerate(ax.patches):
            height = bar.get_height()
            if height == 0:
                continue  # Skip empty bars
            x = bar.get_x() + bar.get_width() / 2
            # Find the category for this bar
            cat_idx = i
            cat = order[cat_idx]
            # Find the matching row in results
            match = results[(results[group_cols[0]] == cat)]
            if not match.empty:
                ci = match['ci'].values[0]
                ax.errorbar(x, height, yerr=ci, color='black', capsize=4, fmt='none')
                # Set hatch based on reliability, fallback to empty string if not found
                est_rel = match['est_reliability'].values[0]
                bar.set_hatch(hatch_map.get(est_rel, ""))
                bar.set_edgecolor('grey')
            else:
                print(f"No match found for cat: {cat}")
                bar.set_hatch("")  # Default hatch for missing data
            # Annotate bar only if not zero
            ax.annotate(f'{height*100:.1f}%',
                        xy=(x, height + ci + 0.001),
                        xytext=(0, 3),
                        textcoords="offset points",
                        ha='center', va='bottom')

        ax.set_title(plt_title)
        ax.set_xlabel(x_label)
        ax.set_ylabel(y_label)
        ax.set_xticklabels(order, rotation=45, ha='right')

        # Only show reliability legend (no hue legend), combine if any existing handles
        handles, labels = ax.get_legend_handles_labels()
        reliability_patches = [
            mpatches.Patch(facecolor='gray', hatch=hatch_map[rel], label=f'{rel} Reliability', edgecolor='k')
            for rel in ['High', 'Medium', 'Low']
        ]
        if rel_legend:
            handles += reliability_patches
        if handles:
            ax.legend(handles=handles, loc='best', title='Estimate Reliability')
        # Attach Bokeh spec for interactive conversion in dashboard (include reliability)
        try:
            # Merge lower/upper bounds and reliability into the Bokeh spec data
            gw = group_weighted[[group_cols[0], 'share']].copy()
            bounds_map = results[[group_cols[0], 'lower_bound', 'upper_bound']]
            rel_map = results[[group_cols[0], 'est_reliability']].rename(columns={'est_reliability': 'reliability'})
            gw = gw.merge(bounds_map, on=group_cols[0], how='left')
            gw = gw.merge(rel_map, on=group_cols[0], how='left')
            # Build a palette for Bokeh (list of hex colors)
            from matplotlib.colors import to_hex
            if palette is None:
                # Use first color of default palette
                color_list = [self.default_palette[0]]
            elif isinstance(palette, str):
                color_list = [sns.color_palette(palette, n_colors=1)[0]]
            else:
                # assume list/tuple of colors; take at least one
                color_list = [palette[0]] if len(palette) > 0 else [self.default_palette[0]]
            palette_hex = [to_hex(c) for c in color_list]
            fig._bokeh_spec = {
                'type': 'bar',
                'group_col': group_cols[0],
                'axis_order': order,
                'data': gw.to_dict('records'),
                'palette': palette_hex,
                'title': plt_title or '',
                'x_label': x_label or group_cols[0],
                'y_label': y_label or 'Share of Trips',
            }
        except Exception:
            pass
        # if ax is None:
        #     plt.show()
        return fig

    def plot_share_pie(self, df=None, group_col='income_broad', weight_col='trip_weight', axis_order=None, plt_title=None, legend_loc='best', legend_bbox=None, figsize=(8, 8), palette=None):
        """
        Plot a pie chart of share of each group normalized to the total sum.
        This version is for a single categorical variable (no hue).
        """
        if df is None:
            filtered_trips = self.data_source
        else:
            filtered_trips = df.copy()
        if axis_order is None:
            axis_order = filtered_trips[group_col].unique().tolist()
        if plt_title is None:
            plt_title = f"Share of Trips by {group_col.replace('_', ' ').title()}"
        if palette is None:
            # Repeat or slice default palette to match needed length
            from itertools import islice, cycle
            colors = list(islice(cycle(self.default_palette), len(axis_order)))
        elif isinstance(palette, str):
            colors = sns.color_palette(palette, n_colors=len(axis_order))
        else:
            colors = palette
        # Aggregate weights
        group_weighted = filtered_trips.groupby(group_col)[weight_col].sum().reindex(axis_order, fill_value=0)
        total_weight = group_weighted.sum()
        shares = group_weighted / total_weight
        # Pie chart
        fig, ax = plt.subplots(figsize=figsize)
        wedges, texts, autotexts = ax.pie(
            shares,
            labels=axis_order,
            autopct="%.1f%%",
            startangle=90,
            counterclock=False,
            colors=colors
        )
        ax.set_title(plt_title)
        # Optional legend
        if legend_loc:
            ax.legend(wedges, axis_order, loc=legend_loc, bbox_to_anchor=legend_bbox)
        plt.tight_layout()
        # plt.show()
        return fig

    def show_side_by_side(self, fig1, fig2, figsize=(16, 8), titles=(None, None)):
        """
        Display two matplotlib figures side by side in a new figure.
        Args:
            fig1: First matplotlib Figure object
            fig2: Second matplotlib Figure object
            figsize: Tuple for the size of the combined figure
            titles: Tuple of titles for each subplot
        """
        import matplotlib.pyplot as plt
        from matplotlib.backends.backend_agg import FigureCanvasAgg
        import numpy as np

        # Render fig1 and fig2 to numpy arrays
        def fig_to_array(fig):
            canvas = FigureCanvasAgg(fig)
            canvas.draw()
            buf = canvas.buffer_rgba()
            arr = np.asarray(buf)
            return arr

        arr1 = fig_to_array(fig1)
        arr2 = fig_to_array(fig2)

        # Create a new figure with two subplots
        fig, axes = plt.subplots(1, 2, figsize=figsize)
        axes[0].imshow(arr1)
        axes[0].axis('off')
        if titles[0]:
            axes[0].set_title(titles[0])
        axes[1].imshow(arr2)
        axes[1].axis('off')
        if titles[1]:
            axes[1].set_title(titles[1])
        plt.tight_layout()
        # plt.show()
        return fig

    def plot_concentric_pie(self,
        df,
        group_col,
        period_col,
        weight_col,
        period_order=None,
        bin_order=None,
        plt_title=None,
        outer_label_fmt="{share:.1%}",
        legend_loc='center left',
        legend_bbox=(1, 0.5),
        figsize=(10, 8),
        color_palette=None
    ):
        """
        Plots a concentric pie chart for two periods (e.g., Pre/Post) for a categorical variable.

        Parameters:
            df: DataFrame with columns [group_col, period_col, value_col]
            group_col: str, column for pie wedges (e.g., 'mode_brief', 'commute_bin')
            period_col: str, column for period (e.g., 'period')
            period_order: list, order of periods (default: sorted unique)
            bin_order: list, order of bins (default: sorted unique)
            plt_title: str, plot title
            outer_label_fmt: str, format for outer labels
            legend_loc: str, legend location
            legend_bbox: tuple, legend bbox_to_anchor
            figsize: tuple, figure size
            color_palette: list, matplotlib color list (default: tab20)
        """
        # Determine periods and bins
        if period_order is None:
            period_order = sorted(df[period_col].unique())
        if bin_order is None:
            bin_order = sorted(df[group_col].unique())
        if color_palette is None:
            color_palette = list(self.default_palette)
        if len(period_order) != 2:
            raise ValueError("period_order must contain exactly two periods for concentric pie chart.")
        if plt_title is None:
            plt_title = f"Share of {group_col.replace('_', ' ').title()} by {period_col.replace('_', ' ').title()}"

        # Aggregate and calculate shares
        df = df.groupby([group_col, period_col], as_index=False)[[weight_col]].sum()
        df['share'] = df.groupby(period_col)[weight_col].transform(lambda x: x / x.sum())

        bin_color_map = {bin: color_palette[i % len(color_palette)] for i, bin in enumerate(bin_order)}
        wedge_colors = [bin_color_map[bin] for bin in bin_order]

        # Get shares for each period, fill missing bins with 0
        shares = {}
        for period in period_order:
            shares[period] = (
                df[df[period_col] == period]
                .set_index(group_col)
                .reindex(bin_order, fill_value=0)['share']
                .values
            )

        fig, ax = plt.subplots(figsize=figsize)

        # Outer ring (latest period)
        outer_labels = [
            outer_label_fmt.format( share=share)
            for  share in shares[period_order[1]]
        ]
        wedges, _ = ax.pie(
            shares[period_order[1]],
            labels=None,
            radius=1,
            wedgeprops=dict(width=0.3, edgecolor='w'),
            labeldistance=1.15,
            autopct=None,
            startangle=90,
            colors=wedge_colors
        )

        # Inner ring (earlier period)
        inner_labels = [
            outer_label_fmt.format( share=share)
            for  share in shares[period_order[0]]
        ]
        wedges2, _ = ax.pie(
            shares[period_order[0]],
            labels=None,
            radius=0.7,
            wedgeprops=dict(width=0.3, edgecolor='w'),
            labeldistance=0.6,
            autopct=None,
            startangle=90,
            colors=wedge_colors
        )

        threshold = 0.02  # If share < 5%, use a line
        for i, (wedge, share, label) in enumerate(zip(wedges, shares[period_order[1]], outer_labels)):
            ang = (wedge.theta2 + wedge.theta1) / 2.
            x = np.cos(np.deg2rad(ang))
            y = np.sin(np.deg2rad(ang))
            r = 1.15  # label distance
            if share < threshold:
                # Place label further out and draw a line
                ax.text(r*x, r*y, label, ha='center', va='center', fontsize=10)
                line = ConnectionPatch(
                    xyA=(0.95*x, 0.95*y), xyB=(r*x, r*y),
                    coordsA="data", coordsB="data",
                    axesA=ax, axesB=ax, color="gray", lw=1
                )
                ax.add_artist(line)
            else:
                ax.text(0.95*x, 0.95*y, label, ha='center', va='center', fontsize=10)

        for i, (wedge, share, label) in enumerate(zip(wedges2, shares[period_order[0]], inner_labels)):
            ang = (wedge.theta2 + wedge.theta1) / 2.
            x = np.cos(np.deg2rad(ang))
            y = np.sin(np.deg2rad(ang))
            r = 0.6 # label distance
            r2 = 1.07
            if share < threshold:
                ax.text(r2*x, r2*y, label, ha='center', va='center', fontsize=10)
                line = ConnectionPatch(
                    xyA=(r*x, r*y), xyB=(r2*x, r2*y),
                    coordsA="data", coordsB="data",
                    axesA=ax, axesB=ax, color="gray", lw=1
                )
                ax.add_artist(line)
            else:
                ax.text(r*x, r*y, label, ha='center', va='center', fontsize=10)

        col1 = period_order[0]
        col2 = period_order[1]
        if isinstance(col1, np.bool_):
            col1 = period_col + " " + str(col1)
        if isinstance(col2, np.bool_):
            col2 = period_col + " " + str(col2)
        # Legend for inner ring
        legend_labels = [
            f"{bin} "
            for bin in bin_order
        ]
        plt.legend(wedges, legend_labels, loc=legend_loc, bbox_to_anchor=legend_bbox,
                   title=f"Inner: {col1}\nOuter: {col2}")

        plt.title(plt_title)
        plt.tight_layout()
        # plt.show()

        summary_df = pd.DataFrame({
            group_col.replace('_', ' ').title(): bin_order,
            col1: shares[period_order[0]],
            col2: shares[period_order[1]]
        })
        summary_df = summary_df.sort_values(by=col1, ascending=False)
        display(summary_df.style.format({col: "{:.1%}" for col in summary_df.columns[1:]}))

        return fig



    def bokeh_dashboard(self, dashboard_tabs, map_width = None, force_png = False, save_html = False, html_filename = None):
        """
        Create a Bokeh dashboard with multiple tabs, each with two columns:
        - Left: a plot (Bokeh figure, folium map, or pre-made plot)
        - Right: a text explanation
        """
        import re
        import io
        import base64
        try:
            import folium
        except ImportError:
            folium = None
        from bokeh.models import Div, TabPanel, Tabs
        from bokeh.layouts import column, row
        from bokeh.io import show

        # Modern cell style: padding, rounded corners, box-shadow, white bg for plots, subtle bg for text
        def render_cell(cell, width, map_width = None, n_cols = None, cell_style=None, is_heading: bool = False):
            from bokeh.models.layouts import LayoutDOM
            if isinstance(cell, LayoutDOM):
                return cell
            # Handle pandas DataFrame as a Bokeh DataTable
            import pandas as pd
            # Allow a simple wrapper to mark headings per cell:
            # - dict: { 'text': '...', 'heading': True } or { 'html': '...', 'heading': False }
            # - tuple/list: ('...', True)
            if isinstance(cell, dict) and (('text' in cell) or ('html' in cell)):
                content = cell.get('html') if 'html' in cell else cell.get('text')
                heading_flag = bool(cell.get('heading', False))
                return render_cell(content, width, map_width=map_width, n_cols=n_cols, cell_style=cell_style, is_heading=heading_flag)
            if isinstance(cell, (tuple, list)) and len(cell) == 2 and isinstance(cell[0], str) and isinstance(cell[1], bool):
                return render_cell(cell[0], width, map_width=map_width, n_cols=n_cols, cell_style=cell_style, is_heading=cell[1])
            try:
                from bokeh.models import DataTable, TableColumn, ColumnDataSource
            except ImportError:
                DataTable = TableColumn = ColumnDataSource = None
            if DataTable and isinstance(cell, pd.DataFrame):
                if cell.empty:
                    return Div(text='<div style="color:#fff;">(Empty table)</div>', styles=cell_style)
                source = ColumnDataSource(cell)
                columns = []
                from bokeh.models.widgets import NumberFormatter
                from bokeh.models import HTMLTemplateFormatter
                for col in cell.columns:
                    # Use comma separator, no decimals for numbers; else default
                    if pd.api.types.is_numeric_dtype(cell[col]):
                        formatter = NumberFormatter(format="0,0")
                        columns.append(TableColumn(field=str(col), title=str(col), formatter=formatter))
                    else:
                        columns.append(TableColumn(field=str(col), title=str(col)))
                # Compute a natural height so the table fits its content without large empty space
                visible_rows = len(cell)
                max_visible_rows = 18  # cap to avoid overly tall tables
                visible_rows = min(visible_rows, max_visible_rows)
                header_px = 40
                row_px = 34
                table_height = header_px + (visible_rows * row_px)

                # Custom CSS for larger font
                table = DataTable(
                    source=source,
                    columns=columns,
                    width=width//n_cols if n_cols else 600,
                    height=table_height,
                    row_height=row_px,
                    sizing_mode='stretch_width',
                    styles=cell_style,
                    css_classes=["custom-table-style"]
                )
                # Add custom CSS to the page for larger font
                from bokeh.io import curdoc
                style_tag = """
                <style>
                .custom-table-style .slick-header-column {
                    font-size: 24px !important;
                    font-weight: bold !important;
                    line-height: 28px !important;
                }
                .custom-table-style .slick-cell {
                    font-size: 22px !important;
                    line-height: 26px !important;
                }
                </style>
                """
                curdoc().template_variables["custom_table_style"] = style_tag
                return table
            # Handle folium map, fixed width and true centering
            if folium and hasattr(cell, '_repr_html_') and 'folium' in str(type(cell)).lower():
                if not map_width is None:
                    map_size = map_width
                    print("using custom map width: {}".format(map_size))
                else:
                    map_size = width
                map_pixel_width = int(map_size * (n_cols + 1) / 2)
                map_pixel_height = int(map_size * (n_cols + 1) / 2 / 2)
                f = folium.Figure(width=map_pixel_width, height=map_pixel_height)
                cell = cell.add_to(f)
                html = cell._repr_html_()
                centered_html = f"<div style='margin:0 auto; width:{map_pixel_width}px'>{html}</div>"
                return Div(text=centered_html, width=map_pixel_width, styles=cell_style)
            # Handle matplotlib Figure (try interactive conversion if spec present)
            elif hasattr(cell, 'figure') and hasattr(cell.figure, 'canvas'):
                try:
                    spec = getattr(cell, '_bokeh_spec', None)
                except Exception:
                    spec = None
                if spec and not force_png:
                    try:
                        if spec['type'] == 'bar':
                            p = self.seaborn_to_bokeh(cell)
                        elif spec['type'] == 'grouped_bar':
                            p = self.seaborn_to_bokeh_hue(cell)
                        elif spec['type'] == 'stacked_bar':
                            p = self.seaborn_to_bokeh_stacked(cell)
                        elif spec['type'] == 'dist':
                            p = self.seaborn_to_bokeh_dist(cell)
                        # Wrap in a layout container to ensure a LayoutDOM is returned
                        return column(p, sizing_mode='stretch_width', styles=cell_style, css_classes=["card"])
                    except Exception:
                        # Fallback to static image if interactive conversion fails
                        try:
                            buf = io.BytesIO()
                            cell.figure.savefig(buf, format='png', bbox_inches='tight')
                            buf.seek(0)
                            img_base64 = base64.b64encode(buf.read()).decode('utf-8')
                            img_html = f'<img src="data:image/png;base64,{img_base64}" style="max-width:100%;height:auto;"/>'
                            plot_style = dict(cell_style) if cell_style else {}
                            plot_style.update({
                                "background": "#fff",
                                "box-shadow": "0 2px 8px 0 rgba(0,0,0,0.08)",
                                "border-radius": "12px",
                                "padding": "18px"
                            })
                            return Div(text=img_html, sizing_mode='stretch_width', styles=plot_style)
                        except Exception:
                            # Last-resort placeholder to avoid returning None
                            return Div(text='<div>(Plot render failed)</div>', sizing_mode='stretch_width', styles=cell_style)
                else:
                    buf = io.BytesIO()
                    # Use bbox_inches='tight' to crop all excess whitespace
                    cell.figure.savefig(buf, format='png', bbox_inches='tight')
                    buf.seek(0)
                    img_base64 = base64.b64encode(buf.read()).decode('utf-8')
                    img_html = f'<img src="data:image/png;base64,{img_base64}" style="max-width:100%;height:auto;"/>'
                    # White bg for plot, shadow, rounded
                    plot_style = dict(cell_style) if cell_style else {}
                    plot_style.update({
                        "background": "#fff",
                        "box-shadow": "0 2px 8px 0 rgba(0,0,0,0.08)",
                        "border-radius": "12px",
                        "padding": "18px"
                    })
                    return Div(text=img_html, sizing_mode='stretch_width', styles=plot_style)
            # Handle HTML string
            elif isinstance(cell, str) and cell.strip().startswith('<'):
                text_style = dict(cell_style) if cell_style else {}
                text_style.update({
                    "background": "#29588C" if is_heading else "#4CB4E7",
                    "color": "#fff" ,#if is_heading else "#000",
                    "border-radius": "10px",
                    "padding": "16px",
                    "box-shadow": "0 1px 4px 0 rgba(0,0,0,0.04)",
                    "font-size": "28px" if is_heading else "22px",
                    "font-weight": "700" if is_heading else "400",
                    "text-align": "center",
                    "align-items": "center",
                    "justify-content": "center",
                })
                return Div(text=cell, sizing_mode='stretch_width', styles=text_style)
            # Handle plain text
            elif isinstance(cell, str):
                text_style = dict(cell_style) if cell_style else {}
                text_style.update({
                    "background": "#29588C" if is_heading else "#4CB4E7",
                    "color": "#fff" ,#if is_heading else "#000",
                    "border-radius": "10px",
                    "padding": "16px",
                    "box-shadow": "0 1px 4px 0 rgba(0,0,0,0.04)",
                    "font-size": "28px" if is_heading else "18px",
                    "font-weight": "700" if is_heading else "400",
                    "text-align": "center",
                    "align-items": "center",
                    "justify-content": "center",
                })
                return Div(text=cell, sizing_mode='stretch_width', styles=text_style)
            # Handle None
            elif cell is None:
                return Div(text='', sizing_mode='stretch_width', styles=cell_style)
            else:
                return Div(text=str(cell), sizing_mode='stretch_width', styles=cell_style)

        # Dashboard background
        dashboard_bg = "#fff"  # light blue
        row_colors = ["#fff","#fff"]

        panels = []
        for tab in dashboard_tabs:
            tab_title = tab.get('title', 'Tab')
            columns = tab.get('columns', [])
            if not isinstance(columns, list):
                columns = [columns]

            rows = []
            for i, row_cells in enumerate(columns):
                if not isinstance(row_cells, list):
                    row_cells = [row_cells]
                n_cols = len(row_cells)
                if n_cols == 0:
                    continue
                # Subtle alternating row background
                row_bg = row_colors[i % 2]
                cell_style = {
                    "background": row_bg,
                    "margin": "8px 8px 8px 8px"
                }
                rendered_cells = [render_cell(cell, width=1400, map_width=map_width, n_cols=n_cols, cell_style=cell_style) for cell in row_cells]
                # Center single plot or text cell horizontally, but let it fill the row
                if n_cols == 1:
                    # If the cell is a DataTable, wrap in a row so it displays correctly
                    rc0 = rendered_cells[0]
                    if rc0 is None:
                        rc0 = Div(text='')
                    if hasattr(rc0, 'columns') and hasattr(rc0, 'source'):
                        row_layout = row(rc0, sizing_mode='stretch_width', align='center')
                    elif (
                        isinstance(row_cells[0], str)
                        or (isinstance(row_cells[0], dict) and (('text' in row_cells[0]) or ('html' in row_cells[0])))
                        or (isinstance(row_cells[0], (tuple, list)) and len(row_cells[0]) == 2 and isinstance(row_cells[0][0], str))
                        or (hasattr(row_cells[0], 'text') and not hasattr(row_cells[0], 'figure'))
                    ):
                        row_layout = column(rc0, sizing_mode='stretch_width', align='center')
                    else:
                        # Center plot by wrapping in a row with align='center' and max_width
                        from bokeh.models import Spacer
                        row_layout = row(Spacer(width=0, sizing_mode='stretch_height'),
                                         rc0,
                                         Spacer(width=0, sizing_mode='stretch_height'),
                                         sizing_mode='stretch_height',
                                         align='center'
                                         )
                else:
                    row_layout = row(*rendered_cells, sizing_mode='stretch_width', align='center')
                rows.append(row_layout)

            tab_layout = column(*rows, sizing_mode='stretch_width', styles={"background": dashboard_bg, "padding": "18px"})
            panel = TabPanel(child=tab_layout, title=tab_title)
            panels.append(panel)

        tabs = Tabs(tabs=panels, sizing_mode="stretch_width")
        show(tabs)
        # Optionally save as HTML
        if save_html:
            from bokeh.io import save
            from bokeh.resources import INLINE
            filename = html_filename if html_filename is not None else "dashboard.html"
            save(tabs, filename=filename, resources=INLINE, title="Mode Share Dashboard")




    
    def plot_dist(
        self,
        df=None,
        value_col=None,
        weight_col='trip_weight',
        hue=None,
        hue_order=None,
        bins=50,
        kde=True,
        plt_title=None,
        x_label=None,
        y_label='Density',
        ax=None,
        palette=None,
        grid_points=200,
        x_tick_labels=None  # New parameter for x-tick labels
    ):
        """
        Plot a weighted distribution (normalized to integrate to 1) for a continuous variable.
        If hue is provided, draw one line per hue category.

        Parameters:
            df: DataFrame source; if None, uses self.data_source
            value_col: name of the continuous variable column
            weight_col: name of the weight column
            hue: optional categorical column for multiple lines
            hue_order: list specifying order of hue categories
            bins: number of bins for histogram fallback
            kde: if True, use weighted Gaussian KDE; otherwise use histogram-density step lines
            plt_title: plot title
            x_label: x-axis label
            y_label: y-axis label (default 'Density')
            ax: existing matplotlib axis; if None, a new figure is created
            palette: seaborn palette name or list of colors
            grid_points: number of points for KDE evaluation grid

        Returns:
            fig: matplotlib Figure
        """
        import numpy as np
        import pandas as pd
        import matplotlib.pyplot as plt
        from collections import OrderedDict

        if df is None:
            data = self.data_source.copy()
        else:
            data = df.copy()

        if value_col is None:
            raise ValueError("value_col must be provided for plot_dist")
        if weight_col not in data.columns:
            raise ValueError(f"weight_col '{weight_col}' not found in DataFrame")

        # Filter to rows with non-null values and non-null, non-negative weights
        sub = data[[value_col, weight_col] + ([hue] if hue else [])].dropna()
        sub = sub[sub[weight_col] > 0]
        if sub.empty:
            raise ValueError("No valid data after filtering NaNs and non-positive weights.")

        # Determine hue categories and colors
        if hue is None:
            categories = [None]
        else:
            if hue_order is None:
                categories = list(pd.unique(sub[hue]))
            else:
                categories = list(hue_order)

        if palette is None:
            from itertools import islice, cycle
            colors = list(islice(cycle(self.default_palette), len(categories)))
        elif isinstance(palette, str):
            colors = sns.color_palette(palette, n_colors=len(categories))
        else:
            # Ensure we have enough colors
            if len(palette) < len(categories):
                from itertools import cycle, islice
                colors = list(islice(cycle(palette), len(categories)))
            else:
                colors = list(palette)

        # Axis and labels
        if ax is None:
            fig, ax = plt.subplots(figsize=(10, 5))
        else:
            fig = ax.get_figure()
        if plt_title is None:
            plt_title = f"Distribution of {value_col.replace('_', ' ').title()}"
        if x_label is None:
            x_label = value_col.replace('_', ' ').title()

        # Global x-range across all categories
        x_min = np.nanmin(sub[value_col].values)
        x_max = np.nanmax(sub[value_col].values)
        if np.isfinite(x_min) and np.isfinite(x_max) and x_min != x_max:
            # Use bins to determine the number of x-axis tick locations
            xs = np.linspace(x_min, x_max, bins + 1)  # bins + 1 to include edges
        else:
            # Degenerate case: single unique value
            xs = np.linspace(x_min - 1, x_max + 1, bins + 1)

        # Add option to set x-tick labels
        if x_tick_labels is not None:
            if len(x_tick_labels) != len(xs):
                raise ValueError(
                    f"The number of x_tick_labels ({len(x_tick_labels)}) must match the number of bins ({len(xs)})."
                )
            ax.set_xticks(xs)
            ax.set_xticklabels(x_tick_labels, rotation=45, ha='right')
        else:
            ax.set_xticks(xs)

        # Helper to compute weighted histogram density line
        def weighted_hist_density(x, w):
            hist, edges = np.histogram(x, bins=bins, weights=w, density=True)
            centers = (edges[:-1] + edges[1:]) / 2
            return centers, hist

        # Plot per category
        lines = []
        labels = []
        line_specs = []  # for Bokeh conversion
        for i, cat in enumerate(categories):
            if cat is None:
                df_cat = sub
                label = "All"
            else:
                df_cat = sub[sub[hue] == cat]
                label = str(cat)
            if df_cat.empty:
                continue
            x = df_cat[value_col].values
            w = df_cat[weight_col].values

            if kde:
                try:
                    from scipy.stats import gaussian_kde
                    kde_est = gaussian_kde(x, weights=w)
                    ys = kde_est(xs)
                    # gaussian_kde returns a density that integrates to 1
                    line, = ax.plot(xs, ys, color=colors[i], label=label, linewidth=2)
                    line_specs.append({
                        'label': label,
                        'x': xs.tolist(),
                        'y': ys.tolist(),
                        'kind': 'kde'
                    })
                except Exception:
                    # Fallback to histogram density step line
                    centers, dens = weighted_hist_density(x, w)
                    line = ax.step(centers, dens, where='mid', color=colors[i], label=label, linewidth=2)
                    line_specs.append({
                        'label': label,
                        'x': centers.tolist(),
                        'y': dens.tolist(),
                        'kind': 'hist'
                    })
            else:
                centers, dens = weighted_hist_density(x, w)
                line = ax.step(centers, dens, where='mid', color=colors[i], label=label, linewidth=2)
                line_specs.append({
                    'label': label,
                    'x': centers.tolist(),
                    'y': dens.tolist(),
                    'kind': 'hist'
                })

            lines.append(line)
            labels.append(label)

        ax.set_title(plt_title)
        ax.set_xlabel(x_label)
        ax.set_ylabel(y_label)
        if labels:
            ax.legend()
        ax.grid(True, alpha=0.2)

        # Add option to set x-tick labels
        # if x_tick_labels is not None:
        #     ax.set_xticks(xs)
        #     ax.set_xticklabels(x_tick_labels, rotation=45, ha='right')

        # Attach Bokeh spec for interactive conversion
        try:
            from matplotlib.colors import to_hex
            # Normalize palette to hex strings
            from itertools import islice, cycle
            if isinstance(palette, str) or palette is None:
                # Use default palette for None; for str, generate via seaborn
                from itertools import islice, cycle
                if palette is None:
                    regen = list(islice(cycle(self.default_palette), len(labels)))
                    palette_hex = [c for c in regen]
                else:
                    regen = sns.color_palette(palette, n_colors=len(labels))
                    palette_hex = [to_hex(c) for c in regen]
            else:
                if len(palette) < len(labels):
                    expanded = list(islice(cycle(palette), len(labels)))
                else:
                    expanded = list(palette)
                try:
                    palette_hex = [to_hex(c) for c in expanded]
                except Exception:
                    # assume already hex strings
                    palette_hex = [str(c) for c in expanded]
            fig._bokeh_spec = {
                'type': 'dist',
                'value_col': value_col,
                'hue_col': hue,
                'lines': line_specs,
                'palette': palette_hex,
                'title': plt_title or '',
                'x_label': x_label or value_col,
                'y_label': y_label or 'Density',
            }
        except Exception:
            pass

        return fig

    def plot_stacked_bar(self, df=None, group_col=None, stack_col=None, weight_col='trip_weight', add_ref = False,axis_order=None, stack_order=None, plt_title=None, x_label=None, y_label='Share of Trips', ax=None, rel_legend=True, palette=None):
        """
        Plot a stacked bar chart for group_col, stacking by stack_col, with reliability hatches.
        """
        hatch_map = {
            "High": "",
            "Medium": "//",
            "Low": "xx"
        }
        if df is None:
            if add_ref:
                ref_df = self.data_source.copy()
                ref_df[group_col] = 'Total'
                filtered_trips = pd.concat([self.data_source, ref_df])
            else:
                filtered_trips = self.data_source
        else:
            if add_ref:
                ref_df = df.copy()
                ref_df[group_col] = 'Total'
                filtered_trips = pd.concat([df, ref_df])
            else:
                filtered_trips = df.copy()
        if axis_order is None:
            if add_ref:
                axis_order = ['Total'] +filtered_trips[group_col].unique().tolist()
            else:
                axis_order = filtered_trips[group_col].unique().tolist()
        elif add_ref:
            axis_order = ['Total'] + axis_order
        if stack_order is None:
            stack_order = filtered_trips[stack_col].unique().tolist()
        if plt_title is None:
            plt_title = f"Stacked Bar: {group_col} by {stack_col}"
        if x_label is None:
            x_label = group_col.replace('_', ' ').title()
        # Normalize palette into a list of colors (supports string names, lists, or matplotlib colormaps)
        n_colors = len(stack_order) if stack_order is not None else 6
        def _to_color_list(pal, n):
            try:
                import matplotlib.cm as cm
            except Exception:
                cm = None
            # Default
            if pal is None:
                from itertools import islice, cycle
                return list(islice(cycle(self.default_palette), n))
            # String name: try seaborn first, fall back to matplotlib cmap
            if isinstance(pal, str):
                try:
                    return list(sns.color_palette(pal, n_colors=n))
                except Exception:
                    if cm is not None:
                        try:
                            cmap = cm.get_cmap(pal)
                            return [cmap(i) for i in np.linspace(0, 1, n)]
                        except Exception:
                            pass
                # Final fallback
                return list(sns.color_palette(n_colors=n))
            # Matplotlib colormap (callable)
            if hasattr(pal, "__call__"):
                return [pal(i) for i in np.linspace(0, 1, n)]
            # Already a list/tuple of colors
            if isinstance(pal, (list, tuple)):
                return list(pal)
            # Fallback
            return list(sns.color_palette(n_colors=n))
        palette_list = _to_color_list(palette, n_colors)
        # Prepare data
        grouped = filtered_trips.groupby([group_col, stack_col])[weight_col].sum().unstack(fill_value=0)
        total = grouped.sum(axis=1)
        share = grouped.divide(total, axis=0).fillna(0)
        # Reliability and CI
        # Use within-group totals for reliability so it matches stacked shares
        results = self._weighted_share_se(
            df=filtered_trips,
            group_cols=[group_col, stack_col],
            weight_col=weight_col
        )
        # Plot
        if ax is None:
            fig, ax = plt.subplots(figsize=(10, 5))
        bottoms = np.zeros(len(axis_order))
        bars = []
        for i, stack_val in enumerate(stack_order):
            vals = share[stack_val].reindex(axis_order, fill_value=0).values
            bar = ax.bar(axis_order, vals, bottom=bottoms, color=palette_list[i % len(palette_list)], label=str(stack_val))
            bars.append(bar)
            # Add reliability hatches and error bars
            for j, rect in enumerate(bar):
                main_cat = axis_order[j]
                match = results[(results[group_col] == main_cat) & (results[stack_col] == stack_val)]
                if not match.empty:
                    ci = match['ci'].values[0]
                    est_rel = match['est_reliability'].values[0]
                    rect.set_hatch(hatch_map.get(est_rel, ""))
                    rect.set_edgecolor('grey')
                    # ax.errorbar(rect.get_x() + rect.get_width()/2, bottoms[j] + vals[j], yerr=ci, color='black', capsize=4, fmt='none')
                    # Annotate in the middle of the stack, white text
                    mid_y = bottoms[j] + vals[j]/2
                    ax.annotate(f'{vals[j]*100:.1f}%',
                                xy=(rect.get_x() + rect.get_width()/2, mid_y),
                                xytext=(0, 0),
                                textcoords="offset points",
                                ha='center', va='center',
                                color='white', fontsize=10)
            bottoms += vals
        ax.set_title(plt_title)
        ax.set_xlabel(x_label)
        ax.set_ylabel(y_label)
        ax.set_xticklabels(axis_order, rotation=45, ha='right')
        # Legend
        # Build clean stack legend (no hatches) using proxy patches matching palette
        stack_handles = [
            mpatches.Patch(facecolor=palette_list[i % len(palette_list)], edgecolor='grey', label=str(stack_val))
            for i, stack_val in enumerate(stack_order)
        ]
        reliability_patches = [
            mpatches.Patch(facecolor='gray', hatch=hatch_map[rel], label=f'{rel} Reliability', edgecolor='k')
            for rel in ['High', 'Medium', 'Low']
        ]
        combined_handles = stack_handles + (reliability_patches if rel_legend else [])
        ax.legend(handles=combined_handles, loc='upper left', bbox_to_anchor=(1, 1), title=stack_col.replace('_', ' ').title())
        # Attach Bokeh spec for interactive conversion in dashboard (include reliability per stacker)
        try:
            shares_df = share.reindex(index=axis_order, columns=stack_order, fill_value=0).reset_index()
            shares_df = shares_df.rename(columns={group_col: 'group'})
            # Build reliability columns per stacker
            rel_wide = results.pivot(index=group_col, columns=stack_col, values='est_reliability').reindex(index=axis_order, columns=stack_order)
            for st in stack_order:
                colname = f'reliability_{st}'
                shares_df[colname] = rel_wide[st].values if st in rel_wide.columns else None
            # Serialize palette to hex
            from matplotlib.colors import to_hex
            try:
                palette_hex = [to_hex(c) for c in palette_list]
            except Exception:
                palette_hex = [str(c) for c in palette_list]
            fig._bokeh_spec = {
                'type': 'stacked_bar',
                'group_col': group_col,
                'axis_order': axis_order,
                'stackers': stack_order,
                'data': shares_df.to_dict('records'),
                'palette': palette_hex,
                'title': plt_title or '',
                'x_label': x_label or group_col,
                'y_label': y_label or 'Share of Trips',
            }
        except Exception:
            pass
        return fig
    


    def seaborn_to_bokeh_hue(self, fig, width=920, height=550):
        """
        Convert a Seaborn/Matplotlib grouped bar chart (with _bokeh_spec attribute)
        into an interactive Bokeh figure.
        """
        if not hasattr(fig, "_bokeh_spec"):
            raise ValueError("Figure does not have a _bokeh_spec attribute to convert.")

        spec = fig._bokeh_spec

        group_col = spec['group_col']
        hue_col = spec['hue_col']
        axis_order = spec['axis_order']
        hue_order = spec['hue_order']
        data_records = spec['data']

        # Determine y_col (prefer 'share', then 'value', then any numeric)
        y_col = None
        if data_records and isinstance(data_records, list):
            keys = set(data_records[0].keys())
            exclude = {group_col, hue_col, 'reliability'}
            candidates = [k for k in keys if k not in exclude]
            # Prefer 'share', then 'value', then any numeric
            if 'share' in candidates:
                y_col = 'share'
            elif 'value' in candidates:
                y_col = 'value'
            elif candidates:
                y_col = candidates[0]
        if y_col is None:
            y_col = 'share'

        # Build Bokeh ColumnDataSource for grouped bars
        hatch_dict = {'High': ' ', 'Medium': "\\", 'Low': 'x'}
        factors = [(str(g), str(h)) for g in axis_order for h in hue_order]
        y_vals = []
        reliabilities = []
        hatch = []
        lower_bounds = []
        upper_bounds = []
        for g in axis_order:
            for h in hue_order:
                match = next((rec for rec in data_records if str(rec[group_col]) == str(g) and str(rec[hue_col]) == str(h)), None)
                y_vals.append(match.get(y_col, 0) if match else 0)
                reliabilities.append(match.get('reliability') if match and 'reliability' in match else None)
                hatch.append(hatch_dict.get(match.get('reliability')) if match and 'reliability' in match else None)
                lower_bounds.append(match.get('lower_bound') if match and 'lower_bound' in match else None)
                upper_bounds.append(match.get('upper_bound') if match and 'upper_bound' in match else None)
        print("checking bounds", lower_bounds, upper_bounds)
        source = ColumnDataSource(data=dict(
            x=factors,
            y=y_vals,
            group=[g for g, h in factors],
            hue=[h for g, h in factors],
            reliability=reliabilities,
            lower=lower_bounds,
            upper=upper_bounds,
            hatch=hatch
        ))

        # Pick colors for hue groups (use provided palette if available)
        colors = spec.get('palette') or list(self.default_palette[:len(hue_order)])
        # Ensure palette length matches hue_order length (pad if needed)
        if len(colors) < len(hue_order):
            from itertools import cycle, islice
            colors = list(islice(cycle(colors), len(hue_order)))

        # Create Bokeh figure
        from bokeh.transform import factor_cmap
        from bokeh.models import NumeralTickFormatter
        p = figure(
            x_range=FactorRange(*factors),
            height=height,
            width=width,
            title=spec.get('title', ''),
            toolbar_location="above",
            tools="pan,wheel_zoom,box_zoom,reset,save"
        )

        # Add vertical bars with color mapped to hue (2nd factor)
        bar_renderer = p.vbar(
            x='x',
            top='y',
            width=0.8,
            source=source,
            # hatch_pattern='hatch',
            # hatch_alpha=0.4,
            fill_color=factor_cmap('x', palette=colors, factors=[str(h) for h in hue_order], start=1, end=2),
            line_color="black",
            legend_group='hue'
        )
        # Add error bars (whiskers) if available, and ensure they are drawn on top of bars
        from bokeh.models import Whisker, LabelSet
        if any(lower_bounds) and any(upper_bounds):
            whisker = Whisker(source=source, base='x', upper='upper', lower='lower', line_width=2, line_color='black', level='overlay')
            p.add_layout(whisker)
            # Optionally, add whisker annotations (upper and lower)
            # label_upper = LabelSet(x='x', y='upper', text='upper', level='glyph', source=source,
            #                       render_mode='canvas', text_font_size='8pt', text_align='center', y_offset=2)
            # label_lower = LabelSet(x='x', y='lower', text='lower', level='glyph', source=source,
            #                       render_mode='canvas', text_font_size='8pt', text_align='center', y_offset=-10)
            # p.add_layout(label_upper)
            # p.add_layout(label_lower)

        # Add hover tool, use x-axis label for group_col in tooltip
        x_axis_label = spec.get('x_label', group_col)
        tooltips = [
            (x_axis_label, "@group"),
            (hue_col, "@hue")
        ]
        # Format y_col appropriately
        if y_col == 'share':
            tooltips.append(("Share", "@y{0.0%}"))
        else:
            tooltips.append((y_col.replace('_', ' ').title(), "@y"))
        # Only show reliability if present in any record
        has_reliability = any(r is not None for r in reliabilities)
        if has_reliability:
            tooltips.append(("Reliability", "@reliability"))
        hover = HoverTool(tooltips=tooltips)
        p.add_tools(hover)

        # Style axes
        p.yaxis.axis_label = spec.get('y_label', y_col.replace('_', ' ').title())
        if y_col == 'share':
            p.yaxis.formatter = NumeralTickFormatter(format='0%')
        p.xaxis.axis_label = spec.get('x_label', group_col)
        p.xaxis.major_label_orientation = 1.0
        p.xgrid.grid_line_color = None

        # Move legend outside plot on the right to avoid overlap
        try:
            if p.legend:
                p.add_layout(p.legend[0], 'right')
        except Exception:
            pass

        return p
     
     


    def seaborn_to_bokeh_stacked(self,fig, width=920, height=550):
        """
        Convert a Seaborn/Matplotlib stacked bar chart (with _bokeh_spec) into an interactive Bokeh plot.
        """
        if not hasattr(fig, "_bokeh_spec"):
            raise ValueError("Figure does not have a _bokeh_spec attribute to convert.")

        spec = fig._bokeh_spec
        hatch_dict = {'High': ' ', 'Medium': "\\", 'Low': 'x'}

        from bokeh.models import NumeralTickFormatter
        group_col = spec['group_col']
        axis_order = spec['axis_order']
        stackers = spec['stackers']
        data_records = spec['data']

        # Prepare data in wide format expected by vbar_stack
        # data_records are already wide with keys: 'group' and each stacker
        groups = [str(rec.get('group')) for rec in data_records]
        data_dict = {'group': groups}
        for s in stackers:
            data_dict[str(s)] = [float(rec.get(s, 0) or 0) for rec in data_records]
            # Include reliability per stacker if provided in spec data
            rel_key = f"reliability_{s}"
            data_dict[rel_key] = [rec.get(rel_key, None) for rec in data_records]
            hatch_col = f"hatch_{s}"
            data_dict[hatch_col] = [hatch_dict.get(rec.get(rel_key), " ") for rec in data_records]

        source = ColumnDataSource(data=data_dict)

        # Pick colors for the stackers (use provided palette if available)
        colors = spec.get('palette') or list(self.default_palette[:len(stackers)])
        if len(colors) < len(stackers):
            from itertools import cycle, islice
            colors = list(islice(cycle(colors), len(stackers)))

        # Create figure
        p = figure(
            x_range=[str(g) for g in axis_order],
            height=height,
            width=width,
            title=spec.get('title', ''),
            toolbar_location="above",
            tools="pan,wheel_zoom,box_zoom,reset,save"
        )

        # Draw stacked bars
        renderers = p.vbar_stack(
            stackers=[str(s) for s in stackers],
            x='group',
            width=0.8,
            color=colors,
            source=source,
            hatch_pattern=[f'hatch_{str(s)}' for s in stackers],
            hatch_alpha=0.4,
            legend_label=[str(s) for s in stackers]
        )

        # Add per-layer hovers so we can reference the correct reliability column
        x_axis_label = spec.get('x_label', group_col)
        for i, r in enumerate(renderers):
            s = str(stackers[i])
            rel_key = f"reliability_{s}"
            tooltips = [
                (x_axis_label, "@group"),
                ("Category", "$name"),
                ("Share", "@$name{0.0%}")
            ]
            # Only show reliability if present in any record for this stacker
            has_reliability = any(val is not None for val in data_dict.get(rel_key, []))
            if has_reliability:
                tooltips.append(("Reliability", f"@{{{rel_key}}}"))
            hover = HoverTool(renderers=[r], tooltips=tooltips)
            p.add_tools(hover)

        # Style
        p.yaxis.axis_label = spec.get('y_label', 'Share')
        p.yaxis.formatter = NumeralTickFormatter(format='0%')
        p.xaxis.axis_label = spec.get('x_label', group_col)
        p.xaxis.major_label_orientation = 1.0
        p.xgrid.grid_line_color = None
        # Move legend outside plot on the right to avoid overlap
        try:
            if p.legend:
                p.add_layout(p.legend[0], 'right')
        except Exception:
            pass

        return p

    def seaborn_to_bokeh(self,fig, width=920, height=550):
        """
        Convert a simple Seaborn/Matplotlib bar chart (with _bokeh_spec) into an interactive Bokeh plot.
        Works for ungrouped, single-series bar charts.
        """
        if not hasattr(fig, "_bokeh_spec"):
            raise ValueError("Figure does not have a _bokeh_spec attribute to convert.")

        spec = fig._bokeh_spec
        group_col = spec.get('group_col')
        axis_order = spec.get('axis_order')
        data_records = spec.get('data', [])

        # Determine value column (not group_col, not hue_col)
        value_col = None
        if data_records and isinstance(data_records, list):
            keys = set(data_records[0].keys())
            exclude = {group_col}
            if 'hue_col' in spec and spec['hue_col']:
                exclude.add(spec['hue_col'])
            value_candidates = [k for k in keys if k not in exclude]
            if 'value' in value_candidates:
                value_col = 'value'
            elif value_candidates:
                value_col = value_candidates[0]
        if value_col is None:
            value_col = 'value'  # fallback


        # Determine which y-value column to use (share, value, or other)
        # Find the first numeric column that is not group_col, hue_col, or reliability
        y_col = None
        if data_records and isinstance(data_records, list):
            keys = set(data_records[0].keys())
            exclude = {group_col, 'reliability'}
            if 'hue_col' in spec and spec['hue_col']:
                exclude.add(spec['hue_col'])
            candidates = [k for k in keys if k not in exclude]
            # Prefer 'share', then 'value', then any numeric
            if 'share' in candidates:
                y_col = 'share'
            elif 'value' in candidates:
                y_col = 'value'
            elif candidates:
                y_col = candidates[0]
        if y_col is None:
            y_col = 'value'

        # Build ColumnDataSource, always include reliability column, but only show in tooltip if present
        data_dict = {
            group_col: [str(g) for g in axis_order],
            y_col: [],
            'reliability': []
        }
        has_reliability = False
        for g in axis_order:
            match = next((rec for rec in data_records if str(rec[group_col]) == str(g)), None)
            if match:
                yval = match.get(y_col, 0)
                relval = match.get('reliability')
                data_dict[y_col].append(yval)
                data_dict['reliability'].append(relval)
                if relval is not None:
                    has_reliability = True
            else:
                data_dict[y_col].append(0)
                data_dict['reliability'].append(None)

        source = ColumnDataSource(data=data_dict)

        # Choose colors: use provided palette if present, and map each bar to its color if palette length matches axis_order
        palette_hex = spec.get('palette')
        if palette_hex and len(palette_hex) >= len(axis_order):
            bar_colors = [palette_hex[i] for i in range(len(axis_order))]
        else:
            bar_colors = [self.default_palette[0]] * len(axis_order)
        # Add bar_colors as a column in the data source for Bokeh
        data_dict['fill_color'] = bar_colors
        source = ColumnDataSource(data=data_dict)

        # Create figure
        p = figure(
            x_range=axis_order,
            height=height,
            width=width,
            title=spec.get('title', ''),
            toolbar_location="above",
            tools="pan,wheel_zoom,box_zoom,reset,save"
        )

        # Draw bars, color per bar using fill_color column in the source
        p.vbar(
            x=group_col,
            top=y_col,
            width=0.8,
            source=source,
            fill_color='fill_color',
            line_color="black"
        )

        # Add hover tool, only show reliability if present, and format share as percent if y_col is 'share'
        x_axis_label = spec.get('x_label', group_col)
        if y_col == 'share':
            tooltips = [
                (x_axis_label, f"@{group_col}"),
                ("Share", f"@{y_col}{{0.0%}}")
            ]
            if has_reliability:
                tooltips.append(("Reliability", "@reliability"))
            hover = HoverTool(tooltips=tooltips)
        else:
            tooltips = [
                (x_axis_label, f"@{group_col}"),
                ("Value", f"@{y_col}")
            ]
            if has_reliability:
                tooltips.append(("Reliability", "@reliability"))
            hover = HoverTool(tooltips=tooltips)
        p.add_tools(hover)

        # Style
        p.yaxis.axis_label = spec.get('y_label', 'Value')
        p.xaxis.axis_label = spec.get('x_label', group_col)
        p.xaxis.major_label_orientation = 1.0
        p.xgrid.grid_line_color = None
        return p

    def seaborn_to_bokeh_dist(self, fig, width=920, height=550):
        """
        Convert a Matplotlib distribution plot (with _bokeh_spec type 'dist') to an interactive Bokeh plot.
        Draws multiple lines (one per hue/label) with tooltips.
        """
        if not hasattr(fig, "_bokeh_spec"):
            raise ValueError("Figure does not have a _bokeh_spec attribute to convert.")

        spec = fig._bokeh_spec
        lines = spec.get('lines', [])
        palette = spec.get('palette') or list(self.default_palette)

        p = figure(
            height=height,
            width=width,
            title=spec.get('title', ''),
            toolbar_location="above",
            tools="pan,wheel_zoom,box_zoom,reset,save"
        )

        # Assign colors per label deterministically
        from collections import OrderedDict
        labels = [ln.get('label', 'All') for ln in lines]
        unique_labels = list(OrderedDict.fromkeys(labels))
        color_map = {}
        for i, lbl in enumerate(unique_labels):
            color_map[lbl] = palette[i % len(palette)] if palette else self.default_palette[i % len(self.default_palette)]

        # Draw lines with their own data sources for hover
        renderers = []
        for ln in lines:
            lbl = ln.get('label', 'All')
            xs = ln.get('x', [])
            ys = ln.get('y', [])
            src = ColumnDataSource(data=dict(x=xs, y=ys, label=[lbl]*len(xs)))
            r = p.line('x', 'y', source=src, color=color_map[lbl], line_width=2, legend_label=str(lbl))
            # Add a dedicated hover for this line
            hover = HoverTool(renderers=[r], tooltips=[
                (spec.get('hue_col') or 'Series', '@label'),
                ('x', '@x{0.00}'),
                ('density', '@y{0.000}')
            ])
            p.add_tools(hover)
            renderers.append(r)

        # Style axes
        from bokeh.models import NumeralTickFormatter
        p.yaxis.axis_label = spec.get('y_label', 'Density')
        p.xaxis.axis_label = spec.get('x_label', spec.get('value_col', 'Value'))
        p.xgrid.grid_line_color = None
        p.legend.click_policy = 'hide'
        try:
            if p.legend:
                p.add_layout(p.legend[0], 'right')
        except Exception:
            pass

        return p

    
    def plot_grouped_bar_with_dropdown(
        self, df, group_col, value_col, dropdown_col, hue_col=None,
        axis_order=None, hue_order=None, plt_title=None, x_label=None, y_label=None,
        palette=None, reliability_col='est_reliability'
    ):
        """
        Create a grouped (or simple) bar chart with a dropdown to toggle categories (e.g., trip purpose).
        If hue_col is None, creates a simple bar chart per dropdown value.
        Normalizes values to share within each dropdown selection.
        Uses _weighted_share_se to calculate reliability.
        Adds a 'Total' option to the dropdown to show all categories combined.
        """
        from bokeh.models import Select, CustomJS, ColumnDataSource, HoverTool
        from bokeh.plotting import figure
        from bokeh.layouts import column
        from bokeh.models import FactorRange

        dropdown_options = df[dropdown_col].unique().tolist()
        dropdown_options = ['Total'] + list(dropdown_options)
        if axis_order is None:
            axis_order = df[group_col].unique().tolist()
        if hue_col is not None:
            if hue_order is None:
                hue_order = df[hue_col].unique().tolist()
        if palette is None:
            palette = self.default_palette

        def get_grouped(selected):
            if selected == 'Total':
                d = df.copy()
            else:
                d = df[df[dropdown_col] == selected].copy()
            if hue_col is not None:
                group_fields = [group_col, hue_col]
            else:
                group_fields = [group_col]
            # Use _weighted_share_se to get share and reliability
            results = self._weighted_share_se(
                df=d,
                group_cols=group_fields,
                weight_col=value_col,
                hue_is_tot = False
            )
            # results must have columns: group_col, (hue_col), 'share', 'est_reliability'
            return results

        import json
        # Precompute summarized data for each dropdown option, always using string keys
        summarized = {}
        for opt in dropdown_options:
            opt_str = str(opt)
            grouped = get_grouped(opt)
            if hue_col is not None:
                # Build a dict keyed by json.dumps([group, hue]) with JS-compatible separators
                key_map = {}
                for _, row in grouped.iterrows():
                    key = json.dumps([str(row[group_col]), str(row[hue_col])], separators=(',', ':'))
                    key_map[key] = {
                        'share': row['share'],
                        'reliability': row['est_reliability'],
                        'upper_bound': row.get('upper_bound', None),
                        'lower_bound': row.get('lower_bound', None)
                    }
                summarized[opt_str] = key_map
            else:
                # Build a dict keyed by group
                key_map = {}
                for _, row in grouped.iterrows():
                    key_map[str(row[group_col])] = {
                        'share': row['share'],
                        'reliability': row['est_reliability'],
                        'upper_bound': row.get('upper_bound', None),
                        'lower_bound': row.get('lower_bound', None)
                    }
                summarized[opt_str] = key_map

        # Debug: print summarized['Home']
        import json
        # print("summarized['Home'] =", json.dumps(summarized.get('Home', {}), indent=2))

        def make_source(selected):
            key_map = summarized[str(selected)]
            if hue_col is not None:
                # Ensure factor order is always [(x, y) for x in axis_order for y in hue_order]
                factors = [(str(x), str(y)) for x in axis_order for y in hue_order]
                shares = []
                rels = []
                group_vals = []
                hue_vals = []
                lowers = []
                uppers = []
                for x in axis_order:
                    for y in hue_order:
                        key = json.dumps([str(x), str(y)], separators=(',', ':'))
                        val = key_map.get(key, {'share': 0, 'reliability': None, 'lower_bound': None, 'upper_bound': None})
                        shares.append(val['share'])
                        rels.append(val['reliability'])
                        group_vals.append(str(x))
                        hue_vals.append(str(y))
                        lowers.append(val.get('lower_bound', None))
                        uppers.append(val.get('upper_bound', None))
                return ColumnDataSource(data=dict(x=factors, share=shares, reliability=rels, lower=lowers, upper=uppers, **{group_col: group_vals, hue_col: hue_vals}))
            else:
                shares = []
                rels = []
                lowers = []
                uppers = []
                for x in axis_order:
                    val = key_map.get(str(x), {'share': 0, 'reliability': None, 'lower_bound': None, 'upper_bound': None})
                    shares.append(val['share'])
                    rels.append(val['reliability'])
                    lowers.append(val.get('lower_bound', None))
                    uppers.append(val.get('upper_bound', None))
                return ColumnDataSource(data=dict(x=axis_order, share=shares, reliability=rels, lower=lowers, upper=uppers))

        initial = dropdown_options[0]
        source = make_source(initial)

        if hue_col is not None:
            x_range = FactorRange(*source.data['x'])
        else:
            x_range = axis_order
        p = figure(x_range=x_range, height=550, width=920, sizing_mode='stretch_width', title=plt_title, toolbar_location=None, tools="")
        from bokeh.transform import factor_cmap

        if hue_col is not None:
            vbar = p.vbar(
                x='x', top='share', width=0.8, source=source, line_color="white",
                fill_color=factor_cmap('x', palette=palette, factors=hue_order, start=1, end=2)
            )
        else:
            vbar = p.vbar(
                x='x', top='share', width=0.8, source=source, line_color="white",
                fill_color=palette[0] if palette else "#4CB4E7"
            )
        # Add whiskers (error bars) if available
        from bokeh.models import Whisker
        # Always add whisker, and ensure it updates with the source
        whisker = Whisker(source=source, base='x', upper='upper', lower='lower', line_width=2, line_color='black', level='overlay')
        p.add_layout(whisker)
        p.xaxis.major_label_orientation = 1
        p.xaxis.axis_label = x_label
        p.yaxis.axis_label = y_label if y_label else 'Share'

        # Add hover tool, use x-axis label for group_col in tooltip
        x_axis_label = x_label if x_label is not None else group_col
        if hue_col is not None:
            tooltips = [
                (x_axis_label, f'@{group_col}'),
                (hue_col, f'@{hue_col}'),
                ("Share", "@share{0.0%}")
            ]
            # Only show reliability if present in any record
            has_reliability = any(r is not None for r in source.data.get('reliability', []))
            if has_reliability:
                tooltips.append(("Reliability", "@reliability"))
            hover = HoverTool(renderers=[vbar], tooltips=tooltips)
        else:
            tooltips = [
                (x_axis_label, '@x'),
                ("Share", "@share{0.0%}")
            ]
            has_reliability = any(r is not None for r in source.data.get('reliability', []))
            if has_reliability:
                tooltips.append(("Reliability", "@reliability"))
            hover = HoverTool(renderers=[vbar], tooltips=tooltips)
        p.add_tools(hover)

        select = Select(title=dropdown_col, value=str(initial), options=[str(opt) for opt in dropdown_options])

        # CustomJS callback to update data using only summarized data
        if hue_col is not None:
            # summarized is already stringified above, so just copy
            summarized_js = summarized
            callback_code = """
                var selected = String(select.value);
                var summarized = summarized_js;
                var axis_order = axis_order_js;
                var hue_order = hue_order_js;
                var group_col = group_col_js;
                var hue_col = hue_col_js;
                var keys = Object.keys(summarized);
                var key_map = summarized[selected];
                if (!key_map) {
                    // fallback for non-string keys
                    for (var i = 0; i < keys.length; ++i) {
                        if (String(keys[i]) === selected) {
                            key_map = summarized[keys[i]];
                            break;
                        }
                    }
                }
                var x = [];
                var share = [];
                var reliability = [];
                var group_vals = [];
                var hue_vals = [];
                var lower = [];
                var upper = [];
                for (var i = 0; i < axis_order.length; i++) {
                    for (var j = 0; j < hue_order.length; j++) {
                        var key = JSON.stringify([String(axis_order[i]), String(hue_order[j])]);
                        var val = key_map ? (key_map[key] || {share: 0, reliability: null, lower_bound: null, upper_bound: null}) : {share: 0, reliability: null, lower_bound: null, upper_bound: null};
                        x.push([axis_order[i], hue_order[j]]);
                        share.push(val.share);
                        reliability.push(val.reliability);
                        group_vals.push(String(axis_order[i]));
                        hue_vals.push(String(hue_order[j]));
                        lower.push(val.lower_bound);
                        upper.push(val.upper_bound);
                    }
                }
                source.data = {
                    x: x,
                    share: share,
                    reliability: reliability,
                    lower: lower,
                    upper: upper,
                };
                source.data[group_col] = group_vals;
                source.data[hue_col] = hue_vals;
                source.change.emit();
            """
            # Convert tuple keys to JSON-stringified keys for JS
            import json
            summarized_js = {}
            for k, v in summarized.items():
                summarized_js[k] = {}
                for key, val in v.items():
                    if isinstance(key, tuple):
                        summarized_js[k][json.dumps([str(key[0]), str(key[1])])] = val
                    else:
                        summarized_js[k][str(key)] = val
            callback = CustomJS(args=dict(
                source=source, select=select,
                summarized_js=summarized_js,
                axis_order_js=axis_order, hue_order_js=hue_order,
                group_col_js=group_col, hue_col_js=hue_col
            ), code=callback_code)
        else:
            callback_code = """
                var selected = select.value;
                var summarized = summarized_js;
                var axis_order = axis_order_js;
                var key_map = summarized[selected];
                var x = [];
                var share = [];
                var reliability = [];
                var lower = [];
                var upper = [];
                for (var i = 0; i < axis_order.length; i++) {
                    var key = String(axis_order[i]);
                    var val = key_map[key] || {share: 0, reliability: null, lower_bound: null, upper_bound: null};
                    x.push(axis_order[i]);
                    share.push(val.share);
                    reliability.push(val.reliability);
                    lower.push(val.lower_bound);
                    upper.push(val.upper_bound);
                }
                var data = source.data;
                data['x'] = x;
                data['share'] = share;
                data['reliability'] = reliability;
                data['lower'] = lower;
                data['upper'] = upper;
                source.change.emit();
            """
            summarized_js = {}
            for k, v in summarized.items():
                summarized_js[k] = {}
                for key, val in v.items():
                    summarized_js[k][str(key)] = val
            callback = CustomJS(args=dict(
                source=source, select=select,
                summarized_js=summarized_js,
                axis_order_js=axis_order
            ), code=callback_code)

        select.js_on_change('value', callback)

        return column(select, p, sizing_mode="stretch_width", min_width=920)
    
    def plot_grouped_dist_with_dropdown(self,
        df, value_col, dropdown_col, weight_col='trip_weight', hue_col=None, x_labels=None,
        plt_title=None, x_label=None, y_label='Density', hue_order=None,
        palette=DEFAULT_PALETTE, bins=50, kde=True, grid_points=200
        ):
        """
        Create a distribution plot with a dropdown to toggle categories (e.g., trip purpose`)."""

        dropdown_options = df[dropdown_col].unique().tolist()
        dropdown_options = ['Total'] + list(dropdown_options)


        summarized = {}
        if hue_col is not None:
            if hue_order is None:
                hue_order = df[hue_col].dropna().unique().tolist()
        else:
            hue_order = None

        for opt in dropdown_options:
            if opt == 'Total':
                d = df.copy()
            else:
                d = df[df[dropdown_col] == opt].copy()
            fig = self.plot_dist(
                df=d,
                value_col=value_col,
                weight_col=weight_col,
                hue=hue_col,
                hue_order=hue_order,
                bins=bins,
                kde=kde,
                plt_title=plt_title,
                x_label=x_label,
                y_label=y_label,
                palette=palette,
            )
            lines = fig._bokeh_spec['lines']
            if hue_col is not None:
                # Multiple lines, one per hue
                lines_dict = {line['label']: line for line in lines}
                ordered_lines = [lines_dict.get(str(hue), {'x': [], 'y': [], 'label': str(hue)}) for hue in hue_order]
                summarized[str(opt)] = ordered_lines
            else:
                # Single line only
                summarized[str(opt)] = lines[0]

        initial = dropdown_options[0]
        if hue_col is not None:
            initial_lines = summarized[str(initial)]
            sources = []
            labels = [str(hue) for hue in hue_order]
            for i, line in enumerate(initial_lines):
                src = ColumnDataSource(data=dict(x=line['x'], y=line['y'], label=[line['label']] * len(line['x'])))
                sources.append(src)
        else:
            line = summarized[str(initial)]
            src = ColumnDataSource(data=dict(x=line['x'], y=line['y'], label=[line['label']] * len(line['x'])))
            sources = [src]
            labels = [line['label']]

        p = figure(height=550, width=920, sizing_mode='stretch_width', title=plt_title, toolbar_location=None, tools="")
        color_map = {}
        for i, lbl in enumerate(labels):
            color_map[lbl] = palette[i % len(palette)]
        renderers = []
        for i, src in enumerate(sources):
            lbl = labels[i]
            r = p.line('x', 'y', source=src, color=color_map[lbl], line_width=2, legend_label=str(lbl))
            hover = HoverTool(renderers=[r], tooltips=[
                (hue_col if hue_col else 'Series', '@label'),
                ('x', '@x{0.00}'),
                ('density', '@y{0.000}')
            ])
            p.add_tools(hover)
            renderers.append(r)
        p.xaxis.axis_label = x_label if x_label else value_col
        p.yaxis.axis_label = y_label
        p.legend.click_policy = 'hide'

        select = Select(title=dropdown_col, value=str(initial), options=[str(opt) for opt in dropdown_options])

        if hue_col is not None:
            callback_code = """
                var selected = select.value;
                var summarized = summarized_js;
                var lines = summarized[selected];
                for (var i = 0; i < sources.length; i++) {
                    var line = lines[i];
                    sources[i].data = {
                        x: line.x,
                        y: line.y,
                        label: Array(line.x.length).fill(line.label)
                    };
                    sources[i].change.emit();
                }
            """
        else:
            callback_code = """
                var selected = select.value;
                var summarized = summarized_js;
                var line = summarized[selected];
                sources[0].data = {
                    x: line.x,
                    y: line.y,
                    label: Array(line.x.length).fill(line.label)
                };
                sources[0].change.emit();
            """

        import json
        summarized_js = {}
        for k, v in summarized.items():
            summarized_js[str(k)] = v
        callback = CustomJS(args=dict(
            sources=sources,
            select=select,
            summarized_js=summarized_js
        ), code=callback_code)

        select.js_on_change('value', callback)

        if x_labels is not None:
            p.xaxis.ticker = list(range(len(x_labels)))
            p.xaxis.major_label_overrides = {i: label for i, label in enumerate(x_labels)}
            p.xaxis.major_label_orientation = .5

        return column(select, p, sizing_mode="stretch_width", min_width=920)

    def summary_bar(self,survey, stats = ['population', 'households', 'trips', 'tours'], weighted = True, trip_weight = 'trip_weight', hh_weight = 'hh_weight', person_weight = 'person_weight', tour_weight = 'tour_weight'):
        from bokeh.models import Div
        if weighted:
            heading = "Weighted "
            population_count = int(survey.person[person_weight].sum())

            household_count = int(survey.hh[hh_weight].sum())
            trip_count = int(survey.trips[trip_weight].sum())
        else:
            heading = ""
            population_count = len(survey.person)
            household_count = len(survey.hh)
            trip_count = len(survey.trips)
        if survey.tours is not None:
            tour_count = int(survey.tours[tour_weight].sum())
            tours_html = f'''
                <div style="display:flex;align-items:center;gap:16px;">
                <svg width="36" height="36" fill="#4CB4E7" <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 640"><!--!Font Awesome Free v7.0.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2025 Fonticons, Inc.--><path d="M160 144C190.9 144 216 118.9 216 88C216 57.1 190.9 32 160 32C129.1 32 104 57.1 104 88C104 118.9 129.1 144 160 144zM73.4 291.9L96 269.3L96 338.6C96 366.6 108.2 393.3 129.5 411.5L200.9 472.7C206.8 477.8 210.7 484.8 211.8 492.5L224.4 580.6C226.9 598.1 243.1 610.3 260.6 607.8C278.1 605.3 290.3 589.1 287.8 571.6L275.2 483.5C271.9 460.4 260.3 439.4 242.6 424.2L208.1 394.6L208.1 279.4L211.9 284.1C230.1 306.9 257.7 320.1 286.9 320.1L320.1 320.1C337.8 320.1 352.1 305.8 352.1 288.1C352.1 270.4 337.8 256.1 320.1 256.1L286.9 256.1C277.2 256.1 268 251.7 261.9 244.1L244 221.7C221 192.9 186.1 176.1 149.2 176.1C117 176.1 86.1 188.9 63.4 211.7L28.1 246.6C10.1 264.6 0 289 0 314.5L0 352C0 369.7 14.3 384 32 384C49.7 384 64 369.7 64 352L64 314.5C64 306 67.4 297.9 73.4 291.9zM85.8 471.3C84.3 476.5 81.5 481.3 77.7 485.1L9.4 553.4C-3.1 565.9-3.1 586.2 9.4 598.7C21.9 611.2 42.2 611.2 54.7 598.7L123 530.4C134.5 518.9 142.9 504.6 147.4 488.9L149.6 481.3L103.6 441.9C101.1 439.7 98.6 437.5 96.2 435.1L85.8 471.3zM359 399C349.6 408.4 349.6 423.6 359 432.9L431 504.9C440.4 514.3 455.6 514.3 464.9 504.9C474.2 495.5 474.3 480.3 464.9 471L433.9 440L536 440C537.2 440 538.4 439.9 539.5 439.7C595.6 435.8 640 389.1 640 332C640 272.4 591.6 224 532 224L440 224C426.7 224 416 234.7 416 248C416 261.3 426.7 272 440 272L532 272C565.1 272 592 298.9 592 332C592 365.1 565.1 392 532 392L433.9 392L464.9 361C474.3 351.6 474.3 336.4 464.9 327.1C455.5 317.8 440.3 317.7 431 327.1L359 399.1z"/>
                </svg>
                <span>{heading}Tours: {tour_count:,}</span>
                </div>
                '''
            tour_div = Div(text=tours_html, styles=bubble_styles, sizing_mode='stretch_width')

        population_html = f'''
        <div style="display:flex;align-items:center;gap:16px;">
        <svg width="36" height="36" fill="#4CB4E7" viewBox="0 0 24 24"><path d="M12 12c2.7 0 5-2.3 5-5s-2.3-5-5-5-5 2.3-5 5 2.3 5 5 5zm0 2c-3.3 0-10 1.7-10 5v3h20v-3c0-3.3-6.7-5-10-5z"/></svg>
        <span>{heading}Population: {population_count:,}</span>
        </div>
        '''

        households_html = f'''
        <div style="display:flex;align-items:center;gap:16px;">
        <svg width="36" height="36" fill="#4CB4E7" viewBox="0 0 24 24"><path d="M12 3l10 9h-3v9h-6v-6h-2v6H5v-9H2z"/></svg>
        <span>{heading}Households: {household_count:,}</span>
        </div>
        '''

        trips_html = f'''
        <div style="display:flex;align-items:center;gap:16px;">
        <svg width="36" height="36" fill="#4CB4E7" <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 640"><!--!Font Awesome Free v7.0.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2025 Fonticons, Inc.--><path d="M136.5 88C136.5 57.1 161.6 32 192.5 32C223.4 32 248.5 57.1 248.5 88C248.5 118.9 223.4 144 192.5 144C161.6 144 136.5 118.9 136.5 88zM128.5 269.3L105.9 291.9C99.9 297.9 96.5 306 96.5 314.5L96.5 352C96.5 369.7 82.2 384 64.5 384C46.8 384 32.5 369.7 32.5 352L32.5 314.5C32.5 289 42.6 264.6 60.6 246.6L95.7 211.5C118.5 188.7 149.3 175.9 181.5 175.9C218.4 175.9 253.3 192.7 276.3 221.5L294.3 244C300.4 251.6 309.6 256 319.3 256L352.5 256C370.2 256 384.5 270.3 384.5 288C384.5 305.7 370.2 320 352.5 320L319.3 320C290.1 320 262.6 306.7 244.3 284L240.5 279.3L240.5 394.5L275 424.1C292.7 439.3 304.3 460.3 307.6 483.4L320.2 571.5C322.7 589 310.5 605.2 293 607.7C275.5 610.2 259.3 598 256.8 580.5L244.2 492.4C243.1 484.7 239.2 477.7 233.3 472.6L162 411.5C140.7 393.3 128.5 366.6 128.5 338.6L128.5 269.3zM128.6 435C131 437.3 133.4 439.6 136 441.8L182 481.2L179.8 488.8C175.3 504.5 166.9 518.8 155.4 530.3L87.1 598.6C74.6 611.1 54.3 611.1 41.8 598.6C29.3 586.1 29.3 565.8 41.8 553.3L110.1 485C113.9 481.2 116.7 476.4 118.2 471.2L128.6 435zM537.5 409C528.1 418.4 512.9 418.4 503.6 409C494.3 399.6 494.2 384.4 503.6 375.1L534.6 344.1L432.5 344.1C419.2 344.1 408.5 333.4 408.5 320.1C408.5 306.8 419.2 296.1 432.5 296.1L534.6 296.1L503.6 265.1C494.2 255.7 494.2 240.5 503.6 231.2C513 221.9 528.2 221.8 537.5 231.2L609.5 303.2C618.9 312.6 618.9 327.8 609.5 337.1L537.5 409.1z"/>
        </svg>
        <span>{heading}Trips: {trip_count:,}</span>
        </div>
        '''

        
        bubble_styles = {
            "height": "80px",                # Reduce height for less empty space
            "display": "flex",
            "flexDirection": "column",
            "alignItems": "center",
            "justifyContent": "center",
            "borderRadius": "20px",
            "background": "#f5f5f5",
            "boxShadow": "0 2px 8px #ccc",
            "fontSize": "28px",              # Larger text
            "margin": "4px 0",               # Less vertical margin
            "width": "100%",
            "padding": "0",                  # Remove extra padding
        }

        pop_div = Div(text=population_html, styles=bubble_styles, sizing_mode='stretch_width')
        hh_div = Div(text=households_html, styles=bubble_styles, sizing_mode='stretch_width')
        trip_div = Div(text=trips_html, styles=bubble_styles, sizing_mode='stretch_width')
        if survey.tours is not None:
            summary_list = [pop_div, hh_div, trip_div, tour_div]
        else:
            summary_list = [pop_div, hh_div, trip_div]
        return summary_list
