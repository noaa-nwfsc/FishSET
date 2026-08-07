# Summarize active vessels

`vessel_count` counts the number of active vessels in the main table. It
can summarize by period if `date` is provided, group by any number of
grouping variables, and filter by period or value. There are several
options for customizing table/plot output.

## Usage

``` r
vessel_count(
  dat,
  project,
  v_id,
  date = NULL,
  period = NULL,
  group = NULL,
  sub_date = NULL,
  filter_date = NULL,
  date_value = NULL,
  filter_by = NULL,
  filter_value = NULL,
  filter_expr = NULL,
  facet_by = NULL,
  combine = FALSE,
  position = "stack",
  tran = "identity",
  format_lab = "decimal",
  value = "count",
  type = "bar",
  scale = "fixed",
  output = "tab_plot"
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- v_id:

  Variable in `dat` containing vessel identifier to count.

- date:

  Date variable to aggregate by.

- period:

  Time period to aggregate by. Options include `"year"`, `"month"`,
  `"week"` (weeks in the year), `"weekday"`, `"weekday_abv"`,
  `"day_of_month"`, `"day_of_year"`, and `"cal_date"` (calender date).

- group:

  Names of grouping variables. For line plots (`type = "line"`) two
  grouping variables can be entered, the first is passed to "color" and
  second to "linetype". Only one grouping variable can be used for
  barplots (`type = "bar"`), which is passed to "fill". When
  `combine = TRUE` all variables in `group` will be joined. Grouping by
  `"year"`, `"month"`, and `"week"` are available if a date variable is
  added to `sub_date`.

- sub_date:

  Date variable used for subsetting, grouping, or splitting by date.

- filter_date:

  The type of filter to apply to \`MainDataTable\`. To filter by a range
  of dates, use `filter_date = "date_range"`. To filter by a given
  period, use "year-day", "year-week", "year-month", "year", "month",
  "week", or "day". The argument `date_value` must be provided.

- date_value:

  This argument is paired with `filter_date`. To filter by date range,
  set `filter_date = "date_range"` and enter a start- and end-date into
  `date_value` as a string:
  `date_value = c("2011-01-01", "2011-03-15")`.

  To filter by period (e.g. "year", "year-month"), use integers (4
  digits if year, 1-2 digits if referencing a day, month, or week). Use
  a vector if filtering by a single period: `date_filter = "month"` and
  `date_value = c(1, 3, 5)`. This would filter the data to January,
  March, and May.

  Use a list if using a year-period type filter, e.g. "year-week", with
  the format: `list(year, period)`. For example,
  `filter_date = "year-month"` and `date_value = list(2011:2013, 5:7)`
  will filter the data table from May through July for years 2011-2013.

- filter_by:

  String, variable name to filter \`MainDataTable\` by. the argument
  `filter_value` must be provided.

- filter_value:

  A vector of values to filter \`MainDataTable\` by using the variable
  in `filter_by`. For example, if `filter_by = "GEAR_TYPE"`,
  `filter_value = 1` will include only observations with a gear type of
  1.

- filter_expr:

  String, a valid R expression to filter \`MainDataTable\` by using the
  variable in `filter_by`.

- facet_by:

  Variable name to facet by. Accepts up to two variables. These can be
  variables that exist in `dat`, or a variable created by
  `vessel_count()` such as `"year"`, `"month"`, or `"week"` if a date
  variable is added to `sub_date`. The first variable is facetted by row
  and the second by column.

- combine:

  Whether to combine variables listed in `group`. This is passed to the
  "fill" or "color" aesthetic for plots.

- position:

  Positioning of bar plot. Options include 'stack', 'dodge', and 'fill'.

- tran:

  A function to transform the y-axis. Options include log, log2, log10,
  and sqrt.

- format_lab:

  decimal or scientific

- value:

  Whether to return `"count"` or `"percent"` of active vessels. Defaults
  to `"count"`.

- type:

  Plot type, options include `"bar"` (the default) and `"line"`.

- scale:

  Scale argument passed to `facet_grid`. Options include `"free"`,
  `"free_x"`, `"free_y"`. Defaults to `"fixed"`.

- output:

  Whether to display `"plot"`, `"table"`. Defaults to both
  (`"tab_plot"`).

## Value

When `output = "tab_plot"` a list containing a table and plot are
returned. If `output = "table"` only the summary table is returned, if
`output = "plot"` only the plot.

## Details

`vessel_count` gives the number (or percent) of active vessels using a
column of unique vessel IDs. The data can be filtered by date and/or by
a variable. (console users may want to use a separate filtering
function, like
[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html),
before running `vessel_count`: note that this is okay but will lead to
different output if using [`log_rerun`](log_rerun.md)). `filter_date`
specifies the type of date filter to apply–by date-range or by period.
`date_value` should contain the values to filter the data by. To filter
by a variable, enter its name as a string in `filter_by` and include the
values to filter by in `filter_value`.

Up to two grouping variables can be entered. Grouping variables can be
merged into one variable using `combine = TRUE`; in this case any number
of variables can be joined, but no more than three is recommended.

For faceting, any variable (including ones listed in `group`) can be
used, but "year", "month", "week" are also available provided a date
variable is added to `sub_date`. Currently, combined variables cannot be
faceted.

## Examples

``` r
if (FALSE) { # \dontrun{
# grouping by two variables
vessel_count(pollockMainDataTable, v_id = "VESSEL_ID", 
             group = c("GEAR_TYPE", "IFQ"))
             
# filter by variable
vessel_count(pollockMainDataTable, v_id = "VESSEL_ID", group = "GEAR_TYPE",
             filter_by = "IFQ", filter_value = "Y")
             
# filter by month
vessel_count(pollockMainDataTable, v_id = "VESSEL_ID", group = "GEAR_TYPE",
             sub_date = "HAUL_DATE", date_filter = "month", date_value = 1:5)
             
#' # filter by date
vessel_count(pollockMainDataTable, v_id = "VESSEL_ID", group = "GEAR_TYPE",
             sub_date = "HAUL_DATE", date_filter = "date_range", 
             date_value = c("2011-01-01", "2011-02-05"))

# summarize by month
vessel_count(pollockMainDataTable, v_id = 'VESSEL_ID', date = 'DATE_FISHING_BEGAN', 
             period = 'month', group = 'DISEMBARKED_PORT', position = 'dodge', 
             output = 'plot')
} # }
```
