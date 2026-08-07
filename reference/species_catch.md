# Summarize species catch

`species_catch` summarizes catch (or other numeric variables) in the
main table. It can summarize by period if `date` is provided, grouping
variables, and filter by period or value. There are several options for
customizing the table and plot output.

## Usage

``` r
species_catch(
  dat,
  project,
  species,
  date = NULL,
  period = NULL,
  fun = "sum",
  group = NULL,
  sub_date = NULL,
  filter_date = NULL,
  date_value = NULL,
  filter_by = NULL,
  filter_value = NULL,
  filter_expr = NULL,
  facet_by = NULL,
  type = "bar",
  conv = "none",
  tran = "identity",
  format_lab = "decimal",
  value = "count",
  position = "stack",
  combine = FALSE,
  scale = "fixed",
  output = "tab_plot",
  format_tab = "wide"
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- species:

  Variable in `dat` containing the species catch or a vector of species
  variables (in pounds).

- date:

  Variable in `dat` containing dates to aggregate by.

- period:

  Time period to count by. Options include 'year', 'month', 'week' (week
  of the year), 'weekday', 'day' (day of the month), and 'day_of_year'.
  `date` is required.

- fun:

  String, name of function to aggregate by. Defaults to
  [`sum`](https://rdrr.io/r/base/sum.html).

- group:

  Grouping variable name(s). Up to two grouping variables are available
  for line plots and one for bar plots. For bar plots, if only one
  species is entered the first group variable is passed to 'fill'. If
  multiple species are entered, species is passed to "fill" and the
  grouping variable is dropped. An exception occurs when facetting by
  species, then the grouping variable is passed to "fill". For line
  plots, the first grouping variable is passed to "fill" and the second
  to "linetype" if a single species column is entered or if facetting by
  species. Otherwise species is passed to "fill", the first group
  variable to "linetype", and second is dropped.

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
  variables that exist in the `dat`, or a variable created by
  `species_catch()` such as `"year"`, `"month"`, or `"week"` if a date
  variable is added to `sub_date`. Facetting by `"species"` is available
  if multiple catch columns are included in `"species"`. The first
  variable is facetted by row and the second by column.

- type:

  Plot type, options include `"bar"` (the default) and `"line"`.

- conv:

  Convert catch variable to `"tons"`, `"metric_tons"`, or by using a
  function entered as a string. Defaults to `"none"` for no conversion.

- tran:

  A function to transform the y-axis. Options include log, log2, log10,
  sqrt.

- format_lab:

  Formatting option for y-axis labels. Options include `"decimal"` or
  `"scientific"`.

- value:

  Whether to calculate raw `"count"` or `"percent"` of total catch.

- position:

  Positioning of bar plot. Options include 'stack', 'dodge', and 'fill'.

- combine:

  Whether to combine variables listed in `group`. This is passed to the
  "fill" or "color" aesthetic for plots.

- scale:

  Scale argument passed to `facet_grid`. Defaults to `"fixed"`.

- output:

  Output a `"plot"` or `"table"`. Defaults to both (`"tab_plot"`).

- format_tab:

  How table output should be formatted. Options include 'wide' (the
  default) and 'long'.

## Value

`species_catch()` aggregates catch using one or more columns of catch
data. Users can aggregate by time period, by group, or by both. When
multiple catch variables are entered, a new column "species" is created
and used to group values in plots. The "species" column can also be used
to split (or facet) the plot. For table output, the "species" column
will be kept if `format_tab = "long"`, i.e. a column of species names
("species") and a column containing catch ("catch"). When
`format_tab = "wide"`, each species is given its own column of catch.

The data can be filtered by date and/or by a variable. `filter_date`
specifies the type of date filter to apply–by date-range or by period.
`date_value` should contain the values to filter the data by. To filter
by a variable, enter its name as a string in `filter_by` and include the
values to filter by in `filter_value`.

Plots can handle Up to two grouping variables, there is no limit for
tables. Grouping variables can be merged into one variable using
`combine`; in this case any number of variables can be joined, but no
more than three is recommended.

For faceting, any variable (including ones listed in `group`) can be
used, but "year", "month", "week" are also available provided a date
variable is added to `sub_date`. Currently, combined variables cannot be
faceted. A list containing a table and plot are printed to the console
and viewer by default.

## Examples

``` r
if (FALSE) { # \dontrun{
# summarizing one catch column by one group variable
species_catch(pollockMainDataTable, species = "OFFICIAL_TOTAL_CATCH_MT",
              group = "GEAR_TYPE", ouput = "tab_plot")

# summarizing three catch columns by month
species_catch('pollockMainDataTable', 
              species = c('HAUL_LBS_270_POLLOCK_LBS', 
                          'HAUL_LBS_110_PACIFIC_COD_LBS', 
                          'HAUL_LBS_OTHER_LBS'), 
              date = 'HAUL_DATE', period = 'month_num', output = 'plot', 
              conv = 'tons')

# filtering by variable
species_catch(pollockMainDataTable, species = "OFFICAL_TOTAL_CATCH_MT",
              group = "GEAR_TYPE", filter_by = "PORT_CODE", 
              filter_value = "Dutch Harbor")
              
 # filtering by date
 species_catch(pollockMainDataTable, species = "OFFICAL_TOTAL_CATCH_MT",
               sub_date = "HAUL_DATE", filter_date = "month", date_value = 7:10)
} # }
```
