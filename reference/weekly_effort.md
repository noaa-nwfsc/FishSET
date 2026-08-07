# Summarize average CPUE by week

`weekly_effort` summarizes CPUE (or other numeric variables) in the main
table by week. It can summarize by grouping variables and filter by
period or value. There are several options for customizing the table and
plot output.

## Usage

``` r
weekly_effort(
  dat,
  project,
  cpue,
  date,
  group = NULL,
  sub_date = NULL,
  filter_date = NULL,
  date_value = NULL,
  filter_by = NULL,
  filter_value = NULL,
  filter_expr = NULL,
  facet_by = NULL,
  conv = "none",
  tran = "identity",
  format_lab = "decimal",
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

- cpue:

  Variable(s) in `dat` containing catch per unit effort.

- date:

  A variable in `dat` containing dates to aggregate by.

- group:

  Grouping variable name(s). Up to two grouping variables are available.
  For plotting, if a single CPUE column is entered the first grouping
  variable is passed to the "color" aesthetic and the second to
  "linetype". If multiple CPUE columns are entered, a new variable named
  "species" is created and passed to "fill", the first group variable to
  "linetype", and second is dropped.

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

  Variable name to facet by. Accepts up to two variables. Facetting by
  `"year"` is available if a date variable is added to `sub_date`.
  Facetting by `"species"` is available if multiple cpue columns are
  included in `"cpue"`. The first variable is facetted by row and the
  second by column.

- conv:

  Convert catch variable to `"tons"`, `"metric_tons"`, or by using a
  function entered as a string. Defaults to `"none"` for no conversion.

- tran:

  A function to transform the y-axis. Options include log, log2, log10,
  sqrt.

- format_lab:

  Formatting option for y-axis labels. Options include `"decimal"` or
  `"scientific"`.

- combine:

  Whether to combine variables listed in `group`. This is passed to the
  "color" aesthetic for plots.

- scale:

  Scale argument passed to
  [`facet_grid`](https://ggplot2.tidyverse.org/reference/facet_grid.html).
  Defaults to `"fixed"`.

- output:

  Whether to display `"plot"`, `"table"`. Defaults to both
  (`"tab_plot"`).

- format_tab:

  How table output should be formatted. Options include 'wide' (the
  default) and 'long'.

## Value

`weekly_effort()` calculates mean CPUE by week. This function doesn't
calculate CPUE; the CPUE variable must be created in advance (see
[`cpue`](cpue.md)). When multiple CPUE variables are entered, a new
column named "species" is created and used to group values in plots. The
"species" column can also be used to split (or facet) the plot. For
table output, the "species" column will be kept if
`format_tab = "long"`, i.e. a column of species names ("species") and a
column containing the mean CPUE ("mean_cpue"). When
`format_tab = "wide"`, each CPUE variable is given its own value column.
The data can be filtered by date and/or by a variable. `filter_date`
specifies the type of date filter to apply–by date-range or by period.
`date_value` should contain the values to filter the data by. To filter
by a variable, enter its name as a string in `filter_by` and include the
values to filter by in `filter_value`. Up to two grouping variables can
be entered. Grouping variables can be merged into one variable using
`combine`; in this case any number of variables can be joined, but no
more than three is recommended. For faceting, any variable (including
ones listed in `group`) can be used, but "year" and "species" are also
available. Facetting by "year" requires a date variable be added to
`sub_date`. Currently, combined variables cannot be faceted. A list
containing a table and plot are printed to the console and viewer by
default.

## Examples

``` r
if (FALSE) { # \dontrun{
weekly_effort(pollockMainDataTable, "CPUE", "DATE_FISHING_BEGAN", filter_date = "year", 
              date_value = 2011, output = "table")
} # }
```
