# Create KDE, CDF, or empirical CDF plots

Creates a kernel density estimate, empirical cumulative distribution
function, or cumulative distribution function plot of selected variable.
Grouping, filtering, and several plot options are available.

## Usage

``` r
density_plot(
  dat,
  project,
  var,
  type = "kde",
  group = NULL,
  combine = TRUE,
  date = NULL,
  filter_date = NULL,
  date_value = NULL,
  filter_by = NULL,
  filter_value = NULL,
  filter_expr = NULL,
  facet_by = NULL,
  conv = "none",
  tran = "identity",
  format_lab = "decimal",
  scale = "fixed",
  bw = 1,
  position = "identity",
  pages = "single"
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- var:

  String, name of variable to plot.

- type:

  String, type of density plot. Options include `"kde"` (kernel density
  estimate), `"ecdf"` (empirical cdf), `"cdf"` (cumulative distribution
  function), or "all" (all plot types). Two or more plot types can be
  chosen.

- group:

  Optional, string names of variables to group by. If two or grouping
  variables are included, the default for `"cdf"` and `"ecdf"` plots is
  to not combine groups. This can be changed using `combine = TRUE`.
  `"kde"` plots always combine two or more groups. `"cdf"` and `"ecdf"`
  plots can use up to two grouping variables if `combine = FALSE`: the
  first variable is represented by color and second by line type.

- combine:

  Logical, whether to combine the variables listed in `group` for plot.

- date:

  Date variable from `dat` used to subset and/or facet the plot by.

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

  String, a valid R expression to filter \`MainDataTable\` by.

- facet_by:

  Variable name to facet by. This can be a variable that exists in `dat`
  or a variable created by `density_plot()` such as `"year"`, `"month"`,
  or `"week"`. `date` is required if facetting by period.

- conv:

  Convert catch variable to `"tons"`, `"metric_tons"`, or by using a
  function entered as a string. Defaults to `"none"` for no conversion.

- tran:

  String; name of function to transform variable, for example `"log"` or
  `"sqrt"`.

- format_lab:

  Formatting option for x-axis labels. Options include `"decimal"` or
  `"scientific"`.

- scale:

  Scale argument passed to `facet_grid`. Defaults to `"fixed"`. Other
  options include `"free_y"`, `"free_x"`, and `"free"`.

- bw:

  Adjusts KDE bandwidth. Defaults to 1.

- position:

  The position of the grouped variable for KDE plot. Options include
  `"identity"`, `"stack"`, and `"fill"`.

- pages:

  Whether to output plots on a single page (`"single"`, the default) or
  multiple pages (`"multi"`).

## Value

`denstiy_plot()` can return up to three plots in a single call. When
`pages = "single"` all plots are combined and stacked vertically.
`pages = "multi"` will return separate plots.

## Details

The data can be filtered by date or by variable (see `filter_date` and
`filter_by`). If `type` contains `"kde"` or `"all"` then grouping
variables are automatically combined. Any variable in `dat` can be used
for faceting, but `"year"`, `"month"`, or `"week"` are also available if
`date` is provided.

## Examples

``` r
if (FALSE) { # \dontrun{

density_plot(pollockMainDataTable, "pollock", var = "OFFICIAL_TOTAL_CATCH_MT",
             type = c("kde", "ecdf"))

# facet 
density_plot(pollockMainDataTable, "pollock", var = "OFFICIAL_TOTAL_CATCH_MT",
             type = c("kde", "ecdf"), facet_by = "GEAR_TYPE")

# filter by period
density_plot(pollockMainDataTable, "pollock", var = "OFFICIAL_TOTAL_CATCH_MT", 
             type = "kde", date = "FISHING_START_DATE", filter_date = "year-month", 
             filter_value = list(2011, 9:11))
} # }
```
