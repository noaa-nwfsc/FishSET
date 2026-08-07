# Apply moving average function to catch

Apply moving average function to catch

## Usage

``` r
roll_catch(
  dat,
  project,
  catch,
  date,
  group = NULL,
  combine = FALSE,
  k = 10,
  fun = "mean",
  filter_date = NULL,
  date_value = NULL,
  filter_by = NULL,
  filter_value = NULL,
  filter_expr = NULL,
  facet_by = NULL,
  scale = "fixed",
  align = "center",
  conv = "none",
  tran = "identity",
  format_lab = "decimal",
  output = "tab_plot",
  ...
)
```

## Arguments

- dat:

  Primary data frame over which to apply function. Table in FishSET
  database should contain the string \`MainDataTable\`.

- project:

  Name of project.

- catch:

  Variable name or names containing catch data. Multiple variables can
  entered as a vector.

- date:

  Date variable to aggregate by.

- group:

  Variable name or names to group by. Plot will display up to two
  grouping variables.

- combine:

  Whether to combine variables listed in `group`. This is passed to the
  "fill" or "color" aesthetic for plots.

- k:

  The width of the window.

- fun:

  The function to be applied to window. Defaults to `mean`.

- filter_date:

  The type of filter to apply to table. The "date_range" option will
  subset the data by two date values entered in `filter_val`. Other
  options include "year-day", "year-week", "year-month", "year",
  "month", "week", or "day". The argument filter_value must be provided.

- date_value:

  String containing a start and end date if using filter_date =
  "date_range", e.g. c("2011-01-01", "2011-03-15"). If filter_date =
  "period" or "year-period", use integers (4 digits if year, 1-2 if day,
  month, or week). Use a list if using a two-part filter, e.g.
  "year-week", with the format `list(year, period)` or a vector if using
  a single period, `c(period)`. For example, `list(2011:2013, 5:7)` will
  filter the data table from weeks 5 through 7 for years 2011-2013 if
  filter_date = "year-week".`c(2:5)` will filter the data February
  through May when filter_date = "month".

- filter_by:

  String, variable name to filter by.

- filter_value:

  A vector of values to filter \`MainDataTable\` by using the variable
  in `filter_by`.

- filter_expr:

  String, a valid R expression to filter \`MainDataTable\` by using the
  variable in `filter_by`.

- facet_by:

  Variable name to facet by. This can be a variable that exists in the
  dataset, or a variable created by `roll_catch()` such as `"year"`,
  `"month"`, or `"species"` if more than one variable is entered in
  `catch`.

- scale:

  Scale argument passed to `facet_grid`. Options include `"free"`,
  `"free_x"`, `"free_y"`. Defaults to `"fixed"`.

- align:

  Indicates whether results of window should be left-aligned (`"left"`),
  right-aligned (`"right"`), or centered (`"center"`). Defaults to
  `"center"`.

- conv:

  Convert catch variable to `"tons"`, `"metric_tons"`, or by using a
  function. Defaults to `FALSE`.

- tran:

  A function to transform the y-axis. Options include log, log2, log10,
  sqrt.

- format_lab:

  Formatting option for y-axis labels. Options include `"decimal"` or
  `"scientific"`.

- output:

  Whether to display `"plot"`, `"table"`, or both. Defaults to both
  (`"tab_plot"`).

- ...:

  Additional arguments passed to
  [`rollapply`](https://rdrr.io/pkg/zoo/man/rollapply.html)

## Examples

``` r
if (FALSE) { # \dontrun{
roll_catch(pollockMainDataTable, project = "pollock", catch = "LBS_270_POLLOCK_LBS",
  date = "FISHING_START_DATE", group = "GEAR_TYPE", k = 15
)

roll_catch(pollockMainDataTable, project = "pollock", catch = c("LBS_270_POLLOCK_LBS", 
 "LBS_110_PACIFIC_COD_LBS"), date = "FISHING_START_DATE", group = "GEAR_TYPE", k = 5, 
 filter_date = "month", date_value = 4:6, facet_by = "month", conv = "tons"
)
} # }
```
