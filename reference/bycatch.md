# Compare bycatch CPUE and total catch/percent of total catch for one or more species

Compare bycatch CPUE and total catch/percent of total catch for one or
more species

## Usage

``` r
bycatch(
  dat,
  project,
  cpue,
  catch,
  date,
  period = "year",
  names = NULL,
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
  value = "stc",
  combine = FALSE,
  scale = "fixed",
  output = "tab_plot",
  format_tab = "wide"
)
```

## Arguments

- dat:

  Primary data frame over which to apply function. Table in FishSET
  database should contain the string \`MainDataTable\`.

- project:

  name of project.

- cpue:

  A string of CPUE variable names. The function outputs the mean CPUE by
  period. The variable names must match the order of variable names in
  `catch` and `names`.

- catch:

  A character string of names of catch variables to aggregate. The
  function outputs the total catch or share of total catch by period
  depending on the value argument. The order of the catch variable
  string must match those of the `cpue` and `names` arguments.

- date:

  A variable containing dates to aggregate by.

- period:

  Period to aggregate by. Options include 'year', month', and weeks'.

- names:

  An optional string of species names that will be used in the plot. If
  `NULL`, then species names from `catch` will be used.

- group:

  A categorical variable in `dat` to group by.

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
  `"year"`, `"month"`, or `"week"` is available if a date variable is
  added to `sub_date`.

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

  Whether to return raw catch ("raw") or share of total catch ('stc').

- combine:

  Logical, whether to combine variables listed in `group`.

- scale:

  Scale argument passed to `facet_grid`. Defaults to `"fixed"`. Other
  options include `"free_y"`, `"free_x"`, and `"free_xy"`.

- output:

  Output type. Options include 'table' or 'plot'.

- format_tab:

  How table output should be formatted. Options include `'wide'` (the
  default) and `'long'`.

## Value

`bycatch()` compares the average CPUE and catch total/share of total
catch between one or more species. The data can be filtered by date
and/or by a variable. `filter_date` specifies the type of date filter to
apply–by date-range or by period. `date_value` should contain the values
to filter the data by. To filter by a variable, enter its name as a
string in `filter_by` and include the values to filter by in
`filter_value`. Only one grouping variable will be displayed; however,
any number of variables can be combined by using `combine = TRUE`, but
no more than three is recommended. For faceting, any variable in the
dataset can be used, but "year" and "month" are also available provided
a date variable is added to `sub_date`. Generally, no more than four
species should be compared, and even fewer when faceting due to limited
plot space. A list containing a table and plot are printed to the
console and viewer by default. For optimal plot size in an R
Notebook/Markdown document, use the chunk option `fig.asp = 1`.

## Details

Returns a plot and/or table of the mean CPUE and share of total catch or
raw count for each species entered. For optimal plot size in an R
Notebook/Markdown document, we recommend including no more than four
species. The order of variables in the `cpue` and `catch` arguments must
be in the same order as in the `names` argument. The `names` argument is
used to join the `catch` and `cpue` variables together.

## Examples

``` r
if (FALSE) { # \dontrun{
cpue(pollockMainDataTable, "myproject", xWeight = "f1Weight",
  xTime = "Hour", "f1_cpue"
)

bycatch(pollockMainDataTable, "myproject", 
        cpue = c("f1_cpue", "f2_cpue", "f3_cpue", "f4_cpue"),
        catch = c("f1", "f2", "f3", "f4"), date = "FISHING_START_DATE",
        names = c("fish_1", "fish_2", "fish_3", "fish_4"), period = "month",
        date_filter = "year", date_value = 2011, value = "stc", 
        output = "table")
} # }
```
