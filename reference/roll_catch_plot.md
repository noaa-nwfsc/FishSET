# Create and format `roll_catch` plot

Create and format `roll_catch` plot

## Usage

``` r
roll_catch_plot(
  roll_tab,
  catch,
  date,
  group,
  facet_by,
  fun,
  k,
  conv,
  tran,
  format_lab,
  scale
)
```

## Arguments

- roll_tab:

  Table containing rolling summary of catch.

- catch:

  String, name of catch variable(s).

- date:

  String, name of date column.

- group:

  String, name of grouping variable(s).

- facet_by:

  String, name of facet variable(s).

- fun:

  String, name of summary function.

- k:

  Numeric, the width of the window.

- conv:

  String, convert pounds to "tons" or "metric_tons".

- tran:

  A function to transform the y-axis. Options include log, log2, log10,
  sqrt.

- format_lab:

  Formatting option for y-axis labels. Options include `"decimal"` or
  `"scientific"`.

- scale:

  Scale argument passed to `facet_grid`.Options include `"free"`,
  `"free_x"`, `"free_y"`. Defaults to `"fixed"`.
