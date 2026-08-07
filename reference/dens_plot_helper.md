# density_plot helper function

Creates and formats plots

## Usage

``` r
dens_plot_helper(
  dataset,
  var,
  group,
  date,
  facet_by,
  filter_date,
  date_value,
  type,
  bw,
  conv,
  tran,
  format_lab,
  scale,
  position,
  pages
)
```

## Arguments

- dataset:

  Data used to create plot.

- var:

  String, variable passed from `density_plot`.

- group:

  String, grouping variable(s) passed from `density_plot`.

- date:

  String, date variable passed from `density_plot`.

- facet_by:

  String, facet variable(s) passed from `density_plot`.

- filter_date:

  String, date filter type passed from `density_plot`.

- date_value:

  Numeric, date filter value passed from `density_plot`.

- type:

  String, plot type(s) passed from `density_plot`.

- bw:

  Numeric, bandwidth passed from `density_plot`.

- conv:

  String, convert pounds to "tons" or "metric_tons".

- tran:

  String, scale transformation passed from `density_plot`.

- format_lab:

  String, label formatting option passed from `density_plot`.

- scale:

  Scale argument passed to `facet_grid`. Defaults to "fixed". Other
  options include "free_y", "free_x", and "free".

- position:

  String, plot position passed from `density_plot`.

- pages:

  String, single or multiple plots passed from `density_plot`.
