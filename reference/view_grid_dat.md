# Visualize gridded data on a map

Visualize gridded data on a map

## Usage

``` r
view_grid_dat(
  grid,
  project,
  lon,
  lat,
  value,
  split_by = NULL,
  group = NULL,
  agg_fun = "mean"
)
```

## Arguments

- grid:

  Gridded data table to visualize. Use string if visualizing a gridded
  data table in the FishSET Database.

- project:

  String, project name.

- lon:

  String, variable name containing longitude.

- lat:

  String, variable name containing latitude.

- value:

  String, variable name containing gridded values, e.g. sea surface
  temperature, wind speed, etc.

- split_by:

  String, variable in gridded data table to split by.

- group:

  String, variable in gridded data table to group `value` by. In
  addition to the variable(s) in `group`, `value` is also aggregated by
  each longitude-latitude pair. The string `"lonlat"` is a shortcut for
  `group = c("lon", "lat")` which aggregates the `value` for each
  longitude-latitude pair across the entire dataset.

- agg_fun:

  Aggregating function applied to `group`. Defaults to mean.
