# Map observed vessel locations

Plot observed locations on a map. For large datasets, it is best to plot
a subset of points. Use `percshown` to randomly subset the number of
points. If the predefined map extent needs adjusting, use `minmax`.

## Usage

``` r
map_plot(dat, project, lat, lon, minmax = NULL, percshown = NULL)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  String, project name.

- lat:

  Variable in `dat` that defines latitude, in decimal degrees.

- lon:

  Variable in `dat` that defines longitude, in decimal degrees.

- minmax:

  Optional map extent argument, a vector (num) of length four
  corresponding to c(minlat, maxlat, minlon, maxlon).

- percshown:

  Whole number, percent of points to show. Use this option if there are
  a lot of data points.

## Value

mapout: ggplot2 object

## Examples

``` r
if (FALSE) { # \dontrun{
map_plot(pollockMainDataTable, 'pollock', 'LonLat_START_LAT', 'LonLat_START_LON', percshown=10)
} # }
```
