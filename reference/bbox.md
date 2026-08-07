# Compute bounding box for a dataframe with lon/lat columns.

Compute bounding box for a dataframe with lon/lat columns.

## Usage

``` r
bbox(dat, lon, lat, f = 0.05)
```

## Arguments

- dat:

  Dataframe containing longitude/latitude columns.

- lon:

  Name of Longitude column.

- lat:

  Name of Latitude column.

- f:

  Number specifying the fraction by which to extend the range.
