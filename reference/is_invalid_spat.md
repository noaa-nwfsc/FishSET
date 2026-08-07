# Detect basic spatial issues

Predicate function that returns `TRUE` if certain spatial issues are
found.

## Usage

``` r
is_invalid_spat(spat)
```

## Arguments

- spat:

  Spatial data to check.

## Value

`TRUE` if a "GEOMETRYCOLLECTION" is found, if any spatial features are
not "POLYGON" or "MULTIPOLYGON", if invalid geometries are found, if any
empty geometries are detected, or if longitude needs to be shifted to
Pacific view.
