# Clean spatial data

Clean spatial data

## Usage

``` r
clean_spat(spat)
```

## Arguments

- spat:

  Spatial data to check.

## Details

`clean_spat` extracts polygons from "GEOMETRYCOLLECTION" spatial
features, removes non-polygons from the data, attempts to fix invalid
geometries, and shifts longitude to Pacific view if any points are less
than 0.
