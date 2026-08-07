# View interactive map of spatial data

View interactive map of spatial data

## Usage

``` r
view_spat(spat, id = NULL, type = "polygon")
```

## Arguments

- spat:

  Spatial dataset to view. Must be an object of class `sf` or `sfc`.

- id:

  Optional, name of spatial ID column to view with spatial data.

- type:

  Can be `"polygon"`, `"line"`, or `"point"`.

## Examples

``` r
if (FALSE) { # \dontrun{
view_spat(pollockNMFSSpatTable, id = "NMFS_AREA")
} # }
```
