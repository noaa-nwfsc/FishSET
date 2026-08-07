# Add polygon to spatial data

Add polygon to spatial data

## Usage

``` r
add_polygon(poly, spat, spat.id, new.id = NULL, combine = FALSE)
```

## Arguments

- poly:

  A valid polygon to add to spatial data.

- spat:

  Spatial dataset to add polygon too.

- spat.id:

  The ID column in `spat`.

- new.id:

  The ID for new polygon.

- combine:

  Whether to use [`combine_zone`](combine_zone.md). This will turn the
  intersections between `poly` and `spat` into new polygons. Note that
  the new polygon IDs will be derived from `spat` and `new.id` will not
  be used.

## See also

[`combine_zone`](combine_zone.md)
