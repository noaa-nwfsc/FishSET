# Add an area/polygon to spatial data

Add an area/polygon to spatial data

## Usage

``` r
make_spat_area(spat, project, coord, spat.id, new.id, combine)
```

## Arguments

- spat:

  Spatial dataset to add polygon too.

- project:

  Name of project.

- coord:

  Longitude and latitude coordinates forming a polygon. Can be a numeric
  vector of even length or a numeric matrix with two columns.

- spat.id:

  The ID column in `spat`

- new.id:

  The ID for new polygon.

- combine:

  Whether to use [`combine_zone`](combine_zone.md). This will turn the
  intersections between `poly` and `spat` into new polygons. Note that
  the new polygon IDs will be derived from `spat` and `new.id` will not
  be used.

## Details

Adds an area/polygone to a spatial area

## See also

[`make_polygon`](make_polygon.md) [`add_polygon`](add_polygon.md)
