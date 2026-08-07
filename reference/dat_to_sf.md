# Convert dataframe to sf

Used to convert spatial data with no spatial class to a `sf` object.
This is useful if the spatial data was read from a non-spatial file
type, e.g. a CSV file.

## Usage

``` r
dat_to_sf(dat, lon, lat, id, cast = "POLYGON", multi = FALSE, crs = 4326)
```

## Arguments

- dat:

  Spatial data containing information on fishery management or
  regulatory zones.

- lon:

  Longitude variable in `spatdat`.

- lat:

  Latitude variable in `spatdat`.

- id:

  Spatial feature ID column.

- cast:

  Spatial feature type to create. Commonly used options are `"POINT"`,
  `"LINESTRING"`, and `"POLYGON"`. See
  [`st_cast`](https://r-spatial.github.io/sf/reference/st_cast.html) for
  details.

- multi:

  Logical, use if needing to convert to a multi-featured (grouped) `sf`
  object, e.g. `MULTIPOLYGON` or `MULTILINESTRING`.

- crs:

  Coordinate reference system to assign to `dat`. Defaults to WGS 84
  (EPSG: 4326).
