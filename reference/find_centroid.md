# Identify geographic centroid of fishery management or regulatory zone

Identify geographic centroid of fishery management or regulatory zone

## Usage

``` r
find_centroid(
  spat,
  project,
  spatID,
  lon.spat = NULL,
  lat.spat = NULL,
  cent.name = NULL,
  log.fun = TRUE
)
```

## Arguments

- spat:

  Spatial data containing information on fishery management or
  regulatory zones. Can be shape file, json, geojson, data frame, or
  list.

- project:

  Name of project

- spatID:

  Variable or list in `spat` that identifies the individual areas or
  zones. If `spat` is class sf, `spatID` should be name of list
  containing information on zones.

- lon.spat:

  Variable or list from `spat` containing longitude data. Required for
  csv files. Leave as NULL if `spat` is a shape or json file.

- lat.spat:

  Variable or list from `spat` containing latitude data. Required for
  csv files. Leave as NULL if `spat` is a shape or json file.

- cent.name:

  String, name to include in centroid table. Centroid name take the form
  of \`"projectNameZoneCentroid"\`. Defaults to \`NULL\` (e.g.
  \`"projectZoneCentroid"\`).

- log.fun:

  Logical, whether to log function call (for internal use).

## Value

Returns a data frame where each row is a unique zone and columns are the
zone ID and the latitude and longitude defining the centroid of each
zone.

## Details

Returns the geographic centroid of each area/zone in `spat`. The
centroid table is saved to the FishSET database. Function is called by
the `create_alternative_choice` and `create_dist_between` functions.
