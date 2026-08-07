# Create distance between points variable - non-interactive version

Adds distance between two points to the primary data set. There are two
versions of this function. The difference between the two versions is
how additional arguments specific to start and end locations are added.
This version requires all necessary arguments to be specified before
running and is best used in a non-interactive session. The
[`create_dist_between`](create_dist_between.md) version requires only
five arguments to be specified before running. Additional arguments
specific to identifying the lat/long of start or end points are added
through prompts. This function is designed for an interactive session.
Both versions of the distance between function require that the start
and end points be different vectors. If the start or ending points are
from a port, then `PortTable` must be specified to obtain lat/lons. If
the start or ending points are the center of a fishing zone or area then
`spat`, `lon.dat`, `lat.dat`, `cat`, `lon.spat`, and `lat.spat` must be
specified to obtain latitude and longitude.

## Usage

``` r
create_dist_between_for_gui(
  dat,
  project,
  start = c("lat", "lon"),
  end = c("lat", "lon"),
  units,
  name = "DistBetwen",
  portTable = NULL,
  zoneid,
  spat = NULL,
  lon.dat = NULL,
  lat.dat = NULL,
  cat = NULL,
  lon.spat = NULL,
  lat.spat = NULL
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name

- start, end:

  Starting location. Should be a port, lat/lon location, or centroid of
  regulatory zone/area.

  |  |  |  |  |
  |----|----|----|----|
  | port: | `start` should be the column name in `dat` containing the port names. Latitude and longitude for the port are extracted from the port table. | lat/lon: | `start` should be a character string of column names from `dat`. The order must be \`lat\` then \`lon\` `start=c('lat', 'lon')`. |

- units:

  Unit of distance. Choices are `"miles"`, `"kilometers"`, or
  `"midpoint"`.

- name:

  String, name of new variable. Defaults to \`DistBetween\`.

- portTable:

  Data table containing port data. Required if `start` or `end` are a
  vector from the `dat` containing port names.

- zoneid:

  Variable in `dat` that identifies the individual zones or areas.
  Required if zone identifier variable exists and is not \`ZoneID\`.
  Defaults to NULL.

- spat:

  Spatial data containing information on fishery management or
  regulatory zones. Shape, json, geojson, and csv formats are supported.
  Required if `start` or `end` are `"centroid"` and a centroid table
  doesn't exist in the FishSET database.

- lon.dat:

  Longitude variable from `dat`. Required if `start` or `end` are
  ‘centroid’.

- lat.dat:

  Latitude variable from `dat`. Required if `start` or `end` are
  ‘centroid’.

- cat:

  Variable or list in `spat` that identifies the individual areas or
  zones. If `spat` is class sf, `cat` should be name of list containing
  information on zones. Required if `start` or `end` are `"centroid"`.

- lon.spat:

  Variable or list from `spat` containing longitude data. Required for
  csv files. Leave as NULL if `spat` is a shape or json file, Required
  if `start` or `end` are `"centroid"`.

- lat.spat:

  Variable or list from `spat` containing latitude data. Required for
  csv files. Leave as NULL if `spat` is a shape or json file, Required
  if `start` or `end` are `"centroid"`.

## Value

Primary data set with distance between points variable added.
