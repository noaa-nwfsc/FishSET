# Interactive application to create distance between points variable

Adds a variable for distance between two points to the primary dataset.
There are two versions of this function. The difference between the two
versions is how additional arguments specific to start and end locations
are added. This version requires only five arguments to be specified
before running. Additional arguments specific to identifying the lat/lon
of start or end points are added through prompts. This function is
designed for an interactive session. The
[`create_dist_between_for_gui`](create_dist_between_for_gui.md) function
requires all necessary arguments to be specified before running and is
best used in a non-interactive session. Both versions of the distance
between function require that the start and end points be different
vectors. If the start or ending points are from a port then `PortTable`
must be specified to obtain lat/lons. If the start or ending points are
the center of a fishing zone or area then `spat`, `lon.dat`, `lat.dat`,
`cat`, `lon.spat`, and `lat.spat` must be specified to obtain latitude
and longitude.

## Usage

``` r
create_dist_between(
  dat,
  project,
  start,
  end,
  units = c("miles", "meters", "km", "midpoint"),
  zoneid = NULL,
  name = "distBetween"
)
```

## Arguments

- dat:

  Primary data frame over which to apply function. Table in FishSET
  database should contain the string \`MainDataTable\`.

- project:

  Project name.

- start, end:

  Starting and ending location. Should be a port, lat/lon location, or
  the fishery management zone/area centroid. or area. If port is
  desired, start should be the column name in the `dat` containing the
  port Latitude and longitude for the port are extracted from the port
  table. If a lat/lon location is desired then start should be a
  character string of column names from `dat`. The order must be lon,
  lat. If fishery management centroid is used then set
  `start="centroid"` or `end="centroid"`.
  [`find_centroid`](find_centroid.md) and
  [`assignment_column`](assignment_column.md) will be called to identify
  the latitude and longitude if the centroid table does not exist in the
  FishSET database.

- units:

  Unit of measurement for calculated distance between start and ending
  points. Can be in `"miles"`, `"meters"`, `"kilometers"`, or
  `"midpoint"` location.

- zoneid:

  Variable in `dat` that identifies the individual zones or areas.
  Define if exists in `dat` and is not names \`ZoneID\`.

- name:

  String, output variable name. Defaults to \`distBetween\`.

## Value

Returns primary data set with distance between variable.

## Details

Additional arguments.  
Further arguments are required to identify the latitude and longitude of
the starting or ending location if `start` or `end` is defined as zonal
centroid or a column from primary dataset containing port information,
such as departing or embarking port. Prompts will appear asking for
required arguments.  
  
Port arguments required:

|     |     |     |     |
|-----|-----|-----|-----|
|     |     |     |     |

  
  
Centroids arguments required:

|  |  |  |  |
|----|----|----|----|
| spat: | Spatial data set containing information on fishery management or regulatory zones. Can be shape file, json, geojson, data frame, or list data frame or list. Required if `start` or `end` is centroid. | lon.dat: | Longitude variable from `dat`. |
| lat.dat: | Latitude variable from `dat`. | lon.spat: | Variable or list from `spat` containing longitude data. Required if `start` or `end` is centroid. Leave as NULL if `spat` is a shape or json file. |
| lat.spat: | Variable or list from `spat` containing latitude data. Required if `start` or `end` is centroid. Leave as NULL if `spat` is a shape or json file. | cat: | Variable or list in `spat` that identifies the individual areas or zones. If `spat` is class sf, `cat` should be the name of list containing information on zones. |

## Examples

``` r
if (FALSE) { # \dontrun{
pollockMainDataTable <- create_dist_between(pollockMainDataTable, 'pollock', 'centroid',
 'EMBARKED_PORT', units = 'miles', 'DistCentPort')

pollockMainDataTable <- create_dist_between(pollockMainDataTable, 'pollock', c('LonLat_START_LON',
 'LonLat_START_LAT'), c('LonLat_END_LON','LonLat_END_LAT'), units='midpoint', 'DistLocLock')
 
pollockMainDataTable <- create_dist_between(pollockMainDataTable, 'pollock', 'DISEMBARKED_PORT',
  'EMBARKED_PORT', units='meters', 'DistPortPort')
} # }
```
