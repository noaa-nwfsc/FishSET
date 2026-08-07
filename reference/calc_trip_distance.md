# Calculate Trip Distance

Calculates the total distance for a fishing trip and adds it as a new
column to the dataset.

## Usage

``` r
calc_trip_distance(
  name = "trip_distance",
  project,
  dat,
  port,
  trip_id,
  haul_order,
  starting_port,
  return_port,
  start_haul_lat,
  start_haul_lon,
  end_haul_lat,
  end_haul_lon,
  distance_unit = "miles",
  a = 6378137,
  f = 1/298.257223563
)
```

## Arguments

- name:

  String. Name of new variable for trip distance. Defaults to
  \`trip_distance\`.

- project:

  String. project name.

- dat:

  String or data frame. A string for the name of the main data table in
  the FishSET database (contains 'MainDataTable' in the name). Or a data
  frame of the main data table.

- port:

  String or data frame. A string for the name of the port table in the
  FishSET project database. Or a data frame of the port table. Note that
  the port table must at least include a port name, port latitude and
  port longitude.

- trip_id:

  String. Column name that represents the unique trip identifier in
  `dat`.

- haul_order:

  String. Column name in `dat` that identifies haul order within a trip.
  Can be time, coded variable, etc.

- starting_port:

  String. Column name in `dat` containing departure port for each trip.

- return_port:

  String. Column name in `dat` containing landing port for each trip.

- start_haul_lat:

  String. Column name in `dat` containing haul starting latitude.

- start_haul_lon:

  String. Column name in `dat` containing haul starting longitude.

- end_haul_lat:

  String. Column name in `dat` containing haul end latitude.

- end_haul_lon:

  String. Column name in `dat` containing haul end longitude.

- distance_unit:

  String. The desired unit for the output distance. Options are
  `"miles"` (default), `"kilometers"`, or `"meters"`. Aliases `"mi"` and
  `"km"` are also accepted.

- a:

  Numeric. Major (equatorial) radius of the ellipsoid. The default value
  is for WGS84 ellipsoid.

- f:

  Numeric. Ellipsoid flattening. The default value is for WGS84
  ellipsoid.

## Value

Returns the main data table with a new variable for trip distance.

## Details

This function calculates the total distance traveled during a fishing
trip by summing its four key segments: 1. Distance from the departure
port to the start of the first haul. 2. The cumulative distance covered
\*within\* each haul. 3. The cumulative distance traveled \*between\*
consecutive hauls. 4. The final distance from the end of the last haul
to the return port.

The function uses a vectorized approach with the \`dplyr\` package for
efficiency and relies on \`geosphere::distGeo\` for accurate geodesic
distance calculations based on the WGS84 ellipsoid model.

## Examples

``` r
if (FALSE) { # \dontrun{
main_data_with_distance <- calc_trip_distance(
  name = "total_trip_miles",
  project = "my_project",
  dat = main_data,
  port = port_data,
  trip_id = "TRIP_ID",
  haul_order = "HAUL_NUMBER",
  starting_port = "DEPARTURE_PORT_NAME",
  return_port = "LANDING_PORT_NAME",
  start_haul_lat = "START_LATITUDE",
  start_haul_lon = "START_LONGITUDE",
  end_haul_lat = "END_LATITUDE",
  end_haul_lon = "END_LONGITUDE",
  distance_unit = "miles"
)
} # }
```
