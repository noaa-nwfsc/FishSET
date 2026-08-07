# Lag zone variable

Creates a lagged zone ID variable for haul data, where the lagged
location of the first haul is filled in with the port of departure. This
variable is required for data with multiple sets or hauls in a single
trip and for the full information model with Dahl's correction
([`logit_correction`](logit_correction.md)).

## Usage

``` r
lag_zone(
  dat,
  project,
  spat = NULL,
  port,
  port_name,
  port_lon,
  port_lat,
  trip_id,
  haul_order,
  starting_port,
  zoneID_dat,
  zoneID_spat = NULL,
  name = "startingloc",
  bufferval = 100,
  db_save = FALSE
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  Name of project

- spat:

  Spatial data. Required if *ZoneID* does not exists in `dat`. Shape,
  json, geojson, and csv formats are supported.

- port:

  Port data. Contains columns: Port_Name, Port_Long, Port_Lat. Table is
  generated using the [`load_port`](load_port.md) and saved in the
  FishSET database as the project and port table, for example
  'pollockPortTable'.

- port_name:

  String indicating the column in port table that contains the port name

- port_lon:

  String indication the column in port table that contains port
  longitude

- port_lat:

  String indication the column in port table that contains port latitude

- trip_id:

  Variable in `dat` that identifies unique trips.

- haul_order:

  Variable in `dat` containing information on the order that hauls occur
  within a trip. Can be time, coded variable, etc.

- starting_port:

  Variable in `dat` to identify port at start of trip.

- zoneID_dat:

  Variable in `dat` that identifies the individual zones or areas.

- zoneID_spat:

  Variable in `spat` that identifies the individual zones or areas.

- name:

  String, name of created variable. Defaults to name of the function if
  not defined.

- bufferval:

  Maximum buffer distance, in meters, for assigning observations to the
  closest zone polygon.

- db_save:

  Default = FALSE and only returns the dataframe without saving the the
  database. `db_save = TRUE` will save the dataframe with the lagged
  variable to the database.

## Value

Primary data set with starting location variable added.

## Details

Function creates the `startloc` vector that is required for multihaul
data and the full information model with Dahl's correction
[`logit_correction`](logit_correction.md). The vector is the lagged zone
ID - zone when the decision of where to fish next was made. Generally,
the first zone of a trip is the port of departure. The
[`assignment_column`](assignment_column.md) function is called to assign
starting port locations and haul locations to zones. If ZoneID exists in
`dat`, [`assignment_column`](assignment_column.md) is not called and the
following arguments are not required: `spat, zoneID_spat`.

## Examples

``` r
if (FALSE) { # \dontrun{
pcodMainDataTable <- lag_zone(pcodMainDataTable, 'pcod',
    map2, "pcodPortTable", "TRIP_SEQ", "HAUL_SEQ", "DISEMBARKED_PORT", 
 "START_LON", "START_LAT", "NMFS_AREA", "STARTING_LOC"
)
} # }
```
