# Create Previous Location/Area Variable

Creates a variable of the previous port/zone (previous area) or the
previous longitude/latitude for a vessel.

## Usage

``` r
previous_loc(
  dat,
  spat,
  project,
  starting_port,
  v_id,
  tripID,
  haulID,
  zoneID = NULL,
  spatID = NULL,
  date = NULL,
  lon = NULL,
  lat = NULL
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- spat:

  A spatial data file containing information on fishery management or
  regulatory zones boundaries. `sf` objects are recommended, but `sp`
  objects can be used as well. See [`dat_to_sf()`](dat_to_sf.md) to
  convert a spatial table read from a csv file to an `sf` object. To
  upload your spatial data to the FishSETFolder see
  [`load_spatial()`](load_spatial.md).

- project:

  String, name of project.

- starting_port:

  The name of the starting (or disembarking) port in `dat`.

- v_id:

  The name of the variable in `dat` that uniquely identifies vessels.

- tripID:

  Variable name in `dat` that uniquely identifies trips.

- haulID:

  Variable name in `dat` that uniquely identifies hauls.

- zoneID:

  Name of zone ID column in `dat`. Used to identify the previous area.
  Required for previous area variable.

- spatID:

  Name of zone ID column in `spat`. `spat` is used to assign ports to
  spatial areas. Required for previous area variable.

- date:

  Optional, a date variable to order hauls by.

- lon:

  Longitude variable from `dat`. Required for previous location
  variable.

- lat:

  Latitude variable from `dat`. Required for previous location variable.

## Details

`previous_loc()` can create a previous area or location variable.
"Previous area" is defined as the port or zone the vessel last visited.
The first area for each trip is the disembarking port (`starting_port`).
If a port is within a zone, the zone is returned. If a port is not
within a zone, the name of the port is returned. "Previous location" is
defined as the previous longitude and latitude of the vessel. The first
set of coordinates is the location of the port. Users must have a port
table saved to the FishSET database to use this function (see
[`load_port()`](load_port.md)). This variable can be used to define the
distance matrix (see
[`create_alternative_choice()`](create_alternative_choice.md)).
