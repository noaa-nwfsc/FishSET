# Number of observations by temporal unit

View the number of observations by year, month, and zone in table format

## Usage

``` r
temp_obs_table(
  dat,
  project,
  x,
  zoneid = NULL,
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

  String, name of project.

- x:

  Variable in `dat` containing date variable.

- zoneid:

  Variable in `dat` that identifies the individual zones or areas.
  Defaults to NULL. Define if the name of the zone identifier variable
  is not \`ZoneID\`.

- spat:

  Spatial data containing information on fishery management or
  regulatory zones. Shape, json, geojson, and csv formats are supported.
  Required if `zoneid` does not exist in `dat`.

- lon.dat:

  Longitude variable in `dat`. Required if `zoneid` does not exist in
  `dat`.

- lat.dat:

  Latitude variable in `dat`. Required if `zoneid` does not exist in
  `dat`.

- cat:

  Variable or list in `spat` that identifies the individual areas or
  zones. If `spat` is class sf, `cat` should be name of list containing
  information on zones. Required if `zoneid` does not exist in `dat`.

- lon.spat:

  Variable or list from `spat` containing longitude data. Required if
  `zoneid` does not exist in `dat` and `spat` is a csv file. Leave as
  NULL if `spat` is a shape or json file.

- lat.spat:

  Variable or list from `spat` containing latitude data. Required if
  `zoneid` does not exist in `dat` and `spat` is a csv file. Leave as
  NULL if `spat` is a shape or json file.

## Details

Prints tables displaying the number of observations by year, month, and
zone. [`assignment_column`](assignment_column.md) is called to assign
observations to zones if `zoneid` does not exist in `dat`. Output is not
saved.

## Examples

``` r
if (FALSE) { # \dontrun{
temp_obs_table(pollockMainDataTable, spat = map2, x = "DATE_FISHING_BEGAN",
  lon.dat = "LonLat_START_LON", lat.dat = "LonLat_START_LAT", cat = "NMFS_AREA",
  lon.spat = "", lat.spat = ""
  )
} # }
```
