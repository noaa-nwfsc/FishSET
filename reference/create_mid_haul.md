# Creates haul midpoint latitude and longitude variables

Calculates latitude and longitude of the haul midpoint and adds two
variables to the primary data set: the midpoint latitude and the
midpoint longitude.

## Usage

``` r
create_mid_haul(
  dat,
  project,
  start = c("lon", "lat"),
  end = c("lon", "lat"),
  name = "mid_haul"
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

- start:

  Character string, variables in `dat` defining the longitude and
  latitude of the starting location of haul. Must be in decimal degrees.

- end:

  Character string, variables in `dat` defining the longitude and
  latitude of the ending location of haul. Must be in decimal degrees.

- name:

  String, name of new variable. Defaults to \`mid_haul\`.

## Value

Returns primary dataset with two new variables added: latitude and
longitude of haul midpoint.

## Details

Each row of data must be a unique haul. Requires a start and end point
for each observation.

## Examples

``` r
if (FALSE) { # \dontrun{
pollockMainDataTable <- create_mid_haul(pollockMainDataTable, 'pollock', 
    start = c('LonLat_START_LON', 'LonLat_START_LAT'), 
   end = c('LonLat_END_LON', 'LonLat_END_LAT'), name = 'mid_haul')
} # }
```
