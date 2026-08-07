# Randomize latitude and longitude points by zone

Randomize latitude and longitude points by zone

## Usage

``` r
randomize_lonlat_zone(dat, project, spat, lon, lat, zone)
```

## Arguments

- dat:

  Primary data frame over which to apply function. Table in FishSET
  database should contain the string \`MainDataTable\`.

- project:

  Project name.

- spat:

  Spatial data table containing regulatory zones. This can be a "spatial
  feature" or sf object.

- lon:

  String, variable name containing longitude.

- lat:

  String, variable name containing latitude.

- zone:

  String, column name contain the assigned zone. Must be the same for
  both the spatial data table and MainDataTable.

## Details

This is one of the FishSET confidentiality functions. It replaces
longitude and latitude values with randomly sampled coordinates from the
regulatory zone the observation occurred in.

## Examples

``` r
if (FALSE) { # \dontrun{
randomize_lonlat_zone(pollockMainDataTable, "pollock", spatdat, 
                   lon = "LonLat_START_LON", lat = "LonLat_START_LAT",
                   zone = "NMFS_AREA")
} # }
```
