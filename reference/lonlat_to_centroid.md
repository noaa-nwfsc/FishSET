# Assign longitude and latitude points to zonal centroid

Assign longitude and latitude points to zonal centroid

## Usage

``` r
lonlat_to_centroid(dat, project, lon, lat, spat, zone)
```

## Arguments

- dat:

  Primary data frame over which to apply function. Table in FishSET
  database should contain the string \`MainDataTable\`.

- project:

  Project name.

- lon:

  String, variable name containing longitude.

- lat:

  String, variable name containing latitude.

- spat:

  Spatial data table containing regulatory zones. This can be a "spatial
  feature" or sf object.

- zone:

  String, column name contain the assigned zone. Must be the same for
  both the spatial data table and MainDataTable.

## Details

This is one of the FishSET confidentiality functions. It replaces the
selected longitude and latitude columns with the zonal centroid derived
from a spatial data table.

## Examples

``` r
if (FALSE) { # \dontrun{
lonlat_to_centroid(pollockMainDataTable, "pollock", spatdat, 
                  lon = "LonLat_START_LON", lat = "LonLat_START_LAT",
                  zone = "NMFS_AREA")
} # }
```
