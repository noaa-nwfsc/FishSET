# Calculate trip centroid variable

Calculate latitude and longitude variables (columns) containing the
geographic centroid of each trip

## Usage

``` r
calc_trip_centroid(dat, project, lon, lat, trip_id, weight_var = NULL)
```

## Arguments

- dat:

  String or data frame. A string for the name of the main data table in
  the FishSET database (contains 'MainDataTable' in the name). Or a data
  frame of the main data table.

- project:

  String. Project name.

- lon:

  String. Column name in `dat` containing longitudinal data.

- lat:

  String. Column name in `dat` containing latitudinal data.

- trip_id:

  String. Column name that represents the unique trip identifier in
  `dat`.

- weight_var:

  String. Optional. Column name in `dat` to use for computing a weighted
  average centroid. If `NULL` (the default), an unweighted (simple)
  average is calculated.

## Value

Returns the original data frame (`dataset`) with two new columns added:
`cent_lon` (centroid longitude) and `cent_lat` (centroid latitude).

## Details

This function computes the average longitude and latitude for each
unique trip, as defined by the `trip_id` column. If `weight_var` is
specified, the function calculates the weighted centroid.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'pollockMainDataTable' is a data frame

pollockMainDataTable <- calc_trip_centroid(
  dat = pollockMainDataTable, 
  project = 'pollock', 
  lon = 'LonLat_START_LON',
  lat = 'LonLat_START_LAT',
  trip_id = "TRIP_ID",
  weight_var = NULL
)
} # }
```
