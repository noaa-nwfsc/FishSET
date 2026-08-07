# Spatial data quality checks

This function performs spatial quality checks and outputs summary tables
and plots. Checks include percent of observations on land, outside
regulatory zone (`spat`), and on a zone boundary. If any observation
occurs outside the regulatory zones then summary information on distance
from nearest zone is provided. `spatial_qaqc` can filter out
observations that are not within the distance specified in
`filter_dist`.

## Usage

``` r
spatial_qaqc(
  dat,
  project,
  spat,
  lon.dat,
  lat.dat,
  lon.spat = NULL,
  lat.spat = NULL,
  id.spat = NULL,
  epsg = NULL,
  date = NULL,
  group = NULL,
  filter_dist = NULL
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  Name of project.

- spat:

  Spatial data containing information on fishery management or
  regulatory zones. `sf` objects are recommended, but `sp` objects can
  be used as well. If using a spatial table read from a csv file, then
  arguments `lon.spat` and `lat.spat` are required. To upload your
  spatial data to the FishSETFolder see
  [`load_spatial`](load_spatial.md).

- lon.dat:

  Longitude variable in `dat`.

- lat.dat:

  Latitude variable in `dat`.

- lon.spat:

  Variable or list from `spat` containing longitude data. Required for
  spatial tables read from csv files. Leave as `NULL` if `spat` is an
  `sf` or `sp` object.

- lat.spat:

  Variable or list from `spat` containing latitude data. Required for
  spatial tables read from csv files. Leave as `NULL` if `spat` is an
  `sf` or `sp` object.

- id.spat:

  Polygon ID column. Required for spatial tables read from csv files.
  Leave as `NULL` if `spat` is an `sf` or `sp` object.

- epsg:

  EPSG number. Manually set the epsg code, which will be applied to
  `spat` and `dat`. If epsg is not specified but is defined for `spat`,
  then the `spat` epsg will be applied to `dat`. In addition, if epsg is
  not specified and epsg is not defined for `spat`, then a default epsg
  value will be applied to `spat` and `dat` (`epsg = 4326`). See
  <http://spatialreference.org/> to help identify optimal epsg number.

- date:

  String, name of date variable. Used to summarize over year. If `NULL`
  the first date column will be used. Returns an error if no date
  columns can be found.

- group:

  String, optional. Name of variable to group spatial summary by.

- filter_dist:

  (Optional) Numeric, distance value to filter primary data by (in
  meters). Rows containing distance values greater than or equal to
  `filter_dist` will be removed from the data. This action will be saved
  to the filter table.

## Value

A list of plots and/or dataframes depending on whether spatial data
quality issues are detected. The list includes:

- dataset:

  Primary data. Up to five logical columns will be added if spatial
  issues are found: "ON_LAND" (if obs fall on land), "OUTSIDE_ZONE" (if
  obs occur at sea but outside zone), "ON_ZONE_BOUNDARY" (if obs occurs
  on zone boundary), "EXPECTED_LOC" (whether obs occurs at sea, within a
  zone, and not on zone boundary), and "NEAREST_ZONE_DIST_M" (distance
  in meters from nearest zone. Applies only to obs outside zone or on
  land).

- spatial_summary:

  Dataframe containing the percentage of observations that occur at sea
  and within zones, on land, outside zones but at sea, or on zone
  boundary by year and/or group. The total number of observations by
  year/group are in the "N" column.

- outside_plot:

  Plot of observations outside regulatory zones.

- land_plot:

  Plot of observations that fall on land.

- land_out_plot:

  Plot of observations that occur on land and are outside the regulatory
  zones (combines outside_plot and land_plot if both occur).

- boundary_plot:

  Plot of observations that fall on zone boundary.

- expected_plot:

  Plot of observations that occur at sea and within zones.

- distance_plot:

  Histogram of distance form nearest zone (meters) by year for
  observations that are outside regulatory grid.

- distance_freq:

  Binned frequency table of distance values.

- distance_summary:

  Dataframe containing the minimum, 1st quartile, median, mean, 3rd
  quartile, and maximum distance values by year and/or group.

## Examples

``` r
if (FALSE) { # \dontrun{
# run spatial checks
spatial_qaqc("pollockMainDataTable", "pollock", spat = NMFS_AREAS, 
             lon.dat = "LonLat_START_LON", lat.dat = "LonLat_START_LAT")
             
# filter obs by distance
spat_out <- 
     spatial_qaqc(pollockMainDataTable, "pollock", spat = NMFS_AREAS,
                  lon.dat = "LonLat_START_LON", lat.dat = "LonLat_START_LAT",
                  filter_dist = 100)
mod.dat <- spat_out$dataset
} # }
  
```
