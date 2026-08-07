# Assign observations to fishing zones

Assign each observation in the primary dataset to a fishery management
or regulatory zone. This function is primarily called by other functions
that require zone assignment but can also be used on its own.

## Usage

``` r
assignment_column(
  dat,
  project,
  spat,
  lon_dat,
  lat_dat,
  zoneID_spat,
  name = "ZoneID",
  closest_pt = FALSE,
  bufferval = NULL,
  lon_spat = NULL,
  lat_spat = NULL,
  hull_polygon = FALSE,
  epsg = NULL,
  log_fun = TRUE
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  name of project.

- spat:

  Spatial data containing information on fishery management or
  regulatory zones. `sf` objects are recommended, but `sp` objects can
  be used as well. If using a spatial table read from a csv file, then
  arguments `lon_spat` and `lat_spat` are required. To upload your
  spatial data to the FishSETFolder see
  [`load_spatial`](load_spatial.md).

- lon_dat:

  Longitude variable in `dat`.

- lat_dat:

  Latitude variable in `dat`.

- zoneID_spat:

  Variable or list in `spat` that identifies the individual areas or
  zones. If `spat` is class `sf`, `zoneID_spat` should be name of list
  containing information on zones.

- name:

  The name of the new assignment column. Defaults to `"ZoneID"`.

- closest_pt:

  Logical, if `TRUE`, observations that fall outside zones are classed
  as the closest zone polygon to the point.

- bufferval:

  Maximum buffer distance, in meters, for assigning observations to the
  closest zone polygon. If the observation is not within the defined
  `bufferval`, then it will not be assigned to a zone polygon. Required
  if `closest_pt = TRUE`.

- lon_spat:

  Variable or list from `spat` containing longitude data. Required for
  spatial tables read from csv files. Leave as `NULL` if `spat` is an
  `sf` or `sp` object.

- lat_spat:

  Variable or list from `spat` containing latitude data. Required for
  spatial tables read from csv files. Leave as `NULL` if `spat` is an
  `sf` or `sp` object.

- hull_polygon:

  Logical, if `TRUE`, creates convex hull polygon. Use if spatial data
  creating polygon are sparse or irregular.

- epsg:

  EPSG code. Manually set the epsg code, which will be applied to `spat`
  and `dat`. If epsg is not specified but is defined for `spat`, then
  the `spat` epsg will be applied to `dat`. In addition, if epsg is not
  specified and epsg is not defined for `spat`, then a default epsg
  value will be applied to `spat` and `dat` (`epsg = 4326`). See
  <http://spatialreference.org/> to help identify optimal epsg number.

- log_fun:

  Logical, whether to log function call (for internal use).

## Value

Returns primary dataset with new assignment column.

## Details

Function uses the specified latitude and longitude from the primary
dataset to assign each row of the primary dataset to a zone. Zone
polygons are defined by the spatial dataset. Set `hull_polygon` to
`TRUE` if spatial data is sparse or irregular. Function is called by
other functions if a zone identifier does not exist in the primary
dataset.

## Examples

``` r
if (FALSE) { # \dontrun{
pollockMainDataTable <- 
     assignment_column(pollockMainDataTable, "pollock", spat = pollockNMFSSpatTable,
                       lon_dat = "LonLat_START_LON", lat_dat = "LonLat_START_LAT")
} # }
```
