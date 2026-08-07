# Calculate and view Getis-Ord statistic

Wrapper function to calculate global and local Getis-Ord by discrete
area

## Usage

``` r
getis_ord_stats(
  dat,
  project,
  varofint,
  zoneid,
  spat,
  cat,
  lon.dat = NULL,
  lat.dat = NULL,
  lon.spat = NULL,
  lat.spat = NULL
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- varofint:

  Numeric variable in `dat` to test for spatial high/low clustering.

- zoneid:

  Variable in `dat` that identifies the individual zones or areas.
  Define if exists in `dat` and is not named \`ZoneID\`. Defaults to
  NULL.

- spat:

  Spatial data containing information on fishery management or
  regulatory zones. See [`load_spatial`](load_spatial.md).

- cat:

  Variable in `spat` defining the individual areas or zones.

- lon.dat:

  Longitude variable in `dat`.Require if `zoneid` is not defined.

- lat.dat:

  Latitude variable in `dat`. Require if `zoneid` is not defined.

- lon.spat:

  Variable or list from `spat` containing longitude data. Required for
  csv files. Leave as NULL if `spat` is a shape or json file.

- lat.spat:

  Variable or list from `spat` containing latitude data. Required for
  csv files. Leave as NULL if `spat` is a shape or json file.

## Value

Returns a plot and table. Both are saved to the output folder.

## Details

Calculates the degree, within each zone, that high or low values of the
`varofint` cluster in space. Function utilizes the
[`localG`](https://r-spatial.github.io/spdep/reference/localG.html) and
[`knearneigh`](https://r-spatial.github.io/spdep/reference/knearneigh.html)
functions from the spdep package. The spatial input is a
row-standardized spatial weights matrix for computed nearest neighbor
matrix, which is the null setting for the
[`nb2listw`](https://r-spatial.github.io/spdep/reference/nb2listw.html)
function. Requires a data frame with area as a factor, the lon/lat
centroid for each area, the lat/lon outlining each area, and the
variable of interest (`varofint`) or a map file with lat/lon defining
boundaries of area/zones and variable of interest for weighting. Also
required is the lat/lon defining the center of a zone/area. If the
centroid is not included in the map file, then
[`find_centroid`](find_centroid.md) can be called to calculate the
centroid of each zone. If the variable of interest is not associated
with an area/zone then the [`assignment_column`](assignment_column.md)
function can be used to assign each observation to a zone. Arguments to
identify centroid and assign variable of interest to area/zone are
optional and default to NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
getis_ord_stats(pcodMainDataTable, project = 'pcod', varofint = 'OFFICIAL_MT_TONS',
  spat = spatdat, lon.dat = 'LonLat_START_LON', lat.dat = 'LonLat_START_LAT', cat = 'NMFS_AREA')
} # }
```
