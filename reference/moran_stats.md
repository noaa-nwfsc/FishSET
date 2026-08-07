# Calculate Moran's I statistic

This wrapper function calculates global and local Moran's I statistics
to measure spatial autocorrelation by discrete area.

## Usage

``` r
moran_stats(dat, var, dat_zone, spat, spat_zone, project)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MaindataTable'.

- var:

  Numeric variable from `dat` to test for spatial autocorrelation.

- dat_zone:

  Variable in `dat` that identifies the individual zones or areas.
  Define if exists in `dat` and is not named \`ZoneID\`. Defaults to
  NULL.

- spat:

  Spatial data containing information on fishery management or
  regulatory zones. Shape, json, geojson, and csv formats are supported.

- spat_zone:

  Variable or list in `spat` that identifies the individual areas or
  zones. If `spat` is class sf, `cat` should be name of list containing
  information on zones.

- project:

  Name of the project

## Value

Returns a list with (1) global Moran's I stats, (2) Moran lagged plot,
(3) LISA cluster map summarizing local Moran's values

## Details

The function measures the degree of spatial autocorrelation and utilizes
functions from the \`spdep\` package. The function requires a spatial
file with latitude and longitude coordinates defining the boundaries of
areas or zones and a variable of interest (\`var\`) to test for spatial
autocorrelation.

## Examples

``` r
if (FALSE) { # \dontrun{
moran_stats(pcodMainDataTable, var='OFFICIAL_MT_TONS', dat_zone='zoneID',
  spat=spatdat, spat_zone='NMFS_AREA', project = 'pcod')
} # }
```
