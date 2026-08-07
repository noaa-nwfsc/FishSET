# Summarize variable over data and time

View summary and exploratory statistics of selected variable by date and
zone.

## Usage

``` r
spatial_summary(
  dat,
  project,
  stat.var = c("length", "no_unique_obs", "perc_total", "mean", "median", "min", "max",
    "sum"),
  variable,
  spat,
  lon.spat = NULL,
  lat.spat = NULL,
  lon.dat = NULL,
  lat.dat = NULL,
  cat
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- stat.var:

  Options are `"length"`, `"no_unique_obs"`, `"perc_total"`, `"mean"`,
  `"median"`, `"min"`, `"max"`, and `"sum"`.

- variable:

  Variable in `dat` to summarize over date and zone.

- spat:

  Spatial data containing information on fishery management or
  regulatory zones. Shape, json, geojson, and csv formats are supported.
  Leave as NULL if the variable ‘ZoneID’ assigning observations to zones
  exists in `dat`.

- lon.spat:

  Variable or list from `spat` containing longitude data. Required for
  csv files. Leave as NULL if `spat` is a shape or json file or if the
  variable ‘ZoneID’ exists in `dat`.

- lat.spat:

  Variable or list from `spat` containing latitude data. Required for
  csv files. Leave as NULL if `spat` is a shape or json file, or if the
  variable ‘ZoneID’ exists in `dat`.

- lon.dat:

  Longitude variable in `dat`. Leave as NULL if the variable ‘ZoneID’
  (zonal assignment) exists in `dat`.

- lat.dat:

  Latitude variable in `dat`. Leave as NULL if the variable ‘ZoneID’
  (zonal assignments) exists in `dat`.

- cat:

  Variable or list in `spat` that identifies the individual areas or
  zones. If `spat` is class sf, `cat` should be name of list containing
  information on zones. Leave as NULL if the variable ‘ZoneID’ exists in
  `dat`.

## Value

Returns two plots, the variable aggregated by `stat.var` plotted against
date and against zone.

## Details

`stat.var` details:

|  |  |  |  |
|----|----|----|----|
| length: | Number of observations | no_unique_obs: | Number of unique observations |
| perc_total: | Percent of total observations | mean: | Mean |
| median: | Median | min: | Minimum |
| max: | Maximum | sum: | Sum |

## Examples
