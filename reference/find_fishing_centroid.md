# Create fishing or weighted fishing centroid

Create fishing or weighted fishing centroid

## Usage

``` r
find_fishing_centroid(
  dat,
  project,
  zoneID,
  weight.var = NULL,
  lon.dat,
  lat.dat,
  names = NULL,
  cent.name = NULL,
  log.fun = TRUE
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  Name of project

- zoneID:

  Variable in `dat` that identifies zonal assignments or the If `spat`
  is class sf, `zoneID` should be name of list containing information on
  zones.

- weight.var:

  Variable from `dat` for weighted average. If `weight.var` is defined,
  the centroid is defined by the latitude and longitude of fishing
  locations in each zone weighted by `weight.var`.

- lon.dat:

  Required. Longitude variable in `dat`.

- lat.dat:

  Required. Latitude variable in `dat`.

- names:

  Then names of the fishing centroid columns to be added. A vector of
  length two in the order of `c("lon", "lat")`. The default is
  `c("fish_cent_lon", "fish_cent_lat")` and
  `c("weight_cent_lon", "weight_cent_lat")` if `weight.var` is used.

- cent.name:

  A string to include in the centroid table name. Table names take the
  form of \`"projectNameFishCentroid"\` for fishing centroids.

- log.fun:

  Logical, whether to log function call (for internal use).

## Value

Returns primary dataset with fishing centroid and, if `weight.var` is
specified, the weighted fishing centroid.

## Details

Fishing centroid defines the centroid by mean latitude and longitude of
fishing locations in each zone. Weighted centroid defines the centroid
by the mean latitude and longitude of fishing locations in each zone
weighted by the `weight.var`. The fishing and weighted centroid
variables can be used anywhere latitude/longitude variables appear. Each
observation in `dat` must be assigned to a fishery or regulatory
area/zone. If the zone identifier exists in `dat` and is not called
`'ZoneID'`, then `zoneID` should be the variable name containing the
zone identifier. If a zone identifier variable does not exist in `dat`,
`spat` must be be specified and `zoneID` must be zone identifier in
`spat`. The `assignment_column` function will be run and a zone
identifier variable added to `dat`.
