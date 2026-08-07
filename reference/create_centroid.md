# Create Centroid Table

Create a zonal or fishing centroid table. The centroid can be joined
with the primary data if `output = "dataset"`. The centroid table is
automatically saved to the FishSET Database.

## Usage

``` r
create_centroid(
  spat = NULL,
  dat = NULL,
  project,
  spatID = NULL,
  zoneID = NULL,
  lon.dat = NULL,
  lat.dat = NULL,
  weight.var = NULL,
  type = "zonal centroid",
  names = NULL,
  cent.name = NULL,
  output = "dataset"
)
```

## Arguments

- spat:

  Spatial data containing information on fishery management or
  regulatory zones. Required for `type = "zonal centroid"`, not required
  for `type = "fishing centroid"`. `spat` will be included in centroid
  table name.

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'. `dat` is not
  required if `type = "zonal centroid"` and `output = "centroid table"`.

- project:

  Name of project.

- spatID:

  Variable or list in `spat` that identifies the individual areas or
  zones. If `spat` is class sf, `spatID` should be name of list
  containing information on zones. Ignored if
  `type = "fishing centroid"`.

- zoneID:

  Variable in `dat` that identifies zonal assignments. `zoneID` is not
  required if `type = "zonal centroid"` and `output = "centroid table"`.

- lon.dat:

  Longitude variable in `dat`. Required for `type = "fishing centroid"`.

- lat.dat:

  Latitude variable in `dat`. Required for `type = "fishing centroid"`.

- weight.var:

  Variable from `dat` for weighted average (for
  `type = "fishing centroid"`. only). If `weight.var` is defined, the
  centroid is defined by the latitude and longitude of fishing locations
  in each zone weighted by `weight.var`.

- type:

  The type of centroid to create. Options include `"zonal centroid"` and
  `"fishing centroid"`. See other arguments for `type` requirements.

- names:

  Character vector of length two containing the names of the fishing
  centroid columns. The order should be `c("lon_name", "lat_name")`. The
  default names are `c("weight_cent_lon", "weight_cent_lat")` for
  weighted fishing centroid and `c("fish_cent_lon", "fish_cent_lat")`
  for unweighted fishing centroid.

- cent.name:

  A string to include in the centroid table name. Table names take the
  form of `"projectNameZoneCentroid"` for zonal centroids and
  `"projectNameFishCentroid"` for fishing centroids.

- output:

  Options are `"centroid table"`, `"dataset"`, or `"both"`.
  `"centroid table"` returns a table containing the zone name and the
  longitude and latitude of the centroid. `"dataset"` returns the
  primary table joined with the centroid table. `"both"` returns a list
  containing the merged primary table and the centroid table.
