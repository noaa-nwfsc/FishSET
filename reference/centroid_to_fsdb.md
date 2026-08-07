# Save Primary Table's Centroid Columns to FishSET Database

Save the unique centroid values from the primary table to the FishSET
Database. Use this function if zone ID and centroid longitude/latitude
are included in the primary table.

## Usage

``` r
centroid_to_fsdb(
  dat,
  spat.name = NULL,
  project,
  zoneID,
  cent.lon,
  cent.lat,
  type = "zone"
)
```

## Arguments

- dat:

  Required, main data frame containing data on hauls or trips. Table in
  FishSET database should contain the string `MainDataTable`.

- spat.name:

  Optional, a name to associate with the centroid table.

- project:

  Name of project.

- zoneID:

  Variable in `dat` that identifies the individual zones or areas.

- cent.lon:

  Required, variable in `dat` that identifies the centroid longitude of
  zones or areas.

- cent.lat:

  Required, variable in `dat` that identifies the centroid latitude of
  zones or areas.

- type:

  The type of centroid. Options include `"zone"` for zonal centroids and
  `"fish"` for fishing centroids.

## Details

In certain cases, the user may have the necessary spatial variables to
run a discrete choice model included in the primary table when uploaded
to FishSET, and does not need a spatial table to assign observations to
zones or find centroids (e.g. by using
[`create_centroid()`](create_centroid.md)). However, a centroid table
table must be saved to the FishSET Database if a centroid option is used
to define alternative choice (see
[`create_alternative_choice()`](create_alternative_choice.md)).
`cent_to_fsdb()` allows users to save a zonal or fishing centroid table
provided they have the required variables: a zone ID (`zoneID`), a
centroid longitude (`cent.lon`), and a centroid latitude (`cent.lat`)
column.
