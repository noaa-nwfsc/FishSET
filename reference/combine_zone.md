# Combine zone and closure area

Creates a new spatial dataset that merges regulatory zones with closure
areas.

## Usage

``` r
combine_zone(spat, closure, grid.nm, closure.nm, recast = TRUE)
```

## Arguments

- spat:

  Spatial file containing regulatory zones.

- closure:

  Closure file containing closure areas.

- grid.nm:

  Character, column name containing grid ID.

- closure.nm:

  Character, column name containing closure ID.

- recast:

  Logical, if `TRUE` `combined` is passed to `recast_multipoly`.

## Details

To combine zones with closure areas, this function performs the
following steps:

1.  Create the union of the closure area

2.  Take the difference between the closure union and the zone file

3.  Take the intersection of zone and the closure union

4.  Combine the difference and intersection objects into one spatial
    dataframe

5.  Assign new zone IDs to intersecting polygons

The result is a single spatial dataset containing all polygons from both
`spat` and `closure` with overlapping (intersecting) polygons receiving
new IDs (see [`new_zone_id`](new_zone_id.md)). This allows users to
partially close regulatory zones during the model design stage.

## See also

[`recast_multipoly`](recast_multipoly.md)
[`new_zone_id`](new_zone_id.md)
