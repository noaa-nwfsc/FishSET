# Recast multi-polygons

Re-cast intersecting multi-polygons to polygons. Used when combining
regulatory zones with closure areas.

## Usage

``` r
recast_multipoly(grid, closure, combined, id, inter)
```

## Arguments

- grid:

  Grid file containing regulatory zones.

- closure:

  Closure file containing closure areas.

- combined:

  Combined version of grid and closure file.

- id:

  Character, the name of the zone ID column.

## Details

This function is primarily used to extract polygons during the ID
re-labeling process performed by [`new_zone_id`](new_zone_id.md). If a
polygon from `grid` intersected with `closure`, it is extracted from
it's multi-polygon and given a unique ID.

## See also

[`combine_zone`](combine_zone.md) [`new_zone_id`](new_zone_id.md)
