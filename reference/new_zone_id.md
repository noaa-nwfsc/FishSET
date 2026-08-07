# Create new zone IDs

Creates a new zone ID column for the combined zone file. Used when
combining regulatory zones with closure areas.

## Usage

``` r
new_zone_id(
  combined,
  id,
  grid = NULL,
  closure = NULL,
  inter = NULL,
  recast = TRUE
)
```

## Arguments

- combined:

  Combined version of grid and closure file.

- id:

  Character, the name of the zone ID column.

- grid:

  Grid file containing regulatory zones.

- closure:

  Closure file containing closure areas.

- recast:

  Logical, if `TRUE` `combined` is passed to
  [`recast_multipoly`](recast_multipoly.md).

## Details

This function assigns a new zone ID if the intersection of `grid` and
`closure` creates new, non-contiguous polygons. The ID naming convection
is the original name of the regulatory zone followed by a decimal and a
number. For example, if a closure area bi-sects regulatory zone A, the
resulting zone IDs would be "A.1" and "A.2".

## See also

[`recast_multipoly`](recast_multipoly.md)
[`combine_zone`](combine_zone.md)
