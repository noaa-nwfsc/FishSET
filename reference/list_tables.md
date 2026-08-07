# Display FishSET database tables by type

Show project table names by table type. To see all tables for all
projects in the FishSETFolder, use
[`fishset_tables`](fishset_tables.md).

## Usage

``` r
list_tables(project, type = "main")
```

## Arguments

- project:

  A project name to show main tables by.

- type:

  the type of fishset_db table to search for. Options include "main"
  (MainDataTable), "port" (PortTable), "spat" (SpatTable), "grid"
  (GridTable), "aux" (AuxTable) "ec" (ExpectedCatch), "altc"
  (AltMatrix), "info" (MainDataTableInfo), "gc" (ldglobalcheck), "fleet"
  (FleetTable), "filter" (FilterTable), "centroid" (Centroid or
  FishCentroid), "model" (ModelOut), "model data" (ModelInputData),
  "outsample" (OutSampleDataTable).

## Examples

``` r
if (FALSE) { # \dontrun{
list_tables("pollock", type = "main")
list_tables("pollock", "ec")
} # }
```
