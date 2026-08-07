# Merge data tables using a left join

Merge data tables using a left join

## Usage

``` r
merge_dat(
  dat,
  other,
  project,
  main_key,
  other_key,
  other_type,
  merge_type = "left"
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- other:

  A second data table to join to MainDataTable. Use string if
  referencing a table saved in the FishSET database.

- project:

  Project name.

- main_key:

  String, name of column(s) in MainDataTable to join by. The number of
  columns must match `other_key`.

- other_key:

  String, name of column(s) in the `other` table to join by. The number
  of columns must match `main_key`.

- other_type:

  String, the type of secondary data being merged. Options include "aux"
  (auxiliary), "grid" (gridded), "spat" (spatial), and "port".

- merge_type:

  String, the type of merge to perform. `"left"` keeps all rows from
  `dat` and merges shared rows from `other`. `"full"` keeps all rows
  from each table.

## Details

This function merges two datasets using a left join: all columns and
rows from the MainDataTable are kept while only matching columns and
rows from the secondary table are joined.

## Examples

``` r
if (FALSE) { # \dontrun{
 pollockMainDataTable <- 
    merge_dat("pollockMainDataTable", "pollockPortTable", "pollock", 
              main_key = "PORT_CODE", other_key = "PORT_CODE")
} # } 
```
