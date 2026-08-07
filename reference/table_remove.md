# Remove table from FishSET database

Wrapper for
[`dbRemoveTable`](https://dbi.r-dbi.org/reference/dbRemoveTable.html).
Remove a table from the FishSET database.

## Usage

``` r
table_remove(table, project)
```

## Arguments

- table:

  String, name of table in FishSET database. Table name must be in
  quotes.

- project:

  Name of project

## Details

Function utilizes sql functions to remove tables from the FishSET
database.

## Examples

``` r
if (FALSE) { # \dontrun{
table_remove('pollockMainDataTable', 'pollock')
} # }
```
