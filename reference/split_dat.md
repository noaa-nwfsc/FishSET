# Separate secondary data table from MainDataTable

Separate secondary data table from MainDataTable

## Usage

``` r
split_dat(dat, aux = NULL, project, split_by = NULL, key, output = "main")
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- aux:

  Auxiliary data table in fishset_db or environment. Use string if
  referencing a table saved in the FishSET database. The column names
  from "aux" will be used to find and separate the auxiliary table from
  the MainDataTable.

- project:

  String, name of project.

- split_by:

  String, columns in MainDataTable to split by. These columns will be
  separated from MainDataTable. Must contain values from "key".

- key:

  String, the column(s) that link the main and auxiliary data tables. If
  using "aux" method, "key" must match a column in both MainDataTable
  and "aux" data table. If using "split_by", "key" must match a column
  in "MainDataTable and also be included in "split_by".

- output:

  String, return either the "main" data table, "aux" data table, or
  "both" main and aux data tables in a list.

## Details

This function separates auxiliary data (or gridded and port data) from
the MainDatatable. Users can either input the secondary data table (from
environment or fishset_db) to determine which columns to remove or by
passing a string of columns names to "split_by". Use either the "aux" or
the "split_by" method. Defaults to "aux" method if both arguments are
used.

## Examples

``` r
if (FALSE) { # \dontrun{
split_dat("pollockMainDataTable", "pollock", aux = "pollockPortTable", key = "PORT_CODE")
} # }
```
