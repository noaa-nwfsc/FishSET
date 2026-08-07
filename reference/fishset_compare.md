# Compare imported data table to the previously saved version of the data table

Compare imported data table to the previously saved version of the data
table

## Usage

``` r
fishset_compare(x, y, compare = c(TRUE, FALSE), project)
```

## Arguments

- x:

  Updated data table to be saved.

- y:

  Previously saved version of data table.

- compare:

  Logical, if TRUE, compares `x` to `y` before saving `x` to FishSET
  database.

- project:

  Name of project

## Details

Function is optional. It is designed to check for consistency between
versions of the same data frame so that the logged functions can be used
to rerun the previous analysis on the updated data. The column names,
including spelling and capitalization, must match the previous version
to use the logged functions to rerun code after data has been updated
(i.e., new year of data). The function is called by the data import
functions ([`load_maindata`](load_maindata.md),
[`load_port`](load_port.md), [`load_aux`](load_aux.md),
[`load_grid`](load_grid.md)). Set the `compare` argument to TRUE to
compare column names of the new and previously saved data tables. The
new data tables will be saved to the FishSET database if column names
match. Set the `compare` argument to FALSE if no previous versions of
the data table exist in the FishSET database. No comparison will be made
and the new file will be saved to the database.
