# Import, parse, and save port data to FishSET database

A project must exist before running `load_port()`. See
[`load_maindata`](load_maindata.md) to create a new project.

## Usage

``` r
load_port(
  dat,
  port_name,
  project,
  over_write = TRUE,
  compare = FALSE,
  y = NULL
)
```

## Arguments

- dat:

  Dataset containing port data. At a minimum, must include three
  columns, the port names, and the latitude and longitude of ports.
  `dat` can be a filepath, a existing FishSET table, or a dataframe in
  the working environment.

- port_name:

  Variable containing port names. Names should match port names in
  primary dataset.

- project:

  String, name of project.

- over_write:

  Logical, if TRUE, saves over data table previously saved in the
  FishSET database.

- compare:

  Logical, should new data be compared to previously saved dataframe
  `y`.

- y:

  Name of previously saved table in FishSET database. `y` must be
  defined if `compare` is TRUE.

## Details

Runs a series of checks on the port data. The function checks that each
row is unique, that no variables are empty, and that column names are
case-insensitive unique. There data issues are resolved before the data
is saved to the database. If checks pass, runs the fishset_compare
function and saves the new data frame to the FishSET database. The data
is saved in the FishSET database as the raw data and the working data.
The naming convention for port tables is "projectPortTable". Date is
also attached to the name for the raw data. See
[`table_view`](table_view.md) to view/load port tables into the working
environment.

## See also

[`table_view`](table_view.md), [`load_maindata`](load_maindata.md),
[`write_dat`](write_dat.md)

## Examples

``` r
if (FALSE) { # \dontrun{
load_port(PortTable, over_write = TRUE, project  ='pollock',
          compare = TRUE, y = 'pollockPortTable01012011')
} # }
```
