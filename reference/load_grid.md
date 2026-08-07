# Import, parse, and save gridded data to FishSET database

Gridded data is data that varies by two dimensions. Column names must be
zone names. Load, parse, and save gridded data to FishSET database. A
project must exist before running `load_grid()`. See
[`load_maindata`](load_maindata.md) to create a new project.

## Usage

``` r
load_grid(grid, name, project, over_write = TRUE)
```

## Arguments

- grid:

  File name, including path, of gridded data.

- name:

  Name gridded data should be saved as in FishSET database.

- project:

  String, name of project.

- over_write:

  Logical, If TRUE, saves dat over previously saved data table in the
  FishSET database.

## Details

Grid data is an optional data frame that contains a variable that varies
by the map grid (ex. sea surface temperature, wind speed). Data can also
vary by a second dimension (e.g., date/time). Both dimensions in the
gridded data file need to be variables included in the primary data set.
The grid locations (zones) must define the columns and the optional
second dimension defines the rows. The row variable must have the exact
name as the variable in the primary data frame that it will be linked
to. The function DOES NOT check that column and row variables match a
variable in the primary data set. The function checks that each row is
unique, that no variables are empty, and that column names are
case-insensitive unique. These data issues are resolved before the data
is saved to the database. The data is saved in the FishSET database as
the raw data and the working data. In both cases, the table name is the
`project` and the file name `x`. Date is attached to the name for the
raw data. The naming convention for gridded tables is
"projectNameGridTable". See [`table_view`](table_view.md) to view/load
gridded tables into the working environment.

## See also

[`table_view`](table_view.md), [`load_maindata`](load_maindata.md),
[`write_dat`](write_dat.md)

## Examples

``` r
if (FALSE) { # \dontrun{
load_grid(dat = 'pcodMainDataTable', name = 'SeaSurfaceTemp', 
          over_write = TRUE, project = 'pcod')
} # }
```
