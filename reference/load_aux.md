# Import, parse, and save auxiliary data to FishSET database

Auxiliary data is additional data that connects the primary dataset.
Function pulls the data, parses it, and then and saves the data to the
FishSET database. A project must exist before running `load_aux()`. See
[`load_maindata`](load_maindata.md) to create a new project.

## Usage

``` r
load_aux(dat, aux, name, over_write = TRUE, project = NULL)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- aux:

  File name, including path of auxiliary data.

- name:

  Name auxiliary data should be saved as in FishSET database.

- over_write:

  Logical, If TRUE, saves data over previously saved data table in the
  FishSET database.

- project:

  String, name of project.

## Details

Auxiliary data is any additional data beyond the primary data and the
port data. Auxiliary data can be any data that can be merged with the
primary dataset (ex. prices by date, vessel characteristics, or fishery
season). The auxiliary data does not have to be at a haul or trip level
but must contain a variable to connect the auxiliary data to the primary
dataset. The function checks that at least one column name of the
auxiliary data matches a column name in the primary dataset. The
function checks that each row is unique, that no variables are empty,
and that column names are case-insensitive unique. There data issues are
resolved before the data is saved to the database. The data is saved in
the FishSET database as the raw data and the working data. The naming
convention for auxiliary tables is "projectNameAuxTable". Date is also
added to the name for the raw data. See [`table_view`](table_view.md) to
view/load auxiliary tables into the working environment.

## See also

[`table_view`](table_view.md), [`load_maindata`](load_maindata.md),
[`write_dat`](write_dat.md)

## Examples

``` r
if (FALSE) { # \dontrun{
load_aux(pcodMainDataTable, name = 'FisherySeason', over_write = TRUE, 
         project = 'pcod')
} # }
```
