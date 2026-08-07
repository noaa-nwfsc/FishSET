# Import, parse, and save out-of-sample data to FishSET database

`load_outsample()` saves out-of-sample dataset to the FishSET Database
(located in the FishSETFolder) and the structure must match the primary
dataset. A project must exist before running `load_outsample()`. See
[`load_maindata`](load_maindata.md) to create a new project. Note: if
the data are out-of-sample temporally then upload a new datafile, if the
data are only out-of-sample spatially then upload the primary data file
in this function.

## Usage

``` r
load_outsample(dat, project, over_write = FALSE, compare = FALSE, y = NULL)
```

## Arguments

- dat:

  Out-of-sample data containing information on hauls or trips with same
  structure as the primary data table. This can be the full path to the
  file, the name of a out-of-sample table in the FishSET database, or a
  dataframe object in the working environment. Out-of-sample tables in
  the FishSET database contain the string 'OutSampleDataTable'. A
  complete list of FishSET tables can be viewed by running
  [`fishset_tables()`](fishset_tables.md).

- project:

  String, name of project.

- over_write:

  Logical, If `TRUE`, saves data over previously saved data table in the
  FishSET database. Defaults to `FALSE`.

- compare:

  Logical, whether to compare new dataframe to previously saved
  dataframe `y`. See [`fishset_compare`](fishset_compare.md).

- y:

  Name of previously saved table in FishSET Database. `y` must be
  defined if `compare = TRUE`.

## Details

The out-of-sample dataset is saved in the FishSET database as raw and
working tables. The table name is the `project` and the table type,
'OutSampleDataTable'. The raw table is the original, unedited table. The
working table contains any changes made to the table after uploading. An
eight digit date string is included in the name of the raw table (e.g.
"pollockOutSampleDataTable20220210"). The out-of-sample data is loaded
into the working environment as ‘projectOutSampleDataTable’. The
`fishset_compare` argument compares `dat` to an existing FishSET table
in `y` and returns a message noting basic differences between the two.
The column names are checked for case-insensitivity and uniqueness.

## See also

[`load_maindata`](load_maindata.md), [`save_dat`](save_dat.md),
[`write_dat`](write_dat.md), [`load_data`](load_data.md),
[`fishset_tables`](fishset_tables.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# upload data from filepath
load_outsample(dat = "PATH/TO/DATA", project = "pollock")

# upload from dataframe in working environment
load_outsample(dat = MyData, project = 'pollock', over_write = TRUE, 
              compare = TRUE, y = 'OutSampleDataTable01012011')
              
# upload from an exisitng FishSET out-of-sample data table
load_outsample(dat = "pollockOutSampleDataTable", project = "pollock")
} # }
```
