# Import, parse, and save data to the FishSET Database

`load_maindata()` saves the primary dataset to the FishSET Database
(located in the FishSETFolder) and is a required step. The primary data
will also be loaded into the working environment as a dataframe named
"projectMainDataTable". Running `load_maindata()` creates a new project
directory in the FishSETFolder. To see a list of existing projects run
[`projects()`](projects.md) or open the FishSETFolder.

## Usage

``` r
load_maindata(dat, project, over_write = FALSE, compare = FALSE, y = NULL)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. This can be the
  full path to the file, the name of a main table in the FishSET
  database, or a dataframe object in the working environment. Main
  tables in the FishSET database contain the string 'MainDataTable'. A
  complete list of FishSET tables can be display by running
  [`fishset_tables()`](fishset_tables.md).

- project:

  String, name of project. Cannot contain spaces.

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

The dataset is saved in the FishSET database as raw and working tables.
The table name is the `project` and the table type, 'MainDataTable'. The
raw table is the original, unedited table. The working table contains
any changes made to the table after uploading. An eight digit date
string is included in the name of the raw table (e.g.
"pollockMainDataTable20220210"). The primary data is loaded into the
working environment as ‘projectMainDataTable’. The `fishset_compare`
argument compares `dat` to an existing FishSET table in `y` and returns
a message noting basic differences between the two. The column names are
checked for case-insensitivity and uniqueness.

## See also

[`save_dat`](save_dat.md), [`write_dat`](write_dat.md),
[`load_data`](load_data.md), [`fishset_tables`](fishset_tables.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# upload data from filepath
load_maindata(dat = "PATH/TO/DATA", project = "pollock")

# upload from dataframe in working environment
load_maindata(dat = Mydata, project = 'pollock', over_write = TRUE, 
              compare = TRUE, y = 'MainDataTable01012011')
              
# upload from an exisitng FishSET primary data table
looad_maindata(dat = "pollockMainDataTable", project = "pollock2020")
} # }
```
