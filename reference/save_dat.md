# Save modified primary data table to FishSET database

Save modified primary data table to FishSET database

## Usage

``` r
save_dat(dat, project)
```

## Arguments

- dat:

  Name of data frame in working environment to save to FishSET database.

- project:

  String, name of project.

## Details

Use function to save modified data to the FishSET database. The primary
data is only saved automatically in data upload and data check
functions. It is therefore advisable to save the modified data to the
database before moving on to modeling functions. Users should use
primary data in the working environment for assessing data quality
issues, modifying the data, and generating new variables. Pulling the
primary data from the FishSET database on each function without manually
saving will result in a loss of changes.

## Examples

``` r
if (FALSE) { # \dontrun{
save_dat(pollockMainDataTable, 'pollock')
} # }
```
