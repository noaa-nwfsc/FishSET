# Check for common data quality issues

Check primary data for common data quality issues, such as NaNs, NAs,
outliers, unique rows, and empty variables.

## Usage

``` r
data_check(dat, project, x)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- x:

  Variable in `dat` to check for outliers. Must be in quotes if called
  from the FishSET database.

## Details

Prints summary stats for all variables in `dat`. Prints column names
that contain NaNs or NAs. Checks for outliers for specified variable
`x`. Checks that all column names are unique, whether any columns in
`dat` are empty, whether each row is a unique choice occurrence at the
haul or trip level, that data for either lat/lon or fishing area are
included. The function is also called by other functions.

## Examples

``` r
if (FALSE) { # \dontrun{
data_check(pcodMainDataTable, "OFFICIAL_TOTAL_CATCH_MT")
} # }
```
