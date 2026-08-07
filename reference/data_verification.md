# Check for common data quality issues that may be present in the data set.

Function tests for common data quality issues.

## Usage

``` r
data_verification(dat, project, log_fun = TRUE)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- log_fun:

  Logical, whether to log function call (for internal use).

## Value

Statements as to whether data quality issues may exist.

## Details

Checks that all columnn names in the data frame are unique, whether any
columns in the data frame are empty, whether each row is a unique choice
occurrence at the haul or trip level, and that either latitude and
longitude or fishing area are included.

## Examples

``` r
if (FALSE) { # \dontrun{
data_verification(pollockMainDataTable, 'pollock')
} # }
```
