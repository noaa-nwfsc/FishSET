# Evaluate outliers in Data

`outlier_table()` returns a summary table which shows summary statistics
of a variable after applying several outlier filters.

## Usage

``` r
outlier_table(dat, project, x, sd_val = NULL, log_fun = TRUE)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- x:

  Variable or column number in `dat` to check for outliers.

- sd_val:

  Optional. Number of standard deviations from mean defining outliers.
  For example, `sd_val = 4` would mean values outside +/- 4 SD from the
  mean would be outliers.

- log_fun:

  Logical, whether to log function call (for internal use).

## Value

Table for evaluating whether outliers may exist in the selected data
column.

## Details

Returns a table of summary statistics (mean, median, standard deviation,
minimum, maximum, number of NAs, and skew of the data) for `x` after
values outside the outlier measure have been removed. Outlier measures
include 5-95% quantiles, 25-75% quantiles, mean +/-2SD, mean +/-3SD,
median +/-2SD, and median +/-3SD. Only one variable can be checked at a
time. Table is saved to the Output folder.

## Examples

``` r
if (FALSE) { # \dontrun{
outlier_table(pollockMainDataTable, 'pollock', x = 'HAUL')
} # }
```
