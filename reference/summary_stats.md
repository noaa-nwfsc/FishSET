# View summary statistics

View summary statistics in table format for entire dataset or for a
specific variable.

## Usage

``` r
summary_stats(dat, project, x = NULL, log_fun = TRUE)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  Name of project

- x:

  Optional. Variable in `dat` to view summary statistics for. If not
  defined, summary stats are displayed for all columns in the dataset.

- log_fun:

  Logical, whether to log function call (for internal use).

## Details

Prints summary statistics for each variable in the data set. If `x` is
specified, summary stats will be returned only for that variable.
Numeric variables are summarized by minimum, median, mean, maximum, and
the number of NA's, unique values, and zeros. Non-numeric variables are
summarized by first value and the number of NA's, unique values, and
empty values. Function is called in the [`data_check`](data_check.md)
function.

## Examples

``` r
if (FALSE) { # \dontrun{
summary_stats(pcodMainDataTable, project = "pcod")

summary_stats(pcodMainDataTable, project = "pcod", x = "HAUL")
} # }
```
