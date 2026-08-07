# Find columns that can be converted to Date or Date-time class

Find columns that can be converted to Date or Date-time class

## Usage

``` r
date_cols(dat, out = "names", type = "both")
```

## Arguments

- dat:

  MainDataTable or dataframe to check.

- out:

  Whether to return the column `"names"` (the default) or a logical
  vector (`"logical"`).

- type:

  String, the type of date column to test for. Options are `"date"`,
  `"date_time"`, or `"both"`.

## Examples

``` r
if (FALSE) { # \dontrun{
date_cols(pollockMainDataTable) # returns column names
date_cols(pollockMainDataTable, "logical")
} # }
```
