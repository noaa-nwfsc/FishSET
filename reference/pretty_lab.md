# Format numbers in table

Format numeric columns.

## Usage

``` r
pretty_lab(tab, cols = "all", type = "pretty", ignore = NULL)
```

## Arguments

- tab:

  Table to format.

- cols:

  Character string of columns to format. defaults to `"all"` which will
  include all numeric variables in `tab`. If `ignore = TRUE` then the
  columns listed in `cols` will be not be formatted and all other
  columns in `tab` will be formatted.

- type:

  The type of formatting to apply. `"pretty"` uses
  [`prettyNum`](https://rdrr.io/r/base/formatc.html) which uses commas
  (",") to mark big intervals. `"scientific"` uses scientific notation.
  `"decimal"` simply rounds to two decimal places.

- ignore:

  Logical, whether to exclude the columns listed in `cols` and apply
  formatting to all other columns in `tab`.
