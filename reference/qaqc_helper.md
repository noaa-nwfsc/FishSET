# Helper function for testing data quality issues in MainDataTable

Helper function for testing data quality issues in MainDataTable

## Usage

``` r
qaqc_helper(dat, fun, output = "logical")
```

## Arguments

- dat:

  Dataframe to test for quality issues.

- fun:

  A function or custom function that returns a single logical value to
  apply to each column in `dat`. There are three quick options for
  common checks: `"NA"`, `"NaN"`, and `"Inf"`.

- output:

  `"logical"` returns a single logical value for each column in `dat`.
  `"names"` returns the column names that evaluate to `TRUE`.

## Details

Returns a vector of logical values (`output = "logical"`) or a vector of
column names where the condition evaluated by `fun` returns TRUE
(`output = "names"`).

## Examples
