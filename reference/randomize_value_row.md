# Randomize value between rows

Randomize value between rows

## Usage

``` r
randomize_value_row(dat, project, value)
```

## Arguments

- dat:

  Primary data frame over which to apply function. Table in FishSET
  database should contain the string \`MainDataTable\`.

- project:

  Project name.

- value:

  String, variable name to be randomly distributed between rows.

## Details

This is one of the FishSET confidentiality functions. It is useful for
randomly assigning ID values between observations.

## Examples

``` r
if (FALSE) { # \dontrun{
randomize_value_row(pollockMainDataTable, "pollock", "PERMIT")
} # }
```
