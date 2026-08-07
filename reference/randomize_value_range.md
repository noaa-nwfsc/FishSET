# Randomize variable value by percentage range

Randomize variable value by percentage range

## Usage

``` r
randomize_value_range(dat, project, value, perc = NULL)
```

## Arguments

- dat:

  Primary data frame over which to apply function. Table in FishSET
  database should contain the string \`MainDataTable\`.

- project:

  Project name.

- value:

  String, name of variable to jitter.

- perc:

  Numeric, a vector of percentages to randomly adjust a column of values
  by. Defaults to a range of 0.05 - 0.15 (i.e. 5-15 percent of original
  value).

## Details

This is one of the FishSET confidentiality functions. It adjusts a value
by adding or substracting (chosen at random for each value) a percentage
of the value. The percentage is randomly sampled from a range of
percentages provided in the "perc" argument.

## Examples

``` r
if (FALSE) { # \dontrun{
randomize_value_range(pollockMainDataTable, "pollock", "LBS_270_POLLOCK_LBS")
} # } 
```
