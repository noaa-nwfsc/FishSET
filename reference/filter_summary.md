# Select function calls to display

Select function calls to display

## Usage

``` r
filter_summary(sum_tab, filter_list)
```

## Arguments

- sum_tab:

  Summary table to filter.

- filter_list:

  A named list of integers. Each list entry should contain the name of
  the function and the row number(s) to filter by. For example,
  `list(temporal_mod = 2)` will display the second row of the
  temporal_mod dataframe in a summary list.

## See also

[`function_summary`](function_summary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
filter_summary(function_summary(),
              filter_list = list(set_quants = 2, temporal_mod = 2))
} # }
```
