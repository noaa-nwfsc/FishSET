# Aggregating function

Aggregating function

## Usage

``` r
agg_helper(
  dataset,
  value,
  period = NULL,
  group = NULL,
  within_group = NULL,
  fun = "sum",
  count = FALSE,
  format_tab = "decimal"
)
```

## Arguments

- dataset:

  \`MainDataTable\` to aggregate.

- value:

  String, name of variable to aggregate.

- period:

  String, name of period variable to aggregate by. Primarily for
  internal use. Places temporal variables to the right-end of the
  summary table.

- group:

  String, name of grouping variable(s) to aggregate by.

- within_group:

  String, name of grouping variable(s) for calculating within group
  percentages. `fun = "percent"` and `period` or `group` are required.

- fun:

  String, function name to aggregate by. Also accepts anonymous
  functions. To calculate percentage, set `fun = "percent"`; this will
  return the percent of total when `within_group = NULL`.

- count:

  Logical, if `TRUE` then returns the number of observations by `period`
  and/or `group`.

- format_tab:

  String. Options include `"decimal"` (default), `"scientific"`, and
  `"PrettyNum"` (rounds to two decimal places and uses commas).

## Examples

``` r
if (FALSE) { # \dontrun{

# total catch by port
agg_helper(pollockMainDataTable, value = "OFFICIAL_TOTAL_CATCH_MT", 
           group = "PORT_CODE", fun = "sum")

# count permits
agg_helper(pollockMainDataTable, value = "PERMIT", count = TRUE, fun = NULL)

# count permits by gear type
agg_helper(pollockMainDataTable, value = "PERMIT", group = "GEAR_TYPE",
           count = TRUE, fun = NULL)

# percent of total by gear type
agg_helper(pollockMainDataTable, value = "PERMIT", group = "GEAR_TYPE",
           count = TRUE, fun = "percent")
 
# within group percentage          
agg_helper(pollockMainDataTable, value = "OFFICIAL_TOTAL_CATCH_MT", 
           fun = "percent", group = c("PORT_CODE", "GEAR_TYPE"), 
           within_group = "PORT_CODE")
} # }
```
