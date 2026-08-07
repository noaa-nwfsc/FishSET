# Create a within-group lagged difference variable

Create a within-group lagged difference variable

## Usage

``` r
group_diff(
  dat,
  project,
  group,
  sort_by,
  value,
  name = "group_diff",
  lag = 1,
  include_total_col = FALSE
)
```

## Arguments

- dat:

  Primary data frame over which to apply function. Table in FishSET
  database should contain the string \`MainDataTable\`.

- project:

  String, project name.

- group:

  String, the grouping variable(s) to sum `value` by. Used to create the
  "group_total" variable.

- sort_by:

  String, a date variable to order \`MainDataTable\` by.

- value:

  String, the value variable used to calculate lagged difference. Must
  be numeric.

- name:

  String, the name for the new variable. Defaults to "group_diff".

- lag:

  Integer, adjusts lag length. Defaults to 1.

- include_total_col:

  Logical, whether to remove the "group_total" variable created to
  calculate percentage. Defaults to `FALSE`.

## Details

`group_diff` creates a grouped lagged difference variable. `value` is
first summed by the variable(s) in `group`, then the difference
within-group is calculated. The "group_total" variable gives the total
value by group and can be dropped by setting
`include_total_col = FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
group_diff(pollockMainDataTable, "pollock", group = c("PERMIT", "TRIP_ID"),
           sort_by = "HAUL_DATE", value = "HAUL")
} # }
```
