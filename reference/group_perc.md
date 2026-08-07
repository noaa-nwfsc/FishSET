# Create a within-group percentage variable

Create a within-group percentage variable

## Usage

``` r
group_perc(
  dat,
  project,
  group = NULL,
  value,
  name = "group_perc",
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

  String, primary grouping variable(s). Used to create the "total_value"
  variable which sums `value` by `group`.

- value:

  String, the value variable used to calculate percentage. Must be
  numeric.

- name:

  String, the name for the new variable. Defaults to "group_perc".

- include_total_col:

  Logical, whether to remove the "total_value" variables created to
  calculate percentage. Defaults to `FALSE`.

## Details

`group_perc` creates a within-group percentage variable using a primary
group (`group`). The total value of `group` is stored in the
"total_value" variable, and the within-group total stored in
"group_total". The group percentage is calculated using these two
function-created variables. "total_value" can be dropped by setting
`include_total_col = TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
group_perc(pollockMainDataTable, "pollock", group = "PERMIT",
           value = "OFFICIAL_TOTAL_CATCH_MT")
           
group_perc(pollockMainDataTable, "pollock", group = "DISEMBARKED_PORT", value = "HAUL")
} # }
```
