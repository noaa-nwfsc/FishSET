# Create a within-group running sum variable

Create a within-group running sum variable

## Usage

``` r
group_cumsum(
  dat,
  project,
  group,
  sort_by,
  value,
  name = "group_cumsum",
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

  String, a date variable (or variables) to order \`MainDataTable\` by.

- value:

  String, the value variable used to calculate cumulative sum. Must be
  numeric.

- name:

  String, the name for the new variable. Defaults to "group_cumsum".

- include_total_col:

  Logical, whether to remove the "group_total" variable created to
  calculate percentage. Defaults to `FALSE`.

## Details

`group_cumsum` sums `value` by `group`, then cumulatively sums within
groups. For example, a running sum by trip variable can be made by
entering variables that identify unique vessels and trips into `group`
and a numeric variable (such as catch or \# of hauls) into `value`. Each
vessel's trip total is calculated then cumulatively summed. The
"group_total" variable gives the total value by group and can be dropped
by setting `include_total_col = TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
group_cumsum(pollockMainDataTable, "pollock", group = c("PERMIT", "TRIP_ID"),
             sort_by = "HAUL_DATE", value = "OFFICIAL_TOTAL_CATCH")
} # }
```
