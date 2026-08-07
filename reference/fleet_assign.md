# Create fleet variable using fleet definition table

Add a fleet ID column to the primary data using a fleet table (see
[`fleet_table`](fleet_table.md) for details).

## Usage

``` r
fleet_assign(
  dat,
  project,
  fleet_tab,
  assign = NULL,
  overlap = FALSE,
  format_var = "string"
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- fleet_tab:

  String, name of the fleet table stored in FishSET database. Should
  contain the string \`FleetTable\`.

- assign:

  Integer, a vector of row numbers from `fleet_tab`. Only fleet
  definitions in these rows will be used and added to \`MainDataTable\`.
  If `assign = NULL` (the default), all fleet definitions in the table
  will be used.

- overlap:

  Logical; whether overlapping fleet assignments are allowed. Defaults
  to `FALSE`.

- format_var:

  String. If `format_var = "string"`, a single column named "fleet" will
  be added to \`MainDataTable\`. If `overlap = TRUE`, observations with
  multiple fleet assignments are duplicated. `format_var ="dummy"`
  outputs a binary column for each fleet in the fleet table. Defaults to
  `"string"`.

## Value

Returns the primary dataset with added fleet variable(s).

## See also

[`fleet_table`](fleet_table.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fleet_assign(pollockMainDataTable, 'pollock', fleet_tab = 'pollockFleetTable', 
             overlap = TRUE)
} # }
```
