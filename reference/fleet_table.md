# Define and store fleet expressions

`fleet_table` saves a table of fleet expression to the FishSET database
which can then be applied to a dataset with
[`fleet_assign`](fleet_assign.md). The table must contain a 'condition'
and 'fleet' column with each row corresponding to a set of expressions
that will be used to assign observations to fleets. A table can be
created with the `cond` and `fleet_val` arguments or by uploading an
existing table that matches the format requirements. See 'Details' below
for examples of how tables can be formatted.

## Usage

``` r
fleet_table(
  dat,
  project,
  cond = NULL,
  fleet_val = NULL,
  table = NULL,
  save = TRUE
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- cond:

  String; a vector containing valid R expressions saved as strings. Must
  be used with the `fleet_val` argument. Each expression should be in
  quotes (double or single) with nested quotes indicated with escaped
  quotes (\\) or with the opposite quote used to contain the expression.
  For example, "species == 'cod'" and "species == \\cod\\" are both
  valid.

- fleet_val:

  String; a vector of fleet names to be assigned. Must be used with the
  `cond` argument.

- table:

  A data frame that has one condition column and one fleet column. See
  'Details' for table formatting.

- save:

  Logical; whether to save the current fleet_table to the FishSET
  database. Defaults to `TRUE`. Tables are saved in the format of
  'projectFleetTable'. Each project can only have one fleet table. New
  fleet definitions are appended to the exiting fleet table. See
  [`table_remove`](table_remove.md) to delete a table.

## Value

Returns a table of fleet conditions that is saved to the FishSET
database with the name 'projectFleetTable'.

## Details

Below is a simple example of a fleet table. For a fleet table to be
created, it must contain one "condition" column and one "fleet" column.
Each fleet definition can be as long as necessary. For example, the
first expression in the condition column example could also be
`"GEAR == 8 & species == 'pollock'"`. Use the '&' operator when
combining expressions.

|           |       |             |     |                    |
|-----------|-------|-------------|-----|--------------------|
| condition | fleet | 'GEAR == 8' | 'A' | 'species == "cod"' |

## Examples

``` r
if (FALSE) { # \dontrun{ 
fleet_table("MainDataTable", "myProject", 
            cond = c("GEAR == 8", "species == 'cod'", "area %in% c(640, 620)"),
            fleet_val = c("A", "B", "C"), save = TRUE
            ) 
} # }
```
