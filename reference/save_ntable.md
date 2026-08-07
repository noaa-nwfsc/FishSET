# Save list of tables to output folder

Save list of tables to output folder

## Usage

``` r
save_ntable(table, project, func_name, id = "num", ...)
```

## Arguments

- table:

  List containing tables to save.

- project:

  project name.

- func_name:

  Name of function used to create table.

- id:

  String, id to append to function name. Options include "seq" to save
  by list entry number or "name" to save by list entry name.

- ...:

  addition arguments passed to
  [`write.csv`](https://rdrr.io/r/utils/write.table.html).

## Examples

``` r
if (FALSE) { # \dontrun{
save_ntable(tab_list, project, "species_catch")
} # }
```
