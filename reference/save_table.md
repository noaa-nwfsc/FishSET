# Save table to output folder

Save table to output folder

## Usage

``` r
save_table(table, project, func_name, ...)
```

## Arguments

- table:

  table name.

- project:

  project name.

- func_name:

  function name.

- ...:

  addition arguments passed to
  [`write.csv`](https://rdrr.io/r/utils/write.table.html).

## Examples

``` r
if (FALSE) { # \dontrun{
save_table(count, project, "species_catch")
} # }
```
