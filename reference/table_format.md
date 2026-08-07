# Import and format saved tables to notebook file

Import and format saved tables to notebook file

## Usage

``` r
table_format(x, project)
```

## Arguments

- x:

  Name of table saved in output

- project:

  project name

## See also

[`pull_output`](pull_output.md)

## Examples

``` r
if (FALSE) { # \dontrun{
table_format("pollock_species_catch_2020-05-29.csv", 'pollock')
table_format(pull_output("pollock", "species_catch", type = "table"), 'pollock')
} # }
```
