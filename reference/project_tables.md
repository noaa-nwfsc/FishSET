# Display database table names by project

Display database table names by project

## Usage

``` r
project_tables(project, ...)
```

## Arguments

- project:

  Name of project.

- ...:

  String, additional characters to match by.

## See also

[`list_tables`](list_tables.md), [`fishset_tables`](fishset_tables.md)

## Examples

``` r
if (FALSE) { # \dontrun{
project_tables("pollock")
project_tables("pollock", "main")
} # }
```
