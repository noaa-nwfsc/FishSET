# Lists fields for FishSET database table

Wrapper for
[`dbListFields`](https://dbi.r-dbi.org/reference/dbListFields.html).
View fields of selected table.

## Usage

``` r
table_fields(table, project)
```

## Arguments

- table:

  String, name of table in FishSET database. Table name must be in
  quotes.

- project:

  Project name

## Examples

``` r
if (FALSE) { # \dontrun{
table_fields('pollockMainDataTable', 'pollock')
} # }
```
