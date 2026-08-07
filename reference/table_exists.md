# Check if table exists in the FishSET database for the defined project

Wrapper for
[`dbExistsTable`](https://dbi.r-dbi.org/reference/dbExistsTable.html).
Check if a table exists in the FishSET database.

## Usage

``` r
table_exists(table, project)
```

## Arguments

- table:

  Name of table in FishSET database.Table name must be in quotes.

- project:

  Name of project

## Value

Returns a logical statement of table existence.

## Examples

``` r
if (FALSE) { # \dontrun{
table_exists('pollockMainDataTable', 'pollock')
} # }
```
