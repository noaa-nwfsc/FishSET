# View FishSET Database table

Wrapper for
[`dbGetQuery`](https://dbi.r-dbi.org/reference/dbGetQuery.html). View or
call the selected table from the FishSET database.

## Usage

``` r
table_view(table, project)
```

## Arguments

- table:

  String, name of table in FishSET database. Table name must be in
  quotes.

- project:

  Name of project.

## Details

`table_view()` returns a table from a project's FishSET Database.

## See also

[`list_tables`](list_tables.md) to show existing tables by project and
type. [`fishset_tables`](fishset_tables.md) to show all tables in the
FishSETFolder.

## Examples

``` r
if (FALSE) { # \dontrun{
head(table_view('pollockMainDataTable', project = 'pollock'))
} # }
```
