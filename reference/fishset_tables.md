# Show all SQL Tables in FishSET Folder

Returns a data frame containing all tables from each project by project
name and table type.

## Usage

``` r
fishset_tables(project = NULL)
```

## Arguments

- project:

  Project name. If `NULL`, tables from all available projects will be
  displayed.

## Examples

``` r
if (FALSE) { # \dontrun{
# return all tables for all projects
fishset_tables()

# return all tables for a specific project
fishset_tables("pollock")
} # }
```
