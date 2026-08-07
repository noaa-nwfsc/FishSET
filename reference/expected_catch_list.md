# Get Expected Catch List

Returns the Expected Catch list from the FishSET database.

## Usage

``` r
expected_catch_list(project, name = NULL)
```

## Arguments

- project:

  Name of project.

- name:

  Name of expected catch table from the FishSET database. The table name
  will contain the string "ExpectedCatch". If `NULL`, the default table
  is returned. Use [`tables_database`](tables_database.md) to see a list
  of FishSET database tables by project.
