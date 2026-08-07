# Get Alternative Choice List

Returns the Alternative Choice list from the FishSET database.

## Usage

``` r
alt_choice_list(project, name = NULL)
```

## Arguments

- project:

  Name of project.

- name:

  Name of Alternative Choice list in the FishSET database. The table
  name will contain the string "AltMatrix". If `NULL`, the default table
  is returned. Use [`tables_database`](tables_database.md) to see a list
  of FishSET database tables by project.
