# Get Model Design List

Returns the Model Design list from the FishSET database.

## Usage

``` r
model_design_list(project, name = NULL)
```

## Arguments

- project:

  Name of project.

- name:

  Name of Model Design list in the FishSET database. The table name will
  contain the string "ModelInputData". If `NULL`, the default table is
  returned. Use [`tables_database`](tables_database.md) to see a list of
  FishSET database tables by project.
