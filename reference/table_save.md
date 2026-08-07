# Save an existing FishSET DB table

`table_save()` updates existing FishSET DB tables. If the table doesn't
exist, the user is reminded to use the appropriate `load_` function.

## Usage

``` r
table_save(table, project, type, name = NULL)
```

## Arguments

- table:

  A dataframe to save to the FishSET Database.

- project:

  Name of project.

- type:

  The table type. Options include, `"main"` for primary data tables,
  `"port"` for port tables, `"grid"` for gridded tables, `"aux"` for
  auxiliary tables.

- name:

  String, table name. Applicable only for gridded, auxiliary, and
  spatial tables.
