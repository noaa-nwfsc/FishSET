# Retrieve/display meta data by project

Retrieve/display meta data by project

## Usage

``` r
pull_meta(project, tab.name = NULL, tab.type = NULL, format = FALSE)
```

## Arguments

- project:

  Project name.

- tab.name:

  String, table name. Optional, used to filter output to a specific
  table.

- tab.type:

  String, table type. Optional, used to filter output. Options include
  "main", "spat" (spatial), "port", "grid" (gridded), and "aux"
  (auxiliary).

- format:

  Logical, whether to format output using
  [`pander`](https://rdrr.io/pkg/pander/man/pander.html). Useful for
  displaying in reports.
