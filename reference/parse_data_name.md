# Parse data name for logging

Parse data name for logging

## Usage

``` r
parse_data_name(dat, type, project)
```

## Arguments

- dat:

  Data table to be parsed.

- type:

  String, the data type: "main", "aux", "grid", "port", or "spat".

- project:

  project name.

## Details

If called while the shiny app is running, the data table name is pulled
from the FishSET project settings file. Otherwise, the data table from
the caller environment is used.
