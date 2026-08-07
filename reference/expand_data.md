# Add missing dates and variable combos to \`MainDataTable\`.

Add missing dates and variable combos to \`MainDataTable\`.

## Usage

``` r
expand_data(
  dataset,
  project,
  date = NULL,
  value,
  sub_date = NULL,
  period = NULL,
  group = NULL,
  facet_by = NULL,
  fun = "sum"
)
```

## Arguments

- dataset:

  Object containing \`MainDataTable\`.

- project:

  Name of project.

- date:

  String, name of date variable to find missing days.

- value:

  String, name of value variable to be aggregated by `agg_helper`.

- sub_date:

  String, name of date variable to subset by.

- period:

  String, name of period variable(s).

- group:

  String, name of grouping variable(s).

- facet_by:

  String, name of variable(s) to be facetted (split).

## Details

This function expands the data to include missing periods/dates and
combinations of grouping variables that will be used to aggregate the
data. Only variables needed to aggregate the data are kept to minimize
memory usage. If confidentiality checks are turned on, the vessel ID
column is included as well.
