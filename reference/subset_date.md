# Subset dataset by date value/range

Subset dataset by date value/range

## Usage

``` r
subset_date(dataset, date, filter, value)
```

## Arguments

- dataset:

  \`MainDataTable\` to filter.

- date:

  String, name of date variable to subset by.

- filter:

  String, filter type.

- value:

  A range of dates if `filter = "date_range"`, or integer if using a
  period filter.
