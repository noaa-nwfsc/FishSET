# Create date variables for facetting

Create date variables for facetting

## Usage

``` r
facet_period(dataset, facet_date, date, period = NULL)
```

## Arguments

- dataset:

  Dataset used to create tables/plots in function.

- facet_date:

  String, period to facet by ("year", "month", and "week").

- date:

  String, Data variable used to convert to periods.

- period:

  String, period name. Only needed if summarizing over time.
