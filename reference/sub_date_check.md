# Check subset date variable

Check subset date variable

## Usage

``` r
sub_date_check(sub_date, date, filter_date, group, facet_by)
```

## Arguments

- sub_date:

  String, name of date column to subset by.

- date:

  String, name of date column used in creating period variables.

- filter_date:

  The type of date filter to apply to the data.

- group:

  String, name of group variable(s). Many fleet function allow users to
  create a year, month, or week variable to group by. If grouping by
  period and `sub_date` and `date` are null, the function is stopped.

- facet_by:

  String, name of facetting variable(s). Many fleet function allow users
  to create a year, month, or week variable to facet by. If splitting by
  period and `sub_date` and `date` are null, the function is stopped.

## Value

`sub_date`. When used in a function, assign output to `sub_date`.
