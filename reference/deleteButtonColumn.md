# A column of delete buttons for each row in the data frame for the first column

A column of delete buttons for each row in the data frame for the first
column

## Usage

``` r
deleteButtonColumn(df, id, ...)
```

## Arguments

- df:

  data frame

- id:

  id prefix to add to each actionButton. The buttons will be id'd as
  id_INDEX.

## Value

A DT::datatable with escaping turned off that has the delete buttons in
the first column and `df` in the other function to create one action
button as string
