# Convert dataframe/matrix to html table

Convert dataframe/matrix to html table

## Usage

``` r
to_html_table(x, rownames = FALSE, ...)
```

## Arguments

- x:

  A vector or list object.

- rownames:

  Logical, whether to show rownames.

- ...:

  Arguments to pass to
  [`shiny::renderTable`](https://rdrr.io/pkg/shiny/man/renderTable.html)

## Value

Returns an HTML table or (if the object is not a matrix or dataframe)
the original object unmodified.
