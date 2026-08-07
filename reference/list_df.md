# Convert leaf node to a dataframe

Determines whether a leaf node contains an object that can be converted
to a dataframe.

## Usage

``` r
list_df(l)
```

## Arguments

- l:

  A list.

## Value

A list.

## See also

[`is_leaf_table`](is_leaf_table.md) [`simplify_list`](simplify_list.md)

## Examples

``` r
if (FALSE) { # \dontrun{
list_df(list(A = list(X = 1:10, Y = letters[1:10])))
list_df(list(A = 1:10, B = "Text", C = c("text", "text"))) # no conversion
} # }
```
