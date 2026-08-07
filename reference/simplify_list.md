# Simplify a list

Cleans a list and converts applicable leaf nodes to dataframes.

## Usage

``` r
simplify_list(l, format = FALSE)
```

## Arguments

- l:

  A list.

- format:

  Logical, whether to print list using pandoc markdown.

## Value

A list

## See also

[`clean_list`](clean_list.md) [`list_df`](list_df.md)
[`pander`](https://rdrr.io/pkg/pander/man/pander.html)

## Examples

``` r
if (FALSE) { # \dontrun{
simplify_list(list(A = 1:10, B = 11:20))

simplify_list(list(A = list(X = 1:10, Y = letters[1:10])))

simplify_list(list(A = 1:10, B = "Text", C = c("text", "text")))
} # }
```
