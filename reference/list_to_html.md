# Convert list to HTML

Converts a list to an un-ordered HTML list (\<ul\>).

## Usage

``` r
list_to_html(l)
```

## Arguments

- l:

  A list.

## Value

An un-ordered HTML list.

## See also

[`simplify_list`](simplify_list.md)
[`collapse_leaf_r`](collapse_leaf_r.md) [`list_html_r`](list_html_r.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  l <- list(A = 'a text', B = list(b1 = 'b1 text', b2 = 'b2 text'),
            C = list(data.frame(A = 1:10, B = letters[1:10])))
  list_to_html(l)
} # }
```
