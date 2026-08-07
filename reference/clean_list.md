# Remove unnecessary nested lists

Recursively removes unnecessary lists ("lame lists") from a list. A lame
list is a unnamed list containing a single list object (see
[`lame_list`](lame_list.md)).

## Usage

``` r
clean_list(l)
```

## Arguments

- l:

  A list.

## Value

a list.

## See also

[`simplify_list`](simplify_list.md) [`lame_list`](lame_list.md)

## Examples

``` r
if (FALSE) { # \dontrun{
clean_list(list(list(1:10)))
clean_list(list(A = list(1:10)))
clean_list(list(list(1:10), list(11:20)))
} # }
```
