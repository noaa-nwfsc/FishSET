# Detect an unnecessary nested list

If a list object contains an unnamed list containing another list or a
single object, it is a "lame list". For example: `list(list(1:10)))`.
The nested list in this example is unnecessary since it contains a
single vector, and therefore can be removed. Pruning lame lists can help
simplify a list object.

## Usage

``` r
lame_list(l)
```

## Arguments

- l:

  A list.

## Value

`TRUE` if list object is a lame list, `FALSE` if not.

## See also

[`clean_list`](clean_list.md)

## Examples

``` r
if (FALSE) { # \dontrun{
lame_list(list(list(1:10)))
lame_list(list(A = list(1:10)))
lame_list(list(list(1:10), list(11:20)))
} # }
```
