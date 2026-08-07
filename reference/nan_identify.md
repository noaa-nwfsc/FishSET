# Identify NaNs and NAs

Check whether any columns in the primary dataset contain NAs or NaNs.
Returns column names containing NAs or NaNs.

## Usage

``` r
nan_identify(dat, project)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

## Value

Returns names of columns containing NAs or NaNs, if any.

## Details

Check whether any columns in the primary dataset contain NAs or NaNs.

## See also

[`na_filter`](na_filter.md) and [`nan_filter`](nan_filter.md)

## Examples

``` r
if (FALSE) { # \dontrun{
nan_identify(pcodMainDataTable, "pcod")
} # }
```
