# Reset confidentiality cache tables

This function deletes all confidentiality check tables stored in the
`"confid_cache.json"` file located in the project output folder.
Resetting this cache is recommended after a long period of use as check
tables can accumulate over time.

## Usage

``` r
reset_confid_cache(project)
```

## Arguments

- project:

  Project name

## See also

[`get_confid_cache`](get_confid_cache.md)
