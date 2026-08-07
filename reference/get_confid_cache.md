# Return cached confidentiality tables

This function lists the confidentiality "check" tables used to suppress
values.

## Usage

``` r
get_confid_cache(project, show = "all")
```

## Arguments

- project:

  Name of project

- show:

  Output `"all"` tables, `"last"` table, or `"first"` table.

## Value

A list of tables containing suppression conditions.

## See also

[`reset_confid_cache`](reset_confid_cache.md)
