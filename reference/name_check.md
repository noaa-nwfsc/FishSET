# Check for unique and syntatcic column names

Used for creating new columns.

## Usage

``` r
name_check(dat, names, repair = FALSE)
```

## Arguments

- dat:

  Dataset that will contain new columns.

- names:

  New names to be added to dataset.

- repair:

  Logical, whether to return repaired column names (`repair = TRUE`) or
  just check for unique column names (`repair = FALSE`).

## Details

`name_check()` first checks to see if new column names are unique and
returns an error if not. When `repair = TRUE`, `name_check()` will check
for unique column names and returns new column names that are unique and
syntactic (see
[`vec_as_names`](https://vctrs.r-lib.org/reference/vec_as_names.html)
for details).
