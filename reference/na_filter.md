# Identify, remove, or replace NAs and NaNs

Replaces NAs and NaNs in the primary data with the chosen value or
removes rows containing NAs and NaNs.

## Usage

``` r
na_filter(
  dat,
  project,
  x = NULL,
  replace = FALSE,
  remove = FALSE,
  rep.value = "mean",
  over_write = FALSE
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

- x:

  Character string. Column(s) in `dat` in which to remove or replace
  NAs.

- replace:

  Logical, if `TRUE`, replaces NAs in a vector with `rep.value`.
  Defaults to `FALSE`.

- remove:

  Logical, if `TRUE` removes the entire row of the `dat` where NA is
  present in a `dat`. Defaults to `FALSE`.

- rep.value:

  Value to replace all NAs in a numeric column. Defaults to the mean
  value of the column. Other options include `"median"` or a numeric
  value, e.g. `rep.value = 0`.

- over_write:

  Logical, If `TRUE`, saves data over previously saved data table in the
  FishSET database.

## Value

If `replace` and `remove` are `FALSE` then a statement of whether NAs
are found is returned. If either `replace` or `remove` is `TRUE` the
modified primary dataset is returned.

## Details

To check for NAs across `dat` run the function specifying only `dat`
(`na_filter(dataset, project)`). The function will return a statement of
which variables, if any, contain NAs. To remove NAs, use
`remove = TRUE`. All rows containing NAs in `x` will be removed from
`dat`. To replace NAs, use `replace = TRUE`. If `replace = FALSE` and
`rep.value` is not defined, then NAs are replaced with mean value. The
modified dataset will be returned if `replace = TRUE` or
`remove = TRUE`. If both `replace` and `remove` are `TRUE` then
`replace` is used. Save the modified data table to the FishSET database
by setting `over_write = TRUE)`.

## Examples

``` r
if (FALSE) { # \dontrun{
na_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT')

mod.dat <- na_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT', 
                     replace = TRUE)
                     
mod.dat <- na_filter(pcodMainDataTable,'pcod', 'OFFICIAL_TOTAL_CATCH_MT',
                     replace = TRUE, rep.value = 0)
                     
mod.dat <- na_filter(pcodMainDataTable, 'pcod',
                     c('OFFICIAL_TOTAL_CATCH_MT', 'CATCH_VALUE'), 
                     remove = TRUE)
} # }
```
