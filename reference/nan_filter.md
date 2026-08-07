# Identify, remove, or replace NaNs

Replaces NaNs in the primary data with the chosen value or removes rows
containing NaNs

## Usage

``` r
nan_filter(
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

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

- x:

  Character string of variables to remove or replace NaNs.

- replace:

  Logical, If `TRUE`, NaNs are replaced. Defaults to `FALSE`.

- remove:

  Logical, if `TRUE`, removes the entire row of the dataset where NaN is
  present. Defaults to `FALSE`.

- rep.value:

  Value to replace all NaNs in a numeric column. Defaults to the mean
  value of the column. Other options include `"median"` or a numeric
  value, e.g. `rep.value = 0`.

- over_write:

  Logical, If `TRUE`, saves data over previously saved data table in the
  FishSET database. Defaults to `FALSE`.

## Value

If `replace` and `remove` are `FALSE` then a statement of whether NaNs
are found is returned. If either `replace` or `remove` is `TRUE` the
modified primary dataset is returned.

## Details

To check for NaNs across `dat` run the function specifying only `dat`
(`nan_filter(dataset, project)`). The function will return a statement
of which variables, if any, contain NaNs. To remove NaNs, use
`remove = TRUE`. All rows containing NaNs in `x` will be removed from
`dat`. To replace NaNs, use `replace = TRUE`. If both `replace` and
`remove` are `TRUE` then `replace` is used. If `replace` is `FALSE` and
`rep.value` is not defined, then NaNs are replaced with mean value. The
modified dataset will be returned if `replace = TRUE` or
`remove = TRUE`. Save the modified data table to the FishSET database by
setting `over_write = TRUE)`.

## Examples

``` r
if (FALSE) { # \dontrun{
nan_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT')

mod.dat <- nan_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT', 
                      replace = TRUE)
                      
mod.dat <- nan_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT',
                      replace = TRUE, rep.value = 0)
                      
mod.dat <- nan_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT', 
                      remove = TRUE)
} # }
```
