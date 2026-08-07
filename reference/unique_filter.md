# Check rows are unique

Check for and remove non-unique rows from primary dataset.

## Usage

``` r
unique_filter(dat, project, remove = FALSE)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- remove:

  Logical, if `TRUE` removes non-unique rows. Defaults to `FALSE`.

## Value

Returns the modified primary dataset with non-unique rows removed if
`remove = TRUE`.

## Details

Output is determined by `remove`. If `remove = TRUE` then non-unique
rows are removed. If `remove = FALSE` then only a statement is returned
regarding the number of rows that are not unique.

## Examples

``` r
if (FALSE) { # \dontrun{
# check for unique rows
unique_filter(pollockMainDataTable)

# remove non-unique rows from dataset
mod.dat <- unique_filter(pollockMainDataTable, remove = TRUE)
} # }
```
