# Check variables are not empty

Check for and remove empty variables from dataset. Empty variables are
columns in the data that contain all NAs and/or empty strings.

## Usage

``` r
empty_vars_filter(dat, project, remove = FALSE)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- remove:

  Logical, whether to remove empty variables. Defaults to `FALSE`.

## Value

Returns the dataset with empty variables removed if `remove = TRUE`.

## Details

Function checks for empty variables and prints an outcome message to the
console. If empty variables are present and `remove = TRUE`, then empty
variables will be removed from the dataset. Empty variables are columns
in the dataset that contain all NAs or empty strings.

## Examples

``` r
if (FALSE) { # \dontrun{
# check for empty vars
empty_vars_filter(pollockMainDataTable)

# remove empty vars from data
mod.dat <- empty_vars_filter(pollockMainDataTable, 'pollock', remove = TRUE)
} # }
```
