# Check for empty variables

Detects variables that contain all `NA`s and removes them if
`remove = TRUE`.

## Usage

``` r
empty_vars(dat, remove = TRUE)
```

## Arguments

- dat:

  The data.frame to check.

- remove:

  Logical, whether to remove empty variables.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- empty_vars(dat)
} # }
```
