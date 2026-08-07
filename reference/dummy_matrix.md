# Create dummy matrix from a coded ID variable

Create dummy matrix from a coded ID variable

## Usage

``` r
dummy_matrix(dat, project, x)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

- x:

  Variable in `dat` used to generate dummy matrix.

## Details

Creates a dummy matrix of 1/0 with dimensions *\[(number of observations
in dataset) x (number of factors in x)\]* where each column is a unique
factor level. Values are 1 if the value in the column matches the column
factor level and 0 otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
PortMatrix <- dummy_matrix(pollockMainDataTable, 'pollock', 'PORT_CODE')
} # }
```
