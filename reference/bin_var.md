# Creates numeric variables divided into equal sized groups

Creates numeric variables divided into equal sized groups

## Usage

``` r
bin_var(dat, project, var, br, name = "bin", labs = NULL, ...)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- var:

  Numeric variable in `dat` to bin into a factor.

- br:

  Numeric vector. If a single number, the range of `var` is divided into
  `br` even groups. If two or more values are given, `var` is divided
  into intervals.

- name:

  Variable name to return. Defaults to \`bin\`.

- labs:

  A character string of category labels.

- ...:

  Additional arguments passed to
  [`cut`](https://rdrr.io/r/base/cut.html).

## Value

Returns the primary dataset with binned variable added.

## Details

Function adds a new factor variable, labeled by name, to the primary
dataset. The numeric variable is divided into equal sized groups if the
length of `br` is equal to one and into intervals if the length of `br`
is greater than one.

## Examples

``` r
if (FALSE) { # \dontrun{
 pollockMainDataTable <- bin_var(pollockMainDataTable, 'pollock', 'HAUL', 10, 'HAULCAT')
 pollockMainDataTable <- bin_var(pollockMainDataTable, 'pollock', 'HAUL', c(5,10), 'HAULCAT')
} # }
```
