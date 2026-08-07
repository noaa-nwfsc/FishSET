# Create numeric variable using arithmetic expression

Creates a new variable based on the arithmetic operation between two
variables. Function is useful for creating rate variables or the
summation of two related variables.

## Usage

``` r
create_var_num(dat, project, x, y, method, name = "create_var_num")
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

- x:

  Variable in `dat`. Variable will be the numerator if `method` is
  division.

- y:

  Variable in `dat` or numeric value. Variable will be the denominator
  if `method` is division.

- method:

  String, arithmetic expression. Options include: `"sum"`, addition
  (`"add"`), subtraction (`"sub"`), multiplication (`"mult"`), and
  division (`"div"`).

- name:

  String, name of created vector. Defaults to name of the function if
  not defined.

## Value

Returns primary dataset with new variable added.

## Details

Creates a new numeric variable based on the defined arithmetic
expression `method`. New variable is added to the primary dataset.

## Examples

``` r
if (FALSE) { # \dontrun{
pollockMainDataTable <- create_var_num(pollockMainDataTable, 'pollock', x = 'HAUL_CHINOOK',
    y = 'HAUL_CHUM', method = 'sum', name = 'tot_salmon')
} # }
```
