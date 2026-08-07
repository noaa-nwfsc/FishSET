# Create a binary vector from numeric, date, and character or factor vectors.

Create a binary vector from numeric, date, and character or factor
vectors.

## Usage

``` r
dummy_num(dat, project, var, value, opts = "more_less", name = "dummy_num")
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

- var:

  Variable in `dat` to create dummy variable from.

- value:

  String, value to set dummy variable by. If `var` is a date, value
  should be a year, If `var` is a factor, value should be a factor
  level. If `var` is numeric, value should be a single number or range
  of numbers \[use c(1,5)\].

- opts:

  String, how dummy variable should be defined. Choices are `"x_y"` and
  `"more_less’"`. For `"x_y"`, each element of `var` is set to 1 if the
  element matches `value`, otherwise 0. For `"more_less"`, each element
  of `var` less than `value` is set to 0 and all elements greater than
  `value` set to 1. If `var` is a factor, then elements that match value
  will be set to 1 and all other elements set to 0. Default is set to
  `"more_less"`.

- name:

  String, name of created dummy variable. Defaults to name of the
  function if not defined.

## Value

Returns primary dataset with dummy variable added.

## Details

For date variables, the dummy variable is defined by a date (year) and
may be either year `x` versus all other years (`"x_y"`) or before vs
after year `x` (`"more_less"`). Use this function to create a variable
defining whether or not a policy action had been implemented.  
Example: before vs. after a 2008 amendment:  
`dummy_num('pollockMainDataTable', 'Haul_date', 2008, 'more_less', 'amend08')`  
  

For factor variables, both choices in `opts` compare selected factor
level(s) against all other factor levels.  
Example: Fishers targeting pollock vs. another species:  
`dummy_num('pollockMainDataTable', 'GF_TARGET_FT', c('Pollock - bottom', 'Pollock - midwater'), 'x_y', 'pollock_target')`  
  

For numeric variables, `value` can be a single number or a range of
numbers. The dummy variable is the selected value(s) against all others
(`x_y`) or less than the selected value versus more than the selected
value (`more_less`). For `more_less`, the mean is used as the critical
value if a range of values is provided.

## Examples

``` r
if (FALSE) { # \dontrun{
pollockMainDataTable <- dummy_num(pollockMainDataTable, 'pollock', 'Haul_date', 2008, 
  'more_less', 'amend80')
} # }
```
