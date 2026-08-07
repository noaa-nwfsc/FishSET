# Create factor variable from quantiles

Create a factor variable from numeric data. Numeric variable is split
into categories based on quantile categories.

## Usage

``` r
set_quants(
  dat,
  project,
  x,
  quant_cat = 0.25,
  custom_quant = NULL,
  name = "set_quants"
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

- x:

  Variable to transform into quantiles.

- quant_cat:

  Quantile options: `0.1` `0.2`, `0.25`, `0.33`, and `0.4`

  - 0.1: (0%, 10%, 20%, 30%, 40%, 50%, 60%, 70%, 80%, 90%, 100%)

  - 0.2: (0%, 20%, 40%, 60%, 80%, 100%)

  - 0.25: (0%, 25%, 50%, 75%, 100%)

  - 0.33: (0%, 33%, 66%, 100%)

  - 0.4: (0%, 10%, 50%, 90%, 100%)

- custom_quant:

  Vector, user defined quantiles (between 0-1)

- name:

  String, name of created vector. Defaults to name of the function if
  not defined.

## Value

Primary dataset with quantile variable added.

## Examples

``` r
if (FALSE) { # \dontrun{
pollockMainDataTable <- set_quants(pollockMainDataTable, 'pollock', 'HAUL', 
   quant_cat=.2, 'haul.quant')
} # }
```
