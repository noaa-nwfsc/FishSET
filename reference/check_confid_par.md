# Check confidentiality parameters

Check confidentiality parameters

## Usage

``` r
check_confid_par(rule, value)
```

## Arguments

- rule:

  String, `"n"` for rule of n, `"k"` for n/k.

- value:

  Numeric, for `rule = "n"` must be an integer of at least 2. For
  `rule = "k"` any numeric value from 0 to 100.

## Value

`TRUE` if confidentiality parameters are valid, `FALSE` if not.
