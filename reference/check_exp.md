# Prepare expected catch matrix

Checks that specified expected catch matrices exists and is formatted
correctly when `make_model_design` is called.

## Usage

``` r
check_exp(ec, ec_names)
```

## Arguments

- ec:

  Expected catch list created by
  [`create_expectations`](create_expectations.md).

- ec_names:

  The names of the expected catch matrices to include in the model and
  how they are used (i.e. used together or separately). See the
  `expectcatchmodels` argument in `make_model_design`.

## Value

Returns a list containing the filtered expected catch list and specified
matrices to include in model.

## Details

Checks that `ec_names` is specified properly by identifying invalid
options such as combining 'individual' or 'all' with any other option or
including matrices that don't exist in `ec`.
