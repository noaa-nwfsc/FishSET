# Convert DMS to Packed DMS

Convert DMS to Packed DMS

## Usage

``` r
dms_to_pdms(x, type, dec = FALSE, as_num = FALSE)
```

## Arguments

- x:

  Latitude or longitude vector.

- type:

  `"lat"` or `"lon"`.

- dec:

  Logical, whether to keep decimal if present.

- as_num:

  Logical, whether to convert to numeric. If `FALSE`, a character string
  is outputted.

## Details

Primarily used for testing whether [`degree()`](degree.md) can convert
Packed DMS to decimal degrees.
