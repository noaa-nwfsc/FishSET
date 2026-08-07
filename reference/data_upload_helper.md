# Upload data from file, FishSET DB, or working environment

Helper function that can read data from file, from FishSET DB, or a
dataframe in the working environment. Used for data upload functions:
`load_maindata`, `load_port`, `load_aux`, `load_grid`, `load_spatial`.

## Usage

``` r
data_upload_helper(dat, type, ...)
```

## Arguments

- dat:

  Reference to a dataframe. This can be a filepath, the name of an
  existing FishSET table, or a dataframe object in the working
  environment.

- type:

  The type of data to upload. Options include `"main"`, `"port"`,
  `"grid"`, `"aux"`, and `"spat"`.

- ...:

  Additional arguments passed to [`read_dat`](read_dat.md).

## Examples

``` r
if (FALSE) { # \dontrun{
dataset <- data_upload_helper(dat, type = "main")
} # }
```
