# Jitter longitude and latitude variables

Jitter longitude and latitude variables

## Usage

``` r
jitter_lonlat(dat, project, lon, lat, factor = 1, amount = NULL)
```

## Arguments

- dat:

  Primary data frame over which to apply function. Table in FishSET
  database should contain the string \`MainDataTable\`.

- project:

  Project name.

- lon:

  String, variable name containing longitude.

- lat:

  String, variable name containing latitude.

- factor:

  Numeric, see [`jitter`](https://rdrr.io/r/base/jitter.html) for
  details.

- amount:

  Numeric, see [`jitter`](https://rdrr.io/r/base/jitter.html) for
  details. Default (NULL): factor \* d/5 where d is about the smallest
  difference between x values.

## Details

This is one of the FishSET confidentiality functions. It "jitters"
longitude and latitude using the base R function
[`jitter`](https://rdrr.io/r/base/jitter.html).

## Examples

``` r
if (FALSE) { # \dontrun{
jitter_lonlat(pollockMainDataTable, "pollock",
              lon = "LonLat_START_LON", lat = "LonLat_START_LAT")
} # }
```
