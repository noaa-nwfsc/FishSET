# Histogram of latitude and longitude by grouping variable

Histogram of latitude and longitude by grouping variable

## Usage

``` r
spatial_hist(dat, project, group = NULL)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- group:

  Column in `dat` containing grouping categories.

## Value

Returns histogram of latitude and longitude by grouping variable. Output
returned to console and saved to Output folder.

## Details

Returns a histogram of observed lat/lon split by grouping variable.
Output printed to console and saved to Output folder. Function is used
to assess spatial variance/clumping of selected grouping variable.

## Examples

``` r
if (FALSE) { # \dontrun{
spatial_hist(pollockMainDataTable, 'pollock', 'GEAR_TYPE')
} # }
```
