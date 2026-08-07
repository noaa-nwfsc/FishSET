# Check and correct spatial data format

Converts spatial data to a `sf` object

## Usage

``` r
check_spatdat(spatdat, lon = NULL, lat = NULL, id = NULL)
```

## Arguments

- spatdat:

  Spatial data containing information on fishery management or
  regulatory zones.

- lon:

  Longitude variable in `spatdat`. This is required for csv files or if
  `spatdat` is a dataframe (i.e. is not a `sf` or `sp` object).

- lat:

  Latitude variable in `spatdat`. This is required for csv files or if
  `spatdat` is a dataframe (i.e. is not a `sf` or `sp` object).

- id:

  Polygon ID column. This is required for csv files or if `spatdat` is a
  dataframe (i.e. is not a `sf` or `sp` object).

## Details

This function checks whether `spatdat` is a `sf` object and attempts to
convert it if not. It also applies [`clean_spat`](clean_spat.md) which
fixes certain spatial issues such as invalid or empty polygons, whether
a projected CRS is used (converts to WGS84 if detected), and if
longitude should be shifted to Pacific view (0-360 format) to avoid
splitting the Alaska region during plotting.
