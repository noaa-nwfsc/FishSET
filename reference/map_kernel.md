# Kernel density (hotspot) plot

Kernel density (hotspot) plot

## Usage

``` r
map_kernel(
  dat,
  project,
  latlon,
  type = "contours",
  group = NULL,
  facet = FALSE,
  date = NULL,
  filter_date = NULL,
  filter_value = NULL,
  minmax = NULL
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- latlon:

  Character string, specified as latitude then longitude, in decimal
  degrees.

- type:

  String, plot type. Choices are `"point"`, `"contours"`, or
  `"gradient"`. Note if you have a group, you must facet when choosing
  `"gradient"` (cannot overlap polygons clearly).

- group:

  Optional group argument. Should be a factor with length of (# of
  observations), where each observation corresponds to the latlon
  coordinate of the same index. Recall that the legend will output the
  names of factor levels as you have named them (see
  [`?factor`](https://rdrr.io/r/base/factor.html)).

- facet:

  Optional facet parameter. TRUE if mapping each group as a separate
  facet. Defaults to FALSE.

- date:

  Optional date variable to filter data by.

- filter_date:

  Whether to filter data table by `"year"`, `"month"`, or
  `"year-month"`. `date` and `filter_value` must be provided. Defaults
  to `NULL`.

- filter_value:

  Integer (4 digits if year, 1-2 if month). The year, month, or
  year-month to filter data table by. Use a list if using
  `"year-month"`, with the format: list(year(s), month(s)). For example,
  `list(2011:2013, 5:7)` will filter the data table from May to July,
  2011-2013.

- minmax:

  Optional map extent argument, a vector (num) of length 4 corresponding
  to c(minlat, maxlat, minlon, maxlon).

## Value

Returns ggplot2 object. Map plot saved to Output folder.

## Examples

``` r
if (FALSE) { # \dontrun{
map_kernel(pollockMainDataTable, project = 'pollock', type = 'contours',
latlon = c('LonLat_START_LAT', 'LonLat_START_LON'), group = 'PORT_CODE',
facet = TRUE, minmax = NULL, date = 'FISHING_START_DATE',
filter_date = 'year-month', filter_value = list(2011, 2:4))
} # }
```
