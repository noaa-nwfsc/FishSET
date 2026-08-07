# Check and convert lat/lon to decimal degrees

Check that latitude and longitude are in decimal degrees and the
variable sign is correct. Correct lat/lon if required.

## Usage

``` r
degree(
  dat,
  project,
  lat = NULL,
  lon = NULL,
  latsign = FALSE,
  lonsign = FALSE,
  replace = TRUE
)
```

## Arguments

- dat:

  Dataset containing latitude and longitude data.

- project:

  Project name.

- lat:

  Variable(s) containing latitude data. If `NULL` the function will
  attempt to search for all latitude variables by name (e.g. by matching
  "lat" or "LAT").

- lon:

  Variable(s) containing longitude data. If `NULL` the function will
  attempt to search for all longitude variables by name (e.g. by
  matching "lon" or "LON").

- latsign:

  How should the sign value of `lat` be changed? Choices are `NULL` for
  no change, `"neg"` to convert all positive values to negative, `"pos"`
  to convert all negative values to positive, and `"all"` to change all
  values.

- lonsign:

  How should the sign value of `lon` be changed? Choices are `NULL` for
  no change, `"neg"` to convert all positive values to negative, `"pos"`
  to convert all negative values to positive, and `"all"` to change all
  values.

- replace:

  Logical, should `lat` and `lon` in `dat` be converted to decimal
  degrees? Defaults to `TRUE`. Set to `FALSE` if checking for
  compliance.

## Value

Returns the primary dataset with the latitudes and longitudes converted
to decimal degrees if `replace = TRUE` or if Changing the sign.
Otherwise, a message indicating whether selected longitude and latitude
variables are in the correct format.

## Details

First checks whether any variables containing 'lat' or 'lon' in their
names are numeric. Returns a message on results. To convert a variable
to decimal degrees, identify the `lat` or `lon` variable(s) and set
`replace = TRUE`. To change the sign, set `latsign` (for `lat`) or
`lonsign` (for `lon = TRUE`. FishSET requires that latitude and
longitude be in decimal degrees.

## Examples

``` r
if (FALSE) { # \dontrun{
# check format
degree(pollockMainDataTable, 'pollock', lat = 'LatLon_START_LAT',
       lon = 'LatLon_START_LON')

# change signs and convert to decimal degrees
pollockMainDataTable <- degree(pollockMainDataTable, 'pollock', 
                               lat = 'LatLon_START_LAT', 
                               lon = 'LatLon_START_LON', latsign = FALSE, 
                               lonsign = FALSE, replace = TRUE)
} # }

```
