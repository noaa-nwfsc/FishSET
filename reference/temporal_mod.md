# Transform units of date variables

Creates a new temporal variable by extracting temporal unit, such as
year, month, or day from a date variable.

## Usage

``` r
temporal_mod(
  dat,
  project,
  x,
  define_format = NULL,
  timezone = NULL,
  name = NULL,
  log_fun = TRUE,
  ...
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

- x:

  Time variable to modify from `dat`.

- define_format:

  Format of temporal data. `define_format` should be NULL if converting
  timezone for `x` but not changing format. Format can be user-defined
  or from pre-defined choices. Format follows
  [`as.Date`](https://rdrr.io/r/base/as.Date.html) format. See Details
  for more information.

- timezone:

  String, defaults to NULL. Returns the date-time in the specified time
  zone. Must be a recognizable timezone, such as "UTC",
  "America/New_York", "Europe/Amsterdam".

- name:

  String, name of created variables. Defaults to \`TempMod\`.

- log_fun:

  Logical, whether to log function call (for internal use).

- ...:

  Additional arguments. Use `tz=''` to specify time zone.

## Value

Primary data set with new variable added.

## Details

Converts a date variable to desired timezone or units using
[`as.Date`](https://rdrr.io/r/base/as.Date.html).
[`date_parser`](date_parser.md) is also called to ensure the date
variable is in an acceptable format for
[`as.Date`](https://rdrr.io/r/base/as.Date.html). `define_format`
defines the format that the variable should take on. Examples include
`"%Y%m%d"`, `"%Y-%m-%d %H:%M:%S"`. Users can define their own format or
use one of the predefined ones. Hours is 0-23. To return a list of
time-zone name in the Olson/IANA database paste
[`OlsonNames()`](https://rdrr.io/r/base/timezones.html) to the console.

- year: Takes on the format `"%Y"` and returns the year.

- month: Takes on the format `"%Y/%m"` and returns the year and month.

- day: Takes on the format `"%Y/%m/%d"` and returns the year, month, and
  day.

- hour: Takes on the format `"%Y/%m/%d %H"` and returns the year, month,
  day and hour.

- minute: Takes on the format `"%Y/%m/%d %H:%M"` and returns the year,
  month, day, hour, and minute.

For more information on formats, see
<https://www.stat.berkeley.edu/~s133/dates.html>.

## Examples

``` r
if (FALSE) { # \dontrun{
pcodMainDataTable <- temporal_mod(pcodMainDataTable, "pcod", 
   "DATE_LANDED", define_format = "%Y%m%d")
pcodMainDataTable <- temporal_mod(pcodMainDataTable, "pcod", 
   "DATE_LANDED", define_format = "year")
} # }


# Change to Year, month, day, minutes
```
