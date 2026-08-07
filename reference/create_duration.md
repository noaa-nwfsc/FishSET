# Create duration of time variable

Create duration of time variable based on start and ending dates in
desired temporal units.

## Usage

``` r
create_duration(
  dat,
  project,
  start,
  end,
  units = c("week", "day", "hour", "minute"),
  name = "create_duration"
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

- start:

  Date variable from `dat` indicating start of time period.

- end:

  Date variable from `dat` indicating end of time period.

- units:

  String, unit of time for calculating duration. Must be `"week"`,
  `"day"`, `"hour"`, or `"minute"`.

- name:

  String, name of created vector. Defaults to name of the function if
  not defined.

## Value

Returns primary dataset with duration of time variable added.

## Details

Calculates the duration of time between two temporal variables based on
defined time unit. The new variable is added to the dataset. A duration
of time variable is required for other functions, such as
[`cpue`](cpue.md).

## Examples

``` r
if (FALSE) { # \dontrun{
pollockMainDataTable <- create_duration(pollockMainDataTable, 'pollock', 'TRIP_START', 'TRIP_END',
  units = 'minute', name = 'TripDur')
} # }
```
