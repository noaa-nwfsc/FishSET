# Collapse a data frame from haul-level to trip-level

Aggregates a data frame from individual observations (hauls) to a
summary level (trips). It provides flexible methods for collapsing
numeric, character, and temporal data.

## Usage

``` r
haul_to_trip(
  dat,
  project,
  trip_id,
  zoneID_dat,
  zone_fun = "mode",
  date_fun = "min",
  num_fun = "mean",
  char_fun = "mode",
  haul_count = TRUE,
  log_fun = TRUE
)
```

## Arguments

- dat:

  String, the name of the main data containing information on hauls or
  trips. Note that this is the project 'MainDataTable' in the FishSET
  database.

- project:

  String, name of project.

- trip_id:

  String. Column name that represents the unique trip identifier in
  `dat`.

- zoneID_dat:

  String, the name of the column identifying fishing zones. This column
  is handled separately from other columns.

- zone_fun:

  String, method for collapsing the 'zoneID_dat' variable. Options are
  `"first"`, `"last"`, or `"mode"`.

- date_fun:

  String, method for collapsing temporal columns. Options are `"mean"`,
  `"median"`, `"min"`, or `"max"`.

- num_fun:

  String, method for collapsing numeric columns. Options are `"mean"`,
  `"median"`, `"mode"`, `"min"`, `"max"`, or `"sum"`.

- char_fun:

  String, method for collapsing character or factor columns. Options are
  `"first"`, `"last"`, `"paste"`, or `"mode"`.

- haul_count:

  Logical, If `TRUE`, a column name "haul_count" is added, showing the
  number of hauls (rows) per trip in the original data.

- log_fun:

  Logical, If `TRUE`, the function call is logged for tracking.

## Value

A data frame where each row represents a single trip, aggregated
according to the specified methods.

## Details

The function aggregates columns based on their data type per unique trip
ID. For columns that are not numeric, date, or character, the function
defaults to taking the first observation for each trip.

## Examples

``` r
if (FALSE) { # \dontrun{
# Collapse the data from haul to trip level
trip_data <- haul_to_trip(
  dat = "pollockMainDataTable",
  project = "pollock",
  trip_id = "tripID",
  zone_col = "ZONE",
  zone_fun = "mode", # Use the most common zone for the trip
  date_fun = "min",   # Use the earliest haul date as the trip date
  num_fun = "sum",    # Sum the fish weight for the trip
  char_fun = "first", # Use the first vessel name recorded
  haul_count = TRUE
)
} # }
```
