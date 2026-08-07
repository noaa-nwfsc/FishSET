# Console function for rerunning project log

Console function for rerunning project log

## Usage

``` r
log_rerun(
  log_file,
  dat = NULL,
  portTable = NULL,
  aux = NULL,
  gridfile = NULL,
  spat = NULL,
  ind = NULL,
  run = FALSE
)
```

## Arguments

- log_file:

  String, name of the log file starting with the date (YYYY-MM-DD) and
  ending in ".json".

- dat:

  String, new primary data table to rerun log

- portTable:

  String, name of port table. Defualts to NULL.

- aux:

  String, name of auxiliary table. Defaults to NULL.

- gridfile:

  String, name of gridded data table. Defaults to NULL.

- spat:

  String, name of spatial data table. Defaults to NULL.

- ind:

  Numeric, indices of function calls to rerun.

- run:

  Logical, whether to run the logged function calls (TRUE) or simply
  list all function calls (FALSE).

## See also

[`log_rerun_gui`](log_rerun_gui.md)

## Examples

``` r
if (FALSE) { # \dontrun{
log_rerun("pollock_2020-10-23.json", run = TRUE) # reruns entire log with original data table
# runs log with new data table
log_rerun("pollock_2020-10-23.json", dat = "pollockMainDataTable", run = TRUE) 
} # }
```
