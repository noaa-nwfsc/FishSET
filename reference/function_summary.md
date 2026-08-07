# Display summary of function calls

Display summary of function calls

## Usage

``` r
function_summary(project, date = NULL, type = "dat_load", show = "all")
```

## Arguments

- project:

  Project name.

- date:

  Character string; the date of the log file (" retrieve. If `NULL` the
  most recent log is pulled.

- type:

  The type of function to display. "dat_load", "dat_quality",
  "dat_create", "dat_exploration", "fleet", and "model".

- show:

  Whether to display `"all"` calls, the `"last"` (most recent) call, or
  the `"first"` (oldest) function call from the log file.

## Details

Displays a list of functions by type and their arguments from a log
file. If no date is entered the most recent log file is pulled.

## See also

[`filter_summary`](filter_summary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
function_summary("pollock")
} # }
```
