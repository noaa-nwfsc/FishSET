# Reset log file

Reset log file

## Usage

``` r
log_reset(project, over_write = FALSE)
```

## Arguments

- project:

  Project name.

- over_write:

  Logical, whether to over write an existing log file. This only applies
  if a log was created and reset in the same day for the same project.
  See "Details".

## Details

Logs are saved by project name and date (date created, not date
modified). For example, "pollock_2021-05-12.json". Calls to log
functions are automatically appended to the existing project log file.
Resetting the log file will create a new project log file with the
current date. A log will not be reset if `log_reset()` is run the same
day the log was created (or if the log is reset two or more times in a
single day), unless `over_write = TRUE`. This will replace that day's
log file.

## See also

[`list_logs`](list_logs.md) [`project_logs`](project_logs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
log_reset("pollock")
} # }
```
