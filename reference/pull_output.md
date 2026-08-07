# Retrieve output file name by project, function, and type

Retrieve output file name by project, function, and type

## Usage

``` r
pull_output(project, fun = NULL, date = NULL, type = "plot", conf = TRUE)
```

## Arguments

- project:

  Name of project

- fun:

  Name of function.

- date:

  Output file date in " the most recent output file is pulled.

- type:

  Whether to return the `"plot"` (.png), `"table"` (.csv), "notes"
  (.txt) or `"all"` files matching the project name, function, and date.

- conf:

  Logical, whether to return suppressed confidential data. Unsuppressed
  output will be pulled if suppressed output is not available.

## Examples

``` r
if (FALSE) { # \dontrun{
pull_output("pollock", "species_catch", type = "plot")
} # }
```
