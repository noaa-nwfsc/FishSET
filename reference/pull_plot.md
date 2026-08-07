# Import and format plots to notebook file

Import and format plots to notebook file

## Usage

``` r
pull_plot(project, fun, date = NULL, conf = TRUE)
```

## Arguments

- project:

  Project name.

- fun:

  String, the name of the function that created the plot.

- date:

  the date the plot was created. If NULL, then the most recent version
  is retrieved.

- conf:

  Logical, whether to return suppressed confidential data. Unsuppressed
  output will be pulled if suppressed output is not available.

## Examples

``` r
if (FALSE) { # \dontrun{
pull_plot("pollock", "density_plot")
} # }
```
