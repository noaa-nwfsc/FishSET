# Import and format plots to notebook file

Import and format plots to notebook file

## Usage

``` r
plot_format(x, project)
```

## Arguments

- x:

  Name of plot saved in output

- project:

  Name of project

## Examples

``` r
if (FALSE) { # \dontrun{
plot_format("pollock_species_catch_2020-05-29.png")
plot_format(pull_output("pollock", "species_catch", type = "plot"))
} # }
```
