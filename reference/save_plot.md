# Save plot to output folder

Save plot to output folder

## Usage

``` r
save_plot(project, func_name, ...)
```

## Arguments

- project:

  name of project.

- func_name:

  Name of function used to create plot.

- ...:

  addition arguments passed to
  [`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Examples

``` r
if (FALSE) { # \dontrun{
save_plot(project, "species_catch")
} # }
```
