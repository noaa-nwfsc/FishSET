# Save table to output folder

Save table to output folder

## Usage

``` r
save_nplot(project, func_name, plot_list, id = "num", ...)
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
save_nplot(project, "species_catch", plot_list)
} # }
```
