# Edit project settings

Edit confidentiality settings, user output folder location, default plot
size, and tables currently used in the shiny app.

## Usage

``` r
edit_proj_settings(
  project,
  confid = NULL,
  user_out = NULL,
  tab_name = NULL,
  tab_type = NULL,
  plot_size = NULL,
  save_plot_rds = NULL
)
```

## Arguments

- project:

  Name of project.

- confid:

  List containing new confidentiality settings. See
  [`set_confid_check`](set_confid_check.md).

- user_out:

  Folder directory containing FishSET output. see
  [`set_user_locoutput`](set_user_locoutput.md).

- tab_name:

  Name of table loaded into shiny app.

- tab_type:

  Table type. Options include "main", "port", "spat", "grid", and "aux".

- plot_size:

  Plot size (width, height) in inches. Must be numeric.

- save_plot_rds:

  Logical, whether to save plot as an RDS file in the FishSETFolder
  ouput folder in addition to save as a PNG. This allows users to edit
  plots at a later time.

## See also

[`create_proj_settings`](create_proj_settings.md)
[`get_proj_settings`](get_proj_settings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
edit_project_settings("pollock", plot_size = c(5, 4))
} # }
```
