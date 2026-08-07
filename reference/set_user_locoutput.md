# Set user folder directory

Set user folder directory

## Usage

``` r
set_user_locoutput(loc_dir, project)
```

## Arguments

- loc_dir:

  Local user directory

- project:

  Name of project.

## Details

This function saves the local user directory to the project settings
file with a valid folder directory. This directory path is used for
inserting plots and tables from a folder outside the FishSET package
into the FishSET RMarkdown Template.

## See also

[`insert_plot`](insert_plot.md) [`insert_table`](insert_table.md)
[`get_proj_settings`](get_proj_settings.md)
