# Save grid file to project data directory

Save grid file to project data directory

## Usage

``` r
save_grid_cache(project, grid_list, grid_info, mod_type = "combine")
```

## Arguments

- project:

  Name of project.

- grid_list:

  List containing grid files.

- grid_info:

  List containing grid information.

- mod_type:

  String, "combine" for combined map files or "edit" for edited map
  files.

## Details

This function references the grid log to determine whether a grid file
should be saved. If a grid file is unique it is saved, otherwise no
action is taken.

## See also

[`unique_grid`](unique_grid.md) [`log_grid_info`](log_grid_info.md)
