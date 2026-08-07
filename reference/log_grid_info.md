# Log grid file

Writes grid information to a JSON file located in the project data
directory.

## Usage

``` r
log_grid_info(project, grid_info, mod_type = "combine")
```

## Arguments

- project:

  Name of project.

- grid_info:

  List containing grid information.

- mod_type:

  String, "combine" for combined map files or "edit" for edited map
  files.

## Details

The grid log is a list containing information about the grid files
currently saved to the project data folder. Each grid entry contains
three fields: `grid_name`, `closure_name`, and `combined_areas`.
`grid_name` is the name of the original grid object. If the other two
fields are empty, this means that the grid file has not been altered and
is the same as the original. `closure_name` is the name of a second grid
file containing closure areas that were combined with `grid_name`.
`combined_areas` are the names/IDs of the closures areas from the
closure grid file that were combined with `grid_name`.

## See also

[`save_grid_cache`](save_grid_cache.md)
