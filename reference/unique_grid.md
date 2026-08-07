# Check for unique grid files

This function determines whether a grid file should be saved to the
project data folder based on the values in `grid_info`. If a match is
found, indicating that an identical grid file has already been saved, it
is deleted. If no match is found the grid file is saved.

## Usage

``` r
unique_grid(project, grid_info, ind = TRUE)
```

## Arguments

- project:

  Name of project.

- grid_info:

  List of grid characteristics used to determine whether a grid should
  be saved to the project data folder.

- ind:

  Logical, whether to return an index of unique grid or return a single
  logical value.

## Value

`TRUE` if a grid is unique (i.e. has no matches in the current grid
log).
