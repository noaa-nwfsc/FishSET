# Retrieve grid log file

Retrieves the grid log file for a project. The grid log shows which grid
files are currently saved to the project data folder.

## Usage

``` r
get_grid_log(project)
```

## Arguments

- project:

  Name of project.

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

## Examples

``` r
if (FALSE) { # \dontrun{
get_grid_log("pollock")
} # }
```
