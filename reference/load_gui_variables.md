# load_gui_variables

A helper function to load selected variables from an RDS file in the
FishSET GUI.

## Usage

``` r
load_gui_variables(project_name, folderpath)
```

## Arguments

- project_name:

  The name of the current project.

- folderpath:

  The file path to the project's root folder.

## Value

A list of saved variables if the file exists, otherwise returns NULL.
