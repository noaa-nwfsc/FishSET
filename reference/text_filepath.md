# Create a filepath for a .txt document in the output folder

Create a filepath for a .txt document in the output folder

## Usage

``` r
text_filepath(project, fun_name)
```

## Arguments

- project:

  Name of project.

- fun_name:

  Name of function.

## Value

Useful for saving messages generated in functions.

## Examples

``` r
if (FALSE) { # \dontrun{
cat("message", file = text_filepath("my_project", "qaqc_output"))
} # }
```
