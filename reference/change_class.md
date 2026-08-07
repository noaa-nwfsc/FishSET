# Change variable data class

View data class for each variable and call appropriate functions to
change data class as needed.

## Usage

``` r
change_class(dat, project, x = NULL, new_class = NULL, save = FALSE)
```

## Arguments

- dat:

  Primary data frame over which to apply function. Table in FishSET
  database should contain the string \`MainDataTable\`.

- project:

  Name of project.

- x:

  A character string of variable(s) in `dat` that will be changed to
  `new_class`. One ore more variables may be included. Default set to
  NULL.

- new_class:

  A character string of data classes that `x` should be changed to.
  Length of `new_class` should match the length of `x` unless all
  variables in `x` should be the same `new_class`. Defaults to NULL.
  Options are "numeric", "factor", "date", "character". Must be in
  quotes.

- save:

  Logical. Should the data table be saved in the FishSET database,
  replacing the working data table in the database? Defaults to FALSE.

## Value

Table with data class for each variable and the working data with
modified data class as specified.

## Details

Returns a table with data class for each variable in `dat` and changes
variable classes. To view variable classes run the function with default
settings, specifying only `dat` and `project`. If variable class should
be changed, run the function again, specifying the variable(s) `(x)` to
be changed and the new_class(es) `(new_class)`. Set `save` to TRUE to
save modified data table.

## Examples

``` r
if (FALSE) { # \dontrun{
#View table without changing class or saving
change_class(pollockMainDataTable, "myproject")

#Change class for a single variable and save data table to FishSET database
change_class(pollockMainDataTable, "myproject", x = "HAUL", new_class = 'numeric', save=TRUE)

#Change class for multiple variables and save data table to FishSET database
change_class(pollockMainDataTable, "myproject", x = c("HAUL","DISEMBARKED_PORT"),
 new_class = c('numeric', 'factor'), save=TRUE)
} # }
```
