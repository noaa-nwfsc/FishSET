# Log user-created functions or models

Log user-created functions or models

## Usage

``` r
log_func_model(x, project)
```

## Arguments

- x:

  Name of function.

- project:

  Project name.

## Details

Logs function name, arguments, and, call. Use this function to log
user-defined likelihood functions.

## Examples

``` r
if (FALSE) { # \dontrun{
my_func <- function(a, b) {
  a + b
}
log_func_model(my_func)
} # }
```
