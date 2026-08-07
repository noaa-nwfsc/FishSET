# Import and format table to notebook file

Import and format table to notebook file

## Usage

``` r
pull_table(project, fun, date = NULL, conf = TRUE)
```

## Arguments

- project:

  Project name.

- fun:

  String, the name of the function that created the table.

- date:

  the date the table was created. If NULL, then the most recent version
  is retrieved.

- conf:

  Logical, whether to return suppressed confidential data. Unsuppressed
  output will be pulled if suppressed output is not available.

## Examples

``` r
if (FALSE) { # \dontrun{
pull_table("pollock", "vessel_count")
} # }
```
