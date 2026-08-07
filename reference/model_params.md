# Load model parameter estimates, standard errors, and t-statistic to console for the defined project

Returns parameter estimates, standard errors, and t-statistic from
running the discretefish_subroutine function. The table parameter must
be the full name of the table name in the FishSET database.

## Usage

``` r
model_params(project, output = "list")
```

## Arguments

- project:

  Name of project

- output:

  Options include list, table, or print.

## Details

Returns parameter estimates from running `discretefish_subroutine`. The
table argument must be the full name of the table name in the FishSET
database.

## Examples

``` r
if (FALSE) { # \dontrun{
model_params('pcod')
} # }
```
