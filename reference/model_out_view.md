# Load discrete choice model output to console for the defined project

Returns output from running the discretefish_subroutine function. The
table parameter must be the full name of the table name in the FishSET
database.

## Usage

``` r
model_out_view(project, CV = FALSE)
```

## Arguments

- project:

  Name of project

- CV:

  Logical, `CV = TRUE` when viewing model output from training data in
  k-fold cross validation

## Details

Returns output from running `discretefish_subroutine`. The table
argument must be the full name of the table name in the FishSET
database. Output includes information on model convergence, standard
errors, t-stats, etc.

## Examples

``` r
if (FALSE) { # \dontrun{
model_out_view('pcod')
} # }
```
