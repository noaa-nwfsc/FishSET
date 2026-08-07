# Load model comparison metrics to console for the defined project

Load model comparison metrics to console. Metrics are displayed for each
model that was fun. Metrics produced by `discretefish_subroutine`.

## Usage

``` r
model_fit(project, CV = FALSE)
```

## Arguments

- project:

  String, name of project.

- CV:

  Logical, `CV = TRUE` to get model fit for training data in k-fold
  cross validation routine.

## Examples

``` r
if (FALSE) { # \dontrun{
model_fit('pollock')
} # }
```
