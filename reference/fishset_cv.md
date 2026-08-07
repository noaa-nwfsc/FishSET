# K-Fold Cross Validation for FishSET Models

Performs k-fold cross-validation on a fitted FishSET model to evaluate
out-of-sample predictive performance. Bypasses formula parsing by
directly subsetting the design matrix.

## Usage

``` r
fishset_cv(
  project,
  base_model_name,
  k = 5,
  seed = 42,
  distribution = NULL,
  ...
)
```

## Arguments

- project:

  Character string. Name of the project.

- base_model_name:

  Character string. The name of the original model design to
  cross-validate.

- k:

  Integer. The number of folds to create. Default is 5.

- seed:

  Integer. Random seed for reproducible fold generation. Default is 42.

- distribution:

  Character string. Distribution for the continuous catch component in
  EPMs.

- ...:

  Additional control arguments passed to
  [`fishset_fit()`](fishset_fit.md).

## Value

A list containing the average out-of-sample accuracy, log-likelihood,
PAPE, AIC, fold details, and estimated coefficients across folds.

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard logit
cv_results <- fishset_cv(
  project = "MyProject",
  base_model_name = "clogit_design"
)

# EPM - normal catch function
cv_epm_results <- fishset_cv(
  project = "MyProject",
  base_model_name = "epm_design",
  distribution = "normal"
)
} # }
```
