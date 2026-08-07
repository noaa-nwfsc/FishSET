# Fit FishSET Discrete Choice Model

Estimates parameters for logit models using the RTMB (R Template Model
Builder) framework. This function takes a design object created by
[`fishset_design`](fishset_design.md), optimizes the negative
log-likelihood, and returns a comprehensive list of model results, fit
statistics, and diagnostics.

## Usage

``` r
fishset_fit(
  project,
  model_name,
  fit_name = NULL,
  distribution = NULL,
  robust = FALSE,
  return_full_prob_mat = FALSE,
  se_calc = TRUE,
  overwrite = FALSE,
  ...
)
```

## Arguments

- project:

  Character string. Name of the project.

- model_name:

  Character string. Name of the specific model design to fit. Must match
  a name saved in the project's 'ModelDesigns' table.

- fit_name:

  Character string (Optional). Name to assign to the resulting fit
  object in the database. Defaults to `paste0(model_name, "_fit")`.

- distribution:

  Character string. Distribution for the continuous catch component in
  EPMs. Options: `"normal"`, `"lognormal"`, `"weibull"`, default = NULL.

- robust:

  Logical. Default FALSE. If TRUE, uses numerically stable utility
  values.

- return_full_prob_mat:

  Logical. If TRUE, returns the full N_obs x J_alts matrix of
  probabilities for every alternative. Default is FALSE (returns only
  chosen probs) to save memory on large datasets.

- se_calc:

  Logical. Set `"se_calc" = TRUE` (default) to calculate standard
  errors. Set to FALSE for faster runtime during model selection.

- overwrite:

  Logical. Default FALSE. If TRUE, overwrites an existing model fit if
  `fit_name` already exists in the project database.

- ...:

  Additional arguments passed to the optimization control.

  - `control`: A list of control parameters passed to
    [`nlminb`](https://rdrr.io/r/stats/nlminb.html) (e.g.,
    `list(eval.max = 2000, iter.max = 2000)`).

  - `start_values`: A numeric vector of initial parameter values. Must
    match the number of predictors in the design matrix.

## Value

A list object of class `"fishset_fit"` containing, this list is also
saved in the project database:

- coefficients:

  Named vector of estimated parameters.

- coef_table:

  Data frame with Estimates, Std. Errors, Z-values, and P-values.

- vcov:

  Variance-covariance matrix of the parameters.

- opt:

  Raw optimization output from `nlminb`.

- logLik:

  The maximum log-likelihood value of the fitted model.

- null_logLik:

  The log-likelihood of a null model (random guessing).

- pseudo_R2:

  McFadden's Pseudo-R-squared.

- AIC, AICc, BIC:

  Information criteria for model comparison.

- accuracy:

  The proportion of observations where the model assigned the highest
  probability to the actual choice.

- fitted_values:

  Vector of predicted probabilities for the chosen alternatives.

- prob_matrix:

  Matrix of predicted probabilities for all alternatives (N_obs x
  J_alts).

- diagnostics:

  A list containing the Hessian, gradients, eigenvalues, and condition
  number.

## See also

[`fishset_design`](fishset_design.md) for creating the input design
object.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Standard fit using default settings
# This uses the design object named "clogit_design" saved in "MyProject"
fit_result <- fishset_fit(
  project = "MyProject",
  model_name = "clogit_design"
)
  
# 2. Advanced fit with custom optimization settings and start values
# 'control' and 'start_values' are passed via the '...' argument
fit_custom <- fishset_fit(
  project = "MyProject",
  model_name = "clogit_design",
  fit_name = "clogit_custom_fit",
    
  # Pass control list to nlminb (e.g., increase max iterations, turn on tracing)
  control = list(eval.max = 5000, iter.max = 5000, trace = 1),
    
  # Pass initial start values for the parameters (e.g., for 2 predictors)
  start_values = c(0.5, -0.2)
)
  
# 3. EPM - normal catch function
epm_fit <- fishset_fit(project = project,
  model_name = "epm1",
  fit_name = "epm_fit1",
  distribution = "normal"
)
} # }
```
