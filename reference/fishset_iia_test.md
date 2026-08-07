# Hausman-McFadden Test for IIA

Performs the Hausman-McFadden specification test to check the
Independence of Irrelevant Alternatives (IIA) assumption for logit
models.

## Usage

``` r
fishset_iia_test(
  project,
  model_name,
  fit_name = NULL,
  omitted_zones = NULL,
  robust = FALSE,
  ...
)
```

## Arguments

- project:

  Character string. Name of the project.

- model_name:

  Character string. Name of the specific model design used.

- fit_name:

  Character string. Name of the full model fit object previously saved
  in the project database (created by [`fishset_fit`](fishset_fit.md)).

- omitted_zones:

  Character vector (Optional). The names of the zones (alternatives) to
  exclude from the restricted model. These must match the zone labels
  found in the original data. If NULL (default), a random zone is
  selected.

- robust:

  Logical. Default FALSE. If TRUE, uses numerically stable utility
  values, mirroring the fit in [`fishset_fit`](fishset_fit.md).

- ...:

  Additional arguments passed to the optimization of the restricted
  model (e.g., `control`, `start_values`).

## Value

A list object of class `"fishset_iia"` containing:

- statistic:

  The Hausman chi-squared test statistic.

- p_value:

  The p-value of the test.

- df:

  Degrees of freedom (number of coefficients compared).

- full_coefs:

  Coefficients from the full model (subsetted for comparison).

- restricted_coefs:

  Coefficients from the restricted model.

- description:

  Text interpretation of the result.

## Details

The test compares the estimates from a full model (fitted on all
alternatives) against a restricted model (fitted on a subset of
alternatives). If the IIA assumption holds, the coefficients common to
both models should not be systematically different.

A significant p-value (typically \< 0.05) indicates that the IIA
assumption has been violated, suggesting that the logit model may be
misspecified (e.g., unobserved correlation between alternatives). Note:
Alternative-Specific Constants (ASCs) are excluded from the statistical
comparison to prevent reference-level shift bias.
