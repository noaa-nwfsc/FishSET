# Fit Autoregressive (AR) Models for Multiple Groups

This function fits separate autoregressive (AR) models of a specified
order (\`p\`) for multiple groups or categories within a dataset. It
processes raw observation data, calculates daily averages for each
group, fits an AR(p) model via ordinary least squares (\`lm\`), and
returns the in-sample fitted values in a wide-format matrix.

## Usage

``` r
fit_ar_models(
  unique_dates,
  unique_groups,
  obs_dates,
  obs_groups,
  obs_values,
  lag_p,
  empty_catch = NA
)
```

## Arguments

- unique_dates:

  A \`Date\` vector containing all unique, sorted dates that will form
  the rows of the output matrix.

- unique_groups:

  A \`character\` vector containing all unique, sorted group names that
  will form the columns of the output matrix.

- obs_dates:

  A \`Date\` vector of the same length as \`obs_values\`, indicating the
  date of each observation.

- obs_groups:

  A \`character\` vector of the same length as \`obs_values\`,
  indicating the group of each observation.

- obs_values:

  A \`numeric\` vector containing the observation values.

- lag_p:

  An \`integer\` specifying the order of the AR model (i.e., the number
  of lags to use as predictors).

## Value

A \`matrix\` with dates as rownames and groups as colnames. Each cell
contains the in-sample fitted value from the AR(p) model for that group
and date.

## Details

The function first aggregates the raw \`obs_values\` by calculating the
mean for each unique \`obs_groups\` and \`obs_dates\` combination. It
then iterates through each group, creates lagged predictor variables,
and fits a linear model (\`value ~ lag1 + lag2 + ...\`). The output
matrix is dense, containing a row for every date in \`unique_dates\` and
a column for every group in \`unique_groups\`. Cells for which a fitted
value could not be computed (e.g., at the start of a series) will
contain \`NA\`.
