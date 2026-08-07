# Evaluate outliers through plots

Evaluate outliers through plots

## Usage

``` r
outlier_plot_int(
  dat,
  x,
  dat_remove = "none",
  x_dist = "normal",
  sd_val = NULL,
  plot_type
)
```

## Arguments

- dat:

  Primary data frame over which to apply function. Table in fishet_db
  database should contain the string \`MainDataTable\`.

- x:

  Column in dataframe to check for outliers.

- dat_remove:

  Defines method to subset the data. Choices include: 'none',
  '5_95_quant', '25_75_quant', 'mean_2SD', 'median_2SD', 'mean_3SD',
  'median_3SD'.

- x_dist:

  Distribution of the data. Choices include: 'normal', 'lognormal',
  'exponential', 'weibull', 'poisson', 'negative binomial'.

- sd_val:

  User-defined rule.

- plot_type:

  Which plot to return.

## Value

Plot of the data

## Details

The function returns three plots, the data, a probability plot, and a
Q-Q plot. The data plot is the value of `x` against row number. Red
points are all the data without any points removed. The blue points are
the subsetted data. If \`dat_remove\` is \`none\`, then only blue points
will be shown. The probability plot is a histogram of the data with the
fitted probability distribution based on \`x_dist\`. The Q-Q plot plots
are sampled quantiles against theoretical quantiles.
