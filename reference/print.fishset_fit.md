# Print FishSET Model Fit Results

Formats and prints the output of a FishSET discrete choice model fit.
Displays the model formula (if available), coefficients table with
significance stars, and key goodness-of-fit statistics (Log-Likelihood,
AIC, BIC, Pseudo-R2, Accuracy).

## Usage

``` r
# S3 method for class 'fishset_fit'
print(x, digits = 4, ...)
```

## Arguments

- x:

  A `fishset_fit` object returned by [`fishset_fit`](fishset_fit.md).

- digits:

  Integer. The number of significant digits to use when printing numeric
  values. Default is 4.

- ...:

  Additional arguments passed to
  [`printCoefmat`](https://rdrr.io/r/stats/printCoefmat.html).
