# Print FishSET Cross Validation Results

Formats and prints the output of a FishSET cross-validation run.
Displays the average out-of-sample performance, a detailed table of
metrics for each individual fold, and the estimated coefficients across
folds.

## Usage

``` r
# S3 method for class 'fishset_cv'
print(x, digits = 4, ...)
```

## Arguments

- x:

  A `fishset_cv` object returned by [`fishset_cv`](fishset_cv.md).

- digits:

  Integer. The number of significant digits to use when printing numeric
  values. Default is 4.

- ...:

  Additional arguments passed to the print method.
