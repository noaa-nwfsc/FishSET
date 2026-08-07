# Print Hausman-McFadden IIA Test Results

Formats and prints the output of a Hausman-McFadden test for the
Independence of Irrelevant Alternatives (IIA) assumption within the
FishSET framework. Displays the omitted alternatives, the Chi-squared
test statistic, degrees of freedom, the calculated p-value, and a clear
text interpretation of the result (including custom diagnostic
interpretations for Expected Profit Models).

## Usage

``` r
# S3 method for class 'fishset_iia'
print(x, ...)
```

## Arguments

- x:

  A `fishset_iia` object returned by
  [`fishset_iia_test`](fishset_iia_test.md).

- ...:

  Additional arguments passed to other methods (currently ignored).
