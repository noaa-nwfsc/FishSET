# Replace suppression code

This function replaces the default suppression code in a table.

## Usage

``` r
replace_sup_code(output, code = NA)
```

## Arguments

- output:

  Table containing suppressed values.

- code:

  The replacement suppression code. `code = NA` by default; this is
  ideal for plotting as ggplot automatically removes NAs.

## Details

Suppressed values are represented as \`-999\` by default. This isn't
ideal for plotting. NAs – the default in \`replace_sup_code()\`– are a
better alternative for plots as they can easily be removed.

## Examples

``` r
if (FALSE) { # \dontrun{
summary_tab <- replace_sup_code(summary_tab, code = NA)
} # }
```
