# Calculate grouped percentages

Calculate grouped percentages

## Usage

``` r
perc_of_total(
  dat,
  value_var,
  group = NULL,
  drop = FALSE,
  val_type = "perc",
  output = "dataset"
)
```

## Arguments

- dat:

  Data table to summarize.

- value_var:

  String, variable name(s) for calculating total.

- group:

  String, grouping variable(s) to group \`value_var\` by.

- drop:

  Logical, whether to drop the total column.

- val_type:

  String, whether to convert value output to percentage `"perc"` or
  proportion `"prop"`.

- output:

  String, whether to add new variables to dataset (`"dataset"`) or
  return a summary table (`"summary"`)
