# Suppress confidential values in summary table

This function suppresses values in a summary table based on suppression
conditions found in the check table (see `link{check_confidentiality}`)

## Usage

``` r
suppress_table(
  check,
  output,
  value_var,
  group,
  rule,
  type = "code",
  as_vector = FALSE
)
```

## Arguments

- check:

  The check table containing suppression conditions.

- output:

  The summary table to be edited based on check table.

- value_var:

  String, value variable name(s).

- group:

  String, grouping variable name(s). This includes \`period\` and
  \`facet_by\` from summary function.

- rule:

  String, the confidentiality rule to apply. `rule = "n"` suppresses
  values containing fewer than n vessels. `rule = "k"` (the "majority
  allocation rule") suppresses values where a single vessel contains k
  percent or more of the total catch.

- type:

  String, the value used to replace confidential data. `"code"` replaces
  values with `-999`, `"NA"` (with quotes) replaces with `NA`, and
  `"zero"` replaces with 0.

- as_vector:

  Logical, whether to return the suppressed values as a vector. If
  `as_vector == FALSE` the output table is returned.
