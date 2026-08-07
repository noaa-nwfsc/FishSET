# Check and suppress data

Check and suppress data

## Usage

``` r
check_and_suppress(
  dat,
  output,
  project,
  v_id,
  value_var,
  group = NULL,
  rule,
  value,
  type = "code",
  names_to = "name",
  values_to = "value"
)
```

## Arguments

- dat:

  The dataset used to create a summary table. This must include the
  vessel identifier column.

- output:

  The output table to be suppressed. If `output = NULL`, `dat` is used.
  @param project Name of project.

- v_id:

  String, the name of the vessel identifier column.

- value_var:

  String, the name(s) of the value variable(s).

- group:

  String, the name(s) of the grouping variable(s). This should include
  the \`period\` name if summarizing over time.

- rule:

  String, the confidentiality rule to apply. `rule = "n"` suppresses
  values containing fewer than n vessels. `rule = "k"` (the "majority
  allocation rule") suppresses values where a single vessel contains k
  percent or more of the total catch.

- value:

  The threshold for confidentiality. for `rule = "n"` must be an integer
  of at least 3. For `rule = "k"` any double value from 0 to 100.

- type:

  String, the value used to replace confidential data. `"code"` replaces
  values with `-999`, `"NA"` (with quotes) replaces with `NA`, and
  `"zero"` replaces with 0.

- names_to:

  String, the name for the column containing the names of value
  variables when \`value_var\` has two or more columns.

- values_to:

  String, the name for the column containing the values from the
  variables listed in \`names_to\`.
