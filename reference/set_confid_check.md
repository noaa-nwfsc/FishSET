# Set confidentiality parameters

This function specifics whether to check for confidentiality and which
rule should be applied.

## Usage

``` r
set_confid_check(project, check = TRUE, v_id = NULL, rule = "n", value = NULL)
```

## Arguments

- project:

  Name of project.

- check:

  Logical, whether to check for confidentiality.

- v_id:

  String, the column name containing the vessel identifier.

- rule:

  String, the confidentiality rule to apply. See "Details" below.
  `rule = "n"` suppresses values containing fewer than n vessels.
  `rule = "k"` suppresses values where a single vessel contains k
  percent or more of the total catch.

- value:

  The threshold for confidentiality. for `rule = "n"` must be an integer
  of at least 2. For `rule = "k"` any numeric value from 0 to 100.

## Details

`rule = "n"` counts the number of vessel in each strata and suppresses
values where fewer than n vessels are present. For `rule = "k"`, or the
"Majority allocation rule", each vessel's share of catch is calculated
by strata. If any vessel's total catch share is greater than or equal to
k percent the value is suppressed.

## Examples

``` r
if (FALSE) { # \dontrun{
set_confid_check("pollock", check = TRUE, v_id = "PERMIT", rule = "n", value = 3L)
} # }
```
