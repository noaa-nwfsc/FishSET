# Return the confidentiality settings

This function returns the confidentiality settings from project settings
file.

## Usage

``` r
get_confid_check(project)
```

## Arguments

- project:

  Name of project

## Value

A list containing the confidentiality parameters: `check`, `v_id`,
`rule`, and `value`.

## See also

[`set_confid_check`](set_confid_check.md)
[`get_proj_settings`](get_proj_settings.md)
