# Check and suppress roll_catch output

Check and suppress roll_catch output

## Usage

``` r
check_conf_rc(dat, roll_tab, project, catch, date, group, k, full_dates, align)
```

## Arguments

- dat:

  Dataset used to create `roll_tab` dataframe.

- roll_tab:

  Unsuppressed table from `roll_catch`.

- project:

  Name of project.

- catch:

  String, name of catch variable(s).

- date:

  String, name of date variable.

- group:

  String, name of group variable(s).

- k:

  Integer, width of window.

- full_dates:

  Vector of full dates.

- align:

  String, align argument for `rollapply()`.
