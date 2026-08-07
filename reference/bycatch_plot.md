# Bycatch plot helper

Creates and formats plots for `bycatch`.

## Usage

``` r
bycatch_plot(
  dat,
  cpue,
  catch,
  period,
  group,
  facet_by,
  names,
  value,
  scale,
  conv,
  tran,
  format_lab
)
```

## Arguments

- dat:

  Data used to create plot.

- cpue:

  String, cpue variable(s) passed from `bycatch`.

- catch:

  String, catch variable(s) passed from `bycatch`.

- period:

  String, period passed from `bycatch`.

- group:

  String, grouping variable(s) passed from `bycatch`.

- facet_by:

  String, facet variable(s) passed from `bycatch`.

- names:

  String, species names for plot labels passed from `bycatch`.

- value:

  String, whether to return percent or sum of catch.

- scale:

  String, facet scale passed from `bycatch`.

- conv:

  Convert catch variable to `"tons"`, `"metric_tons"`, or by using a
  function entered as a string. Defaults to `"none"` for no conversion.

- tran:

  String, scale transformation passed from `bycatch`.

- format_lab:

  Formatting option for y-axis labels. Options include `"decimal"` or
  `"scientific"`.
