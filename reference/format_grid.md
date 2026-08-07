# Format Gridded Data

Change the format of a gridded dataset from wide to long (or vice versa)
and remove any unmatched area/zones from `grid`. This is a necessary
step for including gridded variables in the conditional logit model.

## Usage

``` r
format_grid(
  grid,
  dat,
  project,
  dat.key,
  area.dat,
  area.grid = NULL,
  id.cols,
  from.format = "wide",
  to.format = "wide",
  val.name = NULL,
  save = FALSE
)
```

## Arguments

- grid:

  Gridded dataset to format.

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  Name of project.

- dat.key:

  String, name of column(s) in MainDataTable to join by. The number of
  columns must match `id.cols`.

- area.dat:

  String, the name of the area or zone column in `dat`.

- area.grid:

  String, the name of the area or zone column in `dat` if
  `from.format = "long"`. Ignored if `from.format = "wide"`.

- id.cols:

  String, the names of columns from `grid` that are neither area
  (`area.grid`) or value (`val.name`) columns, for example date or
  period column(s).

- from.format:

  The original format of `grid`. Options include `"long"` or `"wide"`.
  Use `"long"` if a single area column exists in `grid`. Use `"wide"` if
  `grid` contains a column for each area.

- to.format:

  The desired format of `grid`. Options include `"long"` or `"wide"`.
  Use `"long"` if you want a single area column with a corresponding
  value column. Use `"wide"` if you would like each area to have its own
  column.

- val.name:

  Required if converting from wide to long or long to wide format. When
  `from.format = "wide"` and `to.format = "long"`, `val.name` will be
  the name of the new value variable associated with the area column.
  When `from.format = "long"` and `to.format = "wide"`, `val.name` will
  be the name of the existing value variable associated with the area
  column.

- save:

  Logical, whether to save formatted `grid`. When `TRUE`, the table will
  be saved with the string `"Wide"` or `"Long"` appended depending on
  the value of `to.format`.

## See also

[`merge_dat()`](merge_dat.md)
