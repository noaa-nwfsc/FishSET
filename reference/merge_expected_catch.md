# Merge expected catch

Merge expected catch matrices to the primary dataset.

## Usage

``` r
merge_expected_catch(
  dat,
  project,
  zoneID,
  date,
  exp.name,
  new.name = NULL,
  ec.table = NULL,
  log_fun = TRUE
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- zoneID:

  zone ID Variable in `dat` that identifies the individual zones or
  areas.

- date:

  Date variable used to create the expected catch matrix.

- exp.name:

  Name(s) of expected catch matrix to merge into `dat`.

- new.name:

  Optional, new name for `exp.name`. These should be in the same order
  as `exp.name`.

- ec.table:

  Optional, the name of a specific expected catch table to use. Defaults
  to `projectnameExpectedCatch`.

- log_fun:

  For internal use. Whether to log the function call.

## Value

Merges an expected catch matrix created using
[`create_expectations()`](create_expectations.md) to the primary
dataset, `dat`.
