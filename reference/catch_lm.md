# Linear Model for Catch

First stage regression model for catch.

## Usage

``` r
catch_lm(
  dat,
  project,
  catch.formula,
  zoneID = NULL,
  exp.name = NULL,
  new.name = NULL,
  date,
  output = "matrix"
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- catch.formula:

  A formula object specifying the linear model.See
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html).

- zoneID:

  zone ID Variable in `dat` that identifies the individual zones or
  areas. Required if merging expected catch into `dat` using `exp.name`,
  or when creating a new expected catch matrix and `exp.name` is `NULL`
  (see `output` below).

- exp.name:

  Name(s) of expected catch matrix to merge into `dat`.

- new.name:

  Optional, string. When `output = 'matrix'`, `new.name` will become the
  name of the new expected catch matrix saved to the FishSET DB expected
  catch list. When `output = 'dataset'`, `new.name` will become the name
  of the new expected catch variable added to the primary dataset.

- date:

  Date variable from `dat` used to create expected catch matrix.

- output:

  Whether to output `dat` with the expected catch variable added
  (`'dataset'`) or to save an expected catch matrix to the expected
  catch FishSET DB table (`'matrix'`). Defaults to `output = 'matrix'`.

## Value

`catch_lm()` has two output options: `dataset` and `matrix`. When
`output == 'dataset'`, the primary dataset will be returned with the
fitted values from the model added as a new column. The new column is
named using `new.name`.

When `output == 'matrix'` an expected catch matrix is created and saved
to the FishSET DB expected catch list (it is not outputted to the
console). There are two ways to create an expected catch matrix: by
using an existing expected catch matrix in `catch.formula`, or by using
a zone-identifier column (i.e. `zoneID`) in the `catch.formula`. For
example, if you have created an expected catch matrix named 'user1'
using [`create_expectations()`](create_expectations.md), `catch.formula`
could equal `catch ~ vessel_length * user1`. In this case `exp.name`
would equal `'user1'`. Alternatively, you could create an expected catch
matrix by specifying `catch.formula` as `catch ~ vessel_length * zone`.
In this case, `exp.name = NULL` and `zoneID = 'zone'`.

## Details

`catch_lm()` can merge an expected catch matrix into the primary dataset
before running the linear model. This is done using by passing
`exp.name` and `zoneID` to
[`merge_expected_catch()`](merge_expected_catch.md) and is for
convenience; users can do this separately using
[`merge_expected_catch()`](merge_expected_catch.md) if desired, just
make sure to leave `exp.name` empty before running `catch_lm()`. Merging
expected catch in a separate step is useful for creating tables and
plots before running a first stage linear regression.

## See also

[`merge_expected_catch()`](merge_expected_catch.md)
