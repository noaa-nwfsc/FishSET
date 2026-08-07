# Evaluate sparsity in data over time in table format

Create table of data sparsity by predefined time periods.

## Usage

``` r
sparsetable(dat, project, timevar, zonevar, var)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- timevar:

  Variable in `dat` containing temporal data

- zonevar:

  Variable in `dat` containing zone observation assigned to

- var:

  Variable in `dat` containing catch data
