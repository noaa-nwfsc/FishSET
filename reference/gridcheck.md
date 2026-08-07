# Check that spatial data is a sf object. Convert if not.

Check that spatial data is a sf object. Convert if not.

## Usage

``` r
gridcheck(
  spatialdat,
  catdat,
  londat = NULL,
  latdat = NULL,
  lon.grid = NULL,
  lat.grid = NULL
)
```

## Arguments

- spatialdat:

  The spatial dataframe

- catdat:

  Variable that names polygons

- londat:

  Longitude data from primary dataset

- latdat:

  Latitude data from primary dataset

- lon.grid:

  Variable in spatialdat containing longitude data

- lat.grid:

  Variable in spatialdat containing latitude data
