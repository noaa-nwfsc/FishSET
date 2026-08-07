# Check Spatial Autocorrelation of Model Residuals

Calculates zone-level residuals from a fitted FishSET model, tests for
spatial autocorrelation using Moran's I, and generates a spatial plot of
the residuals.

## Usage

``` r
model_resid_corr(
  project,
  model_name,
  spat,
  spat_id,
  fit_name = NULL,
  distribution = NULL
)
```

## Arguments

- project:

  Character string. Name of the project.

- model_name:

  Character string. Name of the specific model design used.

- spat:

  Character string of spatial table name in project database OR `sf`
  polygon object containing the spatial boundaries of the fishing zones.

- spat_id:

  Character string. The name of the column in `spat` that matches the
  zone identifiers used in the model design.

- fit_name:

  Character string (Optional). Name of the model fit object. Defaults to
  `paste0(model_name, "_fit")`.

- distribution:

  Character string (Optional). Distribution for the continuous catch
  component in EPMs. Required if evaluating an EPM.

## Value

A list of class `"fishset_spatial_resid"` containing:

- moran_test:

  The results of the Moran's I test from `spdep`.

- residual_map:

  A `ggplot2` object mapping the spatial residuals.

- zonal_residuals:

  A dataframe of the calculated mean residuals per zone.

- spatial_data:

  The merged `sf` object containing geometries and residuals.
