# GUI for spatial data checks

Runs the spatial checks performed by [`spatial_qaqc`](spatial_qaqc.md)
in a shiny application.

## Usage

``` r
spat_qaqc_gui(dataset, project, spatdat, checks = NULL)
```

## Arguments

- dataset:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  Name of project.

- spatdat:

  Spatial data containing information on fishery management or
  regulatory zones. See [`read_dat`](read_dat.md) for details on
  importing spatial data.

- checks:

  (Optional) A list of spatial data quality checks outputted by
  `spatial_qaqc`.

## See also

[`spatial_qaqc`](spatial_qaqc.md)
