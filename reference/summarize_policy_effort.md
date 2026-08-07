# Summarize and Plot Policy Effort Redistribution

Extracts and visualizes the spatial redistribution of fishing effort.

## Usage

``` r
summarize_policy_effort(
  project,
  spat,
  zone_spat,
  output_type = "static",
  plot_scenarios = NULL,
  plot_models = NULL,
  plotly_source = "effort_scatter"
)
```

## Arguments

- project:

  Character. Name of the project.

- spat:

  Character. Name of the spatial dataset containing the fishing zones.

- zone_spat:

  Character. The ID column in the spatial data matching simulation zone
  IDs.

- output_type:

  Character. Dictates the rendering engine: "static" (ggplot2) for
  standard R plots, or "dynamic" (leaflet/plotly) for interactive HTML
  widgets. Default is "static".

- plot_scenarios:

  Character vector (optional). Filters plots for specific scenarios
  using partial string matching (grep).

- plot_models:

  Character vector (optional). Filters plots for specific models using
  exact matching.

- plotly_source:

  Character. A unique identifier string utilized by the `crosstalk`
  package to enable cross-widget interactivity (e.g., capturing a click
  on the scatter plot to highlight a zone on the map). The default is
  `"effort_scatter"`. Note: There is no strict menu of alternative
  options; rather, you can provide **any custom string** here to match
  the namespace or `SharedData` group ID defined within your specific
  Shiny app or RMarkdown document.

## Value

A named list containing four main elements:

- `summary_data`: A data frame of all raw metrics for the requested
  simulations.

- `plots_absolute_map`: A list of map objects showing raw net change in
  effort.

- `plots_percent_map`: A list of map objects showing percentage change.

- `plots_scatter`: A list of 1:1 scatter plot objects comparing baseline
  vs. counterfactual effort.

## Details

**What does "Effort" represent?**

In this simulation framework, "effort" represents the expected number of
fishing choice occasions (e.g., trips or hauls) allocated to each
spatial zone.
