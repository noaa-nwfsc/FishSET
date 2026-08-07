# Summarize and Plot Policy Welfare Impacts

Extracts and visualizes the expected economic welfare changes across all
simulated policy scenarios, including statistical uncertainty.

## Usage

``` r
summarize_policy_welfare(project, plot_scenarios = NULL, plot_models = NULL)
```

## Arguments

- project:

  Character. Name of the project.

- plot_scenarios:

  Character vector (optional). Specific Scenario or Simulation names to
  include in the plots. If `NULL` (the default), scenarios are not
  filtered.

- plot_models:

  Character vector (optional). Specific Model names to include in the
  plots (e.g., "zonal_logit", "clogit"). If `NULL` (the default), models
  are not filtered.

## Value

A list containing three elements: `summary_data` (a data frame of
welfare impacts), `plot_bar` (mean per-trip changes), and `plot_density`
(distribution of uncertainty).

## Details

**What does "Welfare Change" represent?**

In this simulation framework, "welfare change" represents the
Compensating Variation (CV) resulting from a policy shock, expressed in
real-world monetary units (e.g., dollars).

It is calculated using the log-sum difference formula from random
utility theory. This metric captures not just the direct penalty of a
closure or a drop in catch, but also the mitigating effect of spatial
substitution—how fishers adapt by reallocating their effort to the
next-best available fishing grounds.
