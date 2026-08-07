# Create FishSET Model Design Object

Constructs the design object required for discrete choice model fitting
within the FishSET framework. This function parses the model formula,
validates the formatted data, and generates the design matrix (X) and
choice vector (y). It handles both alternative-specific variables and
trip- or haul-specific variables (automatically creating interactions
with zone constants). The resulting design object is the primary input
for the [`fishset_fit`](fishset_fit.md) function, which performs the
parameter estimation.

## Usage

``` r
fishset_design(
  formula,
  project,
  model_name,
  formatted_data_name,
  unique_obs_id,
  zone_id,
  catch_formula = NULL,
  price_var = NULL,
  scale = FALSE,
  overwrite = FALSE
)
```

## Arguments

- formula:

  A two-part formula specifying the model structure (e.g.,
  `chosen ~ expected_catch + distance | income`). The left-hand side
  specifies the binary choice variable, which is always specified as
  `chosen` from the [`format_model_data`](format_model_data.md)
  function. The right-hand side is separated by a pipe (\|): Part 1
  contains alternative-specific variables, and Part 2 contains trip- or
  haul-specific variables (i.e., do not vary across fishing zones).

- project:

  Name of the project.

- model_name:

  Name for this specific model design instance. Must be unique within
  the project's design list.

- formatted_data_name:

  Name of the formatted data object to use. This must correspond to a
  name previously created by
  [`format_model_data`](format_model_data.md).

- unique_obs_id:

  Variable name in the dataset representing the unique observation
  identifier.

- zone_id:

  Variable name in the dataset representing the zone (alternative)
  identifier.

- catch_formula:

  A formula specifying the expected catch for an Expected Profit Model.

- price_var:

  Variable name in the dataset representing price. This input is only
  used for Expected Profit Models, and the price variable must be
  included in the formatted dataset created in the
  [`format_model_data`](format_model_data.md) function.

- scale:

  Logical. Default = FALSE. If TRUE, numeric predictors in the design
  matrix (X) are centered and scaled (z-score normalization) before
  saving. Scaling factors are stored to allow unscaling of parameters
  after estimation. Recommended for numerical stability.

- overwrite:

  Logical. Default FALSE. If TRUE, overwrites an existing model design
  if `model_name` already exists in the project folder.

## Value

A list object of class 'fishset_design' containing the design matrices,
choice vector, and metadata. The list is saved as a compressed file in
the project folder.

## Details

The resulting design object is saved as a compressed file in the
'Models/ModelDesigns' folder, which is located inside the project
folder.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Standard Conditional Logit
# "chosen" is ALWAYS the response, "expected_catch" and "distance" are site attributes.
fishset_design(
  formula = chosen ~ expected_catch + distance,
  project = "MyProject",
  model_name = "clogit_model1",
  formatted_data_name = "my_formatted_data",
  unique_obs_id = "haul_id",
  zone_id = "zone_id"
)

# 2. Zonal Logit with Alternative-Specific Constants (ASCs)
# Add the zone_id variable ("zone_id") to the formula to generate fixed effects.
fishset_design(
  formula = chosen ~ expected_catch + distance + zone_id,
  project = "MyProject",
  model_name = "zonal_logit_model1",
  formatted_data_name = "my_formatted_data",
  unique_obs_id = "haul_id",
  zone_id = "zone_id",
  scale = TRUE
) 

# 3. Zonal Logit with trip- or haul-specific variables
# Add the variable that does not vary across zones (e.g., vessel length) after the pipe ("|").
fishset_design(
  formula = chosen ~ expected_catch + distance | vessel_length,
  project = "MyProject",
  model_name = "zonal_logit_model2",
  formatted_data_name = "my_formatted_data",
  unique_obs_id = "haul_id",
  zone_id = "zone_id",
  scale = TRUE
)

# 4. Expected profit model - normal distribution
# The expected catch (catch_var in this example) does not vary across zones in this 
# example (e.g., vessel length).
fishset_design(
  formula = chosen ~ distance | catch_var,
  project = "MyProject",
  model_name = "epm1",
  formatted_data_name = "my_formatted_data",
  unique_obs_id = "haul_id",
  zone_id = "ZoneID",
  catch_formula = actual_catch ~ catch_var:ZoneID,
  price_var = "price_var",
  scale = TRUE
) 
} # }
```
