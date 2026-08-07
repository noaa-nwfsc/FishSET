# Format Model Data

Reshapes the project datasets into a single long format suitable for
discrete choice modeling using RTMB. This function integrates various
data sources-including distance matrices, expectation (catch and/or
revenue) matrices, auxiliary data, and gridded environmental data- and
performs optional missing data imputation.

## Usage

``` r
format_model_data(
  project,
  name,
  alt_name,
  zone_id,
  unique_obs_id,
  select_vars = NULL,
  aux_data = NULL,
  aux_key = NULL,
  gridded_data = NULL,
  expectations = NULL,
  distance = TRUE,
  distance_units = NULL,
  impute = NULL,
  crs = NULL,
  overwrite = FALSE
)
```

## Arguments

- project:

  Name of the project.

- name:

  Name for this specific formatted model data instance. Must be unique
  within the project's formatted data list.

- alt_name:

  Name of the alternative choice matrix.

- zone_id:

  Variable name in the dataset representing the zone identifier.

- unique_obs_id:

  Variable name in the dataset representing the unique observation
  identifier (unique rows in the main data table).

- select_vars:

  Character vector of variable names to retain from the main data table.
  Although this input is optional, it is recommended to limit the final
  format to necessary variables for computational efficiency. IMPORTANT
  NOTE: if modeling multi-haul data, be sure to include the lagged zone
  ID (previous location) in this vector.

  \*IMPORTANT NOTE\*: for expected profit models, the price and actual
  catch variables must be included here.

- aux_data:

  Name of the auxiliary data table to join. Use
  [`list_tables`](list_tables.md) function to view the table name.

- aux_key:

  Variable name used to join the main data table with the auxiliary
  data.

- gridded_data:

  Name of the gridded data table to join. Use
  [`list_tables`](list_tables.md) function to view the table name.

- expectations:

  Character vector containing the names of expected catch or revenue
  matrices to merge into the dataset.

- distance:

  Logical. If 'TRUE', calculates and merges a distance matrix between
  observations and zones. Defaults to 'TRUE'.

- distance_units:

  String representing the units of measurement for distance ("km" or
  "mi").

- impute:

  Method for imputing missing values (NAs). Options are \`"mean"\`,
  \`"median"\`, \`"mode"\`, or \`"remove"\`. \`"remove"\` will
  completely remove zones from the dataset that contain any NAs in
  corresponding data. If NULL, the function stops if NAs are detected.

- crs:

  Coordinate reference system. Only used if 'distance = TRUE' and
  spatial calculations are required.

- overwrite:

  Logical. Defaults to FALSE. If TRUE, overwrites an existing formatted
  dataset with the same name.

## Value

A list containing the formatted data frame and the input settings. The
list is saved to the project database.

## Details

The resulting formatted data is serialized and stored in the FishSET
project database within a table named '\[project_name\]LongFormatData'.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Basic usage: Formatting data with simple mean imputation for missing values
  format_model_data(
    project = "NewEnglandCod",
    name = "ModelData_Run1",
    alt_name = "altname_1",
    zone_id = "zone_code",
    unique_obs_id = "trip_id",
    select_vars = c("vessel_length", "month", "permit_type"),
    impute = "mean"
  )

  # Advanced usage: Including distance calculations, auxiliary economic data, 
  # and revenue expectations
  format_model_data(
    project = "WestCoastGroundfish",
    name = "ModelData_Spatial",
    alt_name = "altname_1",
    zone_id = "grid_id",
    unique_obs_id = "haul_id",
    select_vars = c("vessel_len", "month", "fuel_price"),
    aux_data = "FuelCostIndex",
    aux_key = "year",
    expectations = c("ExpectedRevenue_2023"),
    distance = TRUE,
    crs = 4326,
    impute = "remove"
  )
} # }
```
