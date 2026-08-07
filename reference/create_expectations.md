# Create expected catch/expected revenue matrix

Create expected catch or expected revenue matrix. The matrix is required
for the conditional logit model. Multiple matrices (with unique names)
can be saved in a project.

## Usage

``` r
create_expectations(
  dat,
  project,
  name,
  alt_name,
  catch,
  price = NULL,
  defineGroup = NULL,
  temp_var = NULL,
  temporal = "daily",
  calc_method = "standardAverage",
  temp_window = 7,
  day_lag = 1,
  year_lag = 0,
  empty_catch = NULL,
  empty_expectation = 1e-04,
  dummy_exp = FALSE,
  weight_avg = FALSE,
  outsample = FALSE
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- name:

  Name of the expected matrix to be saved

- alt_name:

  Name of the alternative choice matrix.

- catch:

  Variable from `dat` containing catch data.

- price:

  Optional, variable from `dat` containing price/value data. Price is
  multiplied against `catch` to generated revenue. If revenue exists in
  `dat` and you wish to use this revenue instead of price, then `catch`
  must be a vector of 1 of length equal to `dat`. Defaults to `NULL`.

- defineGroup:

  Optional, variable from `dat` that defines how to split the fleet.
  Defaults to treating entire dataframe `dat` as a fleet.

- temp_var:

  Optional, temporal variable from `dat`. Set to `NULL` if temporal
  patterns in catch should not be considered.

- temporal:

  String, choices are `"daily"` or `"sequential"`. Should time, if
  `temp_var` is defined, be included as a daily timeline or sequential
  order of recorded dates. For daily, catch on dates with no record are
  filled with `NA`. The choice affects how the rolling average is
  calculated. If temporal is daily then the window size for average and
  the temporal lag are in days. If sequential, then averaging will occur
  over the specified number of observations, regardless of how many days
  they represent.

- calc_method:

  String, how catch values are average over window size. Select standard
  average (`"standardAverage"`), simple lag regression (autoregressive)
  of catch (`"simpleLag"`), or weights of regressed groups (`"weights"`)

- temp_window:

  Numeric, temporal window size. If `temp_var` is not `NULL`, set the
  window size to average catch over. Defaults to 14 (14 days if
  `temporal` is `"daily"`).

- day_lag:

  Numeric, temporal lag time. If `temp_var` is not `NULL`, how far back
  to lag `temp_window`.

- year_lag:

  If expected catch should be based on catch from previous year(s), set
  `year_lag` to the number of years to go back.

- empty_catch:

  String, replace empty catch with `NA`, `0`, mean of all catch
  (`"allCatch"`), or mean of grouped catch (`"groupCatch"`).

- empty_expectation:

  Numeric, how to treat empty expectation values. Choices are to not
  replace (`NULL`) or replace with 0.0001 or 0.

- dummy_exp:

  Logical, should a dummy variable be created? If `TRUE`, output dummy
  variable for originally missing value. If `FALSE`, no dummy variable
  is outputted. Defaults to `FALSE`.

- weight_avg:

  Logical, if `TRUE` then all observations for a given zone on a given
  date will be included when calculating the mean, thus giving more
  weight to days with more observations in a given zone. If `FALSE`,
  then the daily mean for a zone will be calculated prior to calculating
  the mean across the time window.

- outsample:

  Logical, if `TRUE` then generate expected catch matrix for
  out-of-sample data. If `FALSE` generate for primary data table.
  Defaults to `outsample = FALSE`

## Value

Function saves a list of expected catch matrices to the FishSET database
as `projectExpectedCatch`. The list includes the expected catch matrix
from the user-defined choices. Multiple expected catch cases can be
added to the list by specifying unique names. The list is automatically
saved to the FishSET database and is called in
[`format_model_data`](format_model_data.md). The expected catch output
does not need to be loaded when defining or running the model.

## Details

Function creates an expectation of catch or revenue for alternative
fishing zones (zones where they could have fished but did not). The
output is saved to the FishSET database and called by the
[`format_model_data`](format_model_data.md) function.
[`create_alternative_choice`](create_alternative_choice.md) must be
called first as observed catch and zone inclusion requirements are
defined there.  
The primary choices are whether to treat data as a fleet or to group the
data (`defineGroup`) and the time frame of catch data for calculating
expected catch. Catch is averaged along a daily or sequential timeline
(`temporal`) using a rolling average. `temp_window` and `day_lag`
determine the window size and temporal lag of the window for averaging.
Use [`temp_obs_table`](temp_obs_table.md) before using this function to
assess the availability of data for the desired temporal moving window
size. Sparse data is not suited for shorter moving window sizes. For
very sparse data, consider setting `temp_var` to `NULL` and excluding
temporal patterns in catch.  
Empty catch values are considered to be times of no fishing activity.
Values of 0 in the catch variable are considered times when fishing
activity occurred but with no catch. These points are included in the
averaging and dummy creation as points in time when fishing occurred.  

## Examples

``` r
if (FALSE) { # \dontrun{
create_expectations(pollockMainDataTable, "pollock", "exp1", "OFFICIAL_TOTAL_CATCH_MT",
  price = NULL, defineGroup = "fleet", temp_var = "DATE_FISHING_BEGAN",
  temporal = "daily", calc_method = "standardAverage", 
  empty_catch = "allCatch", empty_expectation = 0.0001, temp_window = 4,
  day_lag = 2, year_lag = 0, dummy_exp = FALSE, 
  weight_avg = FALSE, outsample = FALSE
)
} # }
```
