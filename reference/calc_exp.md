# Calculate expected catch/revenue

Calculate expected catch/revenue

## Usage

``` r
calc_exp(
  dataset,
  alt_name = NULL,
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
  Alt
)
```

## Arguments

- dataset:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

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
  average (`"standardAverage"`), simple lag regression of means
  (`"simpleLag"`), or weights of regressed groups (`"weights"`)

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

- Alt:

  Alternative choice list loaded from the FishSET database.

## Value

Returns a list containing the expected catch/revenue matrix, dummy
matrix (if `dummy_exp = TRUE`), and list of input args.
