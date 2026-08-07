# Create catch or revenue per unit effort variable

Add catch per unit effort (CPUE) or revenue per unit effort variable to
the primary dataset. Catch should be a weight variable but can be a
count. Effort should be in a duration of time, such as days, hours, or
minutes.

## Usage

``` r
cpue(dat, project, xWeight = NULL, xTime, price = NULL, name = NULL)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

- xWeight:

  Catch variable in `dat`. Variable should be a measure of weight
  (pounds, metric tons, etc) but can also be count. If calculating
  revenue per unit effort (RPUE) and a revenue column exists in `dat`,
  then add the revenue column to `price` and set `xWeight = NULL`.

- xTime:

  Duration of time variable in `dat` representing effort, such as weeks,
  days, hours, or minutes.

- price:

  Optional, variable from `dat` containing price/value data. Price is
  multiplied against the catch variable, `xWeight`, to generated
  revenue. If revenue exists in `dat` and you wish to use this revenue
  instead of price, then `xWeight` must be `NULL`. Defaults to `NULL`.

- name:

  String, name of created variable. Defaults to "cpue" or "rpue" if
  `price` is not `NULL`.

## Value

Returns primary dataset with CPUE variable added.

## Details

Creates the catch or revenue per unit effort variable. Catch variable
should be in weight (lbs, mts). Effort variable should be a measurement
of duration in time. New variable is added to the primary dataset with
the column name defined by the `name` argument. CPUE for individual
species should be calculated separately.

## Examples

``` r
if (FALSE) { # \dontrun{
pollockMainDataTable <- cpue(pollockMainDataTable, 'pollock', 
                             xWeight = 'OFFICIAL_TOTAL_CATCH_MT', 
                             xTime = 'DURATION_IN_MIN', name = 'cpue')
} # }
```
