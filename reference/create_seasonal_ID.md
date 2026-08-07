# Create fishery season identifier variable

Create fishery season identifier variable

## Usage

``` r
create_seasonal_ID(
  dat,
  project,
  seasonal.dat,
  use.location = c(TRUE, FALSE),
  use.geartype = c(TRUE, FALSE),
  sp.col,
  target = NULL
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

- seasonal.dat:

  Table containing date of fishery season(s). Can be pulled from the
  FishSET database.

- use.location:

  Logical, should fishery season dates depend on fishery location?
  Column names containing location in `dat` and `seasonal.dat` must
  match.

- use.geartype:

  Logical, should fishery season dates depend on gear type. Column names
  containing gear type in `dat` and `seasonal.dat` must match.

- sp.col:

  Variable in `seasonal.dat` containing species names.

- target:

  Name of target species. If `target` is NULL, runs through fisheries in
  order listed in `seasonal.dat`

## Value

Returns the primary dataset with the variable SeasonID, or a series of
variables identifying by the individual fisheries included
(seasonID\*fishery).

## Details

Uses a table of fishery season dates to create fishery season identifier
variables. Output is a SeasonID variable and/or multiple
SeasonID\*fishery variables. If fishery season dates vary by location or
gear type, then `use.location` and `use.geartype` should be TRUE.  
  
The function matches fishery season dates provided in `seasonal.dat` to
the earliest date variable in `dat`. The \`seasonID\` variable is a
vector of fishery seasons whereas the \`SeasonID\*fishery\` variables
are 1/0 depending on whether the fishery was open on the observed
date.  
  
If `target` is not defined, then each row of seasonID is defined as the
earliest fishery listed in `seasonal.dat` for which the fishery season
date encompasses the date variable in the primary dataset. If `target`
fishery is defined, then \`SeasonID\` is defined by whether the target
fishery is open on the date in the primary dataset or a different
fishery. The vector is filled with 'target' or 'other'.  
  
\`SeasonID\*fishery\` variables are a 1/0 seasonID vector for each
fishery (labeled by seasonID and fishery) where 1 indicates the dates
for a given row in the primary data table fall within the fishery dates
for that fishery.

## Examples

``` r
if (FALSE) { # \dontrun{
pcodMainDataTable <- create_seasonal_ID("pcodMainDataTable", seasonal_dat,
  use.location = TRUE, use.geartype = TRUE, sp.col = "SPECIES", target = "POLLOCK"
)
} # }
```
