# Create single binary fishery season identifier variable

Create single binary fishery season identifier variable

## Usage

``` r
seasonalID(
  dat,
  project,
  seasonal.dat = NULL,
  start,
  end,
  overlap = FALSE,
  name = NULL
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

- seasonal.dat:

  Data table containing date of fishery season(s). Data table can be
  pulled from the FishSET database. Leave `seasonal.dat` as NULL if
  supplying start and end dates with `start` and `end` arguments.

- start:

  Date, supplied as a string (example: start='2011/04/22',
  start='04222011'), or variable in `seasonal.dat` which identifies
  start date of fishery season

- end:

  DDate, supplied as a string (example: start='2011/04/22',
  start='04222011'), or variable in `seasonal.dat` which identifies end
  date of fishery season

- overlap:

  Logical. Should trip or haul dates that start before or end after the
  fishery season date but starts or ends within the fishery season dates
  be included? FALSE indicates to inlude only hauls/trips that fall
  completely within the bounds of a fishery season date. Defaults to
  FALSE.

- name:

  String Seasonal identifier name

## Value

Returns a binary variable of within (1) or outside (0) the fishery
season.

## Details

Uses a supplied dates or a table of fishery season dates to create
fishery season identifier variables. Output is a binary variable called
`name` or \`SeasonID\` if `name` is not supplied.  
  
For each row `dat`, the function matches fishery season dates provided
in `seasonal.dat` to the earliest date variable in `dat`.

## Examples

``` r
if (FALSE) { # \dontrun{
#Example using a table stored in the FishSET database
pcodMainDataTable <- season_ID("pcodMainDataTable", 'pcod', seasonal_dat='seasonTable', 
     start='SeasonStart', end='SeasonEnd', name='2001A')
#Example using manually entered dates
pcodMainDataTable <- season_ID("pcodMainDataTable", 'pcod', seasonal.dat=NULL, 
    start='04152011', end='06302011', name='2001A')
} # }
```
