# Define zone closure scenarios

Define zone closure scenarios

## Usage

``` r
zone_closure(
  project,
  spatname,
  zone_spat,
  lon_spat = NULL,
  lat_spat = NULL,
  epsg = NULL
)
```

## Arguments

- project:

  Required, name of project.

- spatname:

  Required, data file or character. `spat` is a spatial data file
  containing information on fishery management or regulatory zone
  boundaries. Shape, json, geojson, and csv formats are supported.
  geojson is the preferred format. json files must be converted into
  geojson. This is done automatically when the file is loaded with
  [`read_dat`](read_dat.md) with `is.map` set to true. `spat` cannot, at
  this time, be loaded from the FishSET database.  

- zone_spat:

  Variable in `spat` that identifies the individual areas or zones.

- lon_spat:

  Required for csv files. Variable or list from `spat` containing
  longitude data. Leave as NULL if `spat` is a shape or json file.

- lat_spat:

  Required for csv files. Variable or list from `spat` containing
  latitude data. Leave as NULL if `spat` is a shape or json file.

- epsg:

  EPSG number. Set the epsg to ensure that `spat` has the correct
  projections. If epsg is not specified but is defined for `spat`. See
  <http://spatialreference.org/> to help identify the optimal epsg
  number.

## Value

Returns a yaml file to the project output folder.

## Details

Define zone closure scenarios via an interactive app. Users can define
scenarios by clicking on one or more zones on the map, adjusting the
allowable TAC percentages in the table, and entering a unique scenario
name. Clicking 'Add closure' instantly saves the scenario to the project
database. These saved choices are later called in the policy scenario
function.
