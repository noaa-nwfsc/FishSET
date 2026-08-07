# Define alternative fishing choice

Required step. Creates a list identifying how alternative fishing
choices should be defined. Output is saved to the FishSET database. Run
this function before running models. `dat` must have a zone assignment
column (see [`assignment_column()`](assignment_column.md)). In certain
cases a centroid table must be saved to the FishSET Database, see
`occasion_var` for details.

## Usage

``` r
create_alternative_choice(
  dat,
  project,
  alt_name = NULL,
  zoneID,
  occasion = "zonal centroid",
  occasion_var = NULL,
  alt_var = "zonal centroid",
  min_haul = 0,
  spatname = NULL,
  spatID = NULL,
  outsample = FALSE
)
```

## Arguments

- dat:

  Required, Primary data frame containing data on hauls or trips. Table
  in FishSET database should contain the string `MainDataTable`.

- project:

  Required, name of project.

- alt_name:

  String, **Required**. The name to be assigned to this alternative
  choice list within the FishSET database. If a list with this name
  already exists, the function will stop.

- zoneID:

  Variable in `dat` that identifies the individual zones or areas.

- occasion:

  String, determines the starting point when calculating the distance
  matrix. Options are `"zonal centroid"`, `"fishing centroid"`,
  `"port"`, or `"lon-lat"`. See `occasion_var` for requirements.

- occasion_var:

  Identifies an ID column or set of lon-lat variables needed to create
  the distance matrix. Possible options depend on the value of
  `occasion`:

  Centroid

  :   When `occasion = zonal centroid` the possible options are `NULL`,
      the name of a zone ID variable, or a set coordinate variables (in
      Lon-Lat order).

      NULL

      :   This will merge centroid lon-lat data to the primary table
          using the column enter in `zoneID`. A centroid table must be
          saved to the FishSET Database.

      Zone ID

      :   This option specifies the zone ID variable to merge the
          centroid table to. For example, a column containing the
          previous zonal area. A centroid table must be saved to the
          FishSET Database.

      Lon-Lat

      :   A string vector of length two containing the longitude and
          latitude of an existing set centroid variables in `dat`.

  Port

  :   When `occasion = port` the possible options include the name of a
      port ID variable or a set of lon-lat variables describing the
      location of the port. A value of `NULL` will return an error.

      Port ID

      :   The name of a port ID variable in `dat` that will be used to
          join the port table to the primary table. A port table is
          required (see [`load_port()`](load_port.md)) which contains
          the port name and the longitude and latitude of each port.

      Lon-Lat

      :   A string vector of length two containing a port's longitude
          and latitude in `dat`.

  Lon-Lat

  :   When `occasion = lon-lat`, `occasion_var` must contain a string
      vector of length two containing the longitude and latitude of a
      vessel's location in the `dat`. For example, the current or
      previous haul location.

- alt_var:

  Determines the alternative choices used to calculate the distance
  matrix. `alt_var` may be the centroid of zonal assignment
  (`"zonal centroid"`), `"fishing centroid"`, or the closest point in
  fishing zone (`"nearest point"`). The centroid options require that
  the appropriate centroid table has been saved to the project's FishSET
  Database. See [`create_centroid()`](create_centroid.md) to create and
  save centroids. List existing centroid tables by running
  `list_tables("project", type = "centroid")`.

- min_haul:

  Required, numeric, minimum number of hauls. Zones with fewer hauls
  than the `min_haul` value will not be included in model data.

- spatname:

  Required when `alt_var = 'nearest point'`. `spat` is a spatial data
  file containing information on fishery management or regulatory zones
  boundaries. `sf` objects are recommended, but `sp` objects can be used
  as well. See [`dat_to_sf()`](dat_to_sf.md) to convert a spatial table
  read from a csv file to an `sf` object. To upload your spatial data to
  the FishSETFolder see [`load_spatial()`](load_spatial.md).If `spat`
  should come from the FishSET database, it should be the name of the
  original file name, in quotes. For example,
  `"pollockNMFSZonesSpatTable"`. Use
  [`tables_database()`](tables_database.md) or
  `list_tables("project", type = "spat")` to view the names of spatial
  tables in the FishSET database.

- spatID:

  Required when `alt_var = 'nearest point'`. Variable in `spat` that
  identifies the individual zones or areas.

- outsample:

  Logical, indicating whether this is for primary data or out-of sample
  data.

## Value

Function saves a list of alternative choice matrices to the FishSET
database as `projectAlternativeChoice`. The list includes the
alternative choice list from the user-defined choices. Multiple
alternative choice cases can be added to the list by specifying unique
names. The list is automatically saved to the FishSET database and is
called in `format_model_data`.

## Details

Defines the alternative fishing choices. These choices are used to
develop the matrix of distances between observed and alternative fishing
choices (where they could have fished but did not). The distance matrix
is calculated by the [`format_model_data()`](format_model_data.md)
function. `occasion` defines the observed fishing location and `alt_var`
the alternative fishing location. `occasion_var` identifies an ID column
or set of lon-lat variables needed to create the distance matrix.

Parts of the alternative choice list are pulled by
[`create_expectations()`](create_expectations.md),
[`format_model_data()`](format_model_data.md), and the model run
[`fishset_fit()`](fishset_fit.md)) functions. These output include
choices of which variable to use for catch and which zones to include in
analyses based on a minimum number of hauls per trip within a zone. Note
that if the alternative choice list is modified, the
[`create_expectations()`](create_expectations.md) and
[`format_model_data()`](format_model_data.md) functions should also be
updated before rerunning models.
