# Create the distance matrix

Create the distance matrix

## Usage

``` r
create_dist_matrix(
  dataset,
  unique_obs_id,
  zoneID,
  spat = NULL,
  spatID = NULL,
  alt_var,
  occasion,
  occasion_var = NULL,
  dataZoneTrue,
  zone_cent = NULL,
  fish_cent = NULL,
  choice,
  units,
  port = NULL,
  crs = NULL
)
```

## Arguments

- dataset:

  Primary data set

- unique_obs_id:

  Column name in 'dataset' that represents unique observation (row) id.

- zoneID:

  Column name in 'dataset' for the zone identifier.

- spat:

  Spatial table (sf object) containing polygons or points for locations.

- spatID:

  Column name in 'spat' for the Zone/area ID.

- alt_var:

  Defines the alternative choice location. One of c("zonal centroid",
  "fishing centroid", "nearest point").

- occasion:

  Defines the origin location. One of c("zonal centroid", "fishing
  centroid", "port", "lon-lat").

- occasion_var:

  Variable(s) needed to define the origin location. - For 'occasion =
  "centroid"': The column name for the previous area. - For 'occasion =
  "port"': The column name in 'dataset' for port. - For 'occasion =
  "lon-lat"': A character vector of c(lon, lat) column names. Note that
  longitude must be the first column name in the vector.

- dataZoneTrue:

  Logical vector indicating which rows (observations) meet the minimum
  haul/obs requirement.

- zone_cent:

  Zonal centroid table (data.frame with zone ID, cent.lon, cent.lat).

- fish_cent:

  Fishing centroid table (data.frame with zone ID, cent.lon, cent.lat).

- choice:

  Vector of observed choice zones, same length as 'dataset'.

- units:

  Distance units (e.g., "km", "mi", "nm"). Passed to 'sf::st_distance'.

- port:

  Port table (data.frame with Port_Name, Port_Long, Port_Lat).

- crs:

  Coordinate reference system (numeric EPSG code or PROJ string).

## Value

A list containing:

- \`dist_matrix\`: The calculated distance matrix.

- \`alt_choice_units\`: The units of distance.

- \`alt_choice_type\`: "distance".

- \`occasion\`: The origin type used.

- \`occasion_var\`: The variable(s) used for the origin.

- \`alt_choice\`: The destination type used.

## Details

Function is called by [`format_model_data`](format_model_data.md) to
generate the distance matrix. Alternative fishing options come from the
Alternative Choice list, generated from the
[`create_alternative_choice`](create_alternative_choice.md) function.
