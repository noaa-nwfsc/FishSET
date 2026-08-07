# Import, parse, and save spatial data

Saves a spatial table to the FishSETFolder as a geojson file. A project
must exist before running `load_spatial()`. See
[`load_maindata`](load_maindata.md) to create a new project.

## Usage

``` r
load_spatial(
  spat,
  name = NULL,
  over_write = TRUE,
  project,
  data.type = NULL,
  lon = NULL,
  lat = NULL,
  id = NULL,
  ...
)
```

## Arguments

- spat:

  File name, including path, of spatial data.

- name:

  Name spatial data should be saved as in FishSET project folder. Cannot
  be empty or contain spaces.

- over_write:

  Logical, If `TRUE`, saves `spat` over previously saved data table in
  the FishSET project folder.

- project:

  String, name of project.

- data.type:

  Data type argument passed to [`read_dat`](read_dat.md). If reading
  from a shape folder use `data.type = "shape"`.

- lon:

  Variable or list from `spat` containing longitude data. Required for
  csv files. Leave as `NULL` if `spat` is a shape or json file.

- lat:

  Variable or list from `spat` containing latitude data. Required for
  csv files. Leave as `NULL` if `spat` is a shape or json file

- id:

  Polygon ID column. Required for csv files. Leave as `NULL` if `spat`
  is a shape or json file.

- ...:

  Additional argument passed to [`read_dat`](read_dat.md).

## Details

Function to import, parse, and saved project folder in \`FishSETFolder\`
directory. To export as shape file, use [`write_dat`](write_dat.md)
specifying \`type='shp'\`. `load_spatial()` performs basic quality check
before saving spatial tables to the project data folder as a geojson
file. To be saved, the spatial must pass the checks in
[`check_spatdat`](check_spatdat.md). The spatial table is converted to
an `sf` object, and checked for unique rows and empty columns. The
naming convention for spatial tables is "projectNameSpatTable". See
[`table_view`](table_view.md) to view/load spatial tables into the
working environment.

## See also

[`table_view`](table_view.md), [`load_maindata`](load_maindata.md),
[`write_dat`](write_dat.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# upload from filepath
load_spatial(spat = "FILE/PATH/TO/SPAT", name = 'tenMinSqr', 
             over_write = TRUE, project = 'pcod')

# upload from object in working environment
load_spatial(spat = NMFSAreas, name = "NMFS", project = "pcod")

# upload from an existing FishSET spatial table
load_spatial(spat = "pcodNMFSSpatTable", name = "NMFS", project = "pcod2020")
} # }
```
