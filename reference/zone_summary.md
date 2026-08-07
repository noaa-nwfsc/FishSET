# Summarize zones, closure areas

\`zone_summary\` counts observations and aggregates values in \`dat\` by
regulatory zone or closure area.

## Usage

``` r
zone_summary(
  dat,
  spat,
  project,
  zone.dat,
  zone.spat,
  dat_lon,
  dat_lat,
  count = TRUE,
  var = NULL,
  group = NULL,
  fun = NULL,
  na.rm = TRUE,
  dat_center = TRUE,
  plot_type = "dynamic",
  output = "plot"
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string 'MainDataTable'.

- spat:

  A spatial data file containing information on fishery management or
  regulatory zones boundaries. \`sf\` objects are recommended, but
  \`sp\` objects can be used as well. See \[dat_to_sf()\] to convert a
  spatial table read from a csv file to an \`sf\` object. To upload your
  spatial data to the FishSETFolder see \[load_spatial()\].

- project:

  Name of project.

- zone.dat:

  Name of zone ID column in \`dat\`.

- zone.spat:

  Name of zone ID column in \`spat\`.

- dat_lon:

  Name of longitude column in \`dat\`.

- dat_lat:

  Name of latitude column in \`dat\`.

- count:

  Logical. if \`TRUE\`, then the number observations per zone will be
  returned. Can be paired with \`fun = "percent"\` and \`group\`.
  \`zone_summary\` will return an error if \`var\` is include and
  \`count = TRUE\`.

- var:

  Optional, name of numeric variable to aggregate by zone/closure area.

- group:

  Name of grouping variable to aggregate by zone/closure area. Only one
  variable is allowed.

- fun:

  Function name (string) to aggregate by. \`"percent"\` the percentage
  of observations in a given zone. Other options include "sum", "mean",
  "median", "min", and "max".

- na.rm:

  Logical, whether to remove zones with zero counts.

- dat_center:

  Logical, whether the plot should center on \`dat\` (\`TRUE\`) or
  \`spat\` (\`FALSE\`). Recommend \`dat_center = TRUE\` when aggregating
  by regulatory zone and \`dat_center = FALSE\` when aggregating by
  closure area.

- plot_type:

  Type of plot output; dynamic (\`leaflet\` object) or static
  (\`ggplot\` object)

- output:

  Output a \`"plot"\`, \`"table"\`, or both (\`"tab_plot"\`). Defaults
  to \`"plot"\`.

## Details

Observations in \`dat\` must be assigned to regulatory zones to use this
function. See \[assignment_column()\] for details. \`zone_summary\` can
return: the number of observations per zone (\`count = TRUE\`, \`fun =
NULL\`, \`group = NULL\`), the percentage of observations by zone
(\`count = TRUE\`, \`fun = "percent"\`, \`group = NULL\`), the
percentage of observations by zone and group (\`count = TRUE\`, \`fun =
"percent"\`, \`group = "group"\`), summary of a numeric variable by zone
(\`count = FALSE\`, \`var = "var"\`, \`fun = "sum"\`, \`group = NULL\`),
summary of a numeric variable by zone and group (\`count = FALSE\`,
\`var = "var"\`, \`fun = "sum"\`, \`group = "group"\`), share
(percentage) of a numeric variable by zone (\`count = FALSE\`, \`var =
"var"\`, \`fun = "percent"\`, \`group = NULL\`), share (percentage) of a
numeric variable by zone and group (\`count = FALSE\`, \`var = "var"\`,
\`fun = "percent"\`, \`group = "group"\`).

## Examples

``` r
if (FALSE) { # \dontrun{

# count # of obs
zone_summary(pollockMainTable, spat = nmfs_area, zone.dat = "ZoneID", 
            zone.spat = "NMFS_AREA")
            
# percent of obs
zone_summary(pollockMainTable, spat = nmfs_area, zone.dat = "ZoneID", 
            zone.spat = "NMFS_AREA", count = TRUE, fun = "percent")

# count by group
zone_summary(pollockMainTable, spat = nmfs_area, zone.dat = "ZoneID", 
            zone.spat = "NMFS_AREA", group = "GEAR_TYPE")   

# total catch by zone           
zone_summary(pollockMainTable, spat = nmfs_area, zone.dat = "ZoneID", 
            zone.spat = "NMFS_AREA", var = "OFFICIAL_TOTAL_CATCH_MT",
            count = FALSE, fun = "sum")  

# percent of catch by zone           
zone_summary(pollockMainTable, spat = nmfs_area, zone.dat = "ZoneID", 
            zone.spat = "NMFS_AREA", var = "OFFICIAL_TOTAL_CATCH_MT",
            count = FALSE, fun = "percent")         
            
} # }
```
