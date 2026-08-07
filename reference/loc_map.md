# Define source location for MapViewer folder Returns the location of the MapViewer folder

Define source location for MapViewer folder Returns the location of the
MapViewer folder

## Usage

``` r
loc_map(project)
```

## Arguments

- project:

  Project name

## Details

if loc2 is not in the working environment, then the default location is
use

## Examples

``` r
if (FALSE) { # \dontrun{
loc_map() # will return output folder location within the fishset package
loc2 <- getwd()
loc_map() #will return output folder location as within the working directory
} # }
```
