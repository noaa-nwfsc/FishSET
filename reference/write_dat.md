# Write a data table to local file directory

Write a data table to local file directory

## Usage

``` r
write_dat(dat, project, path = NULL, file_type = "csv", ...)
```

## Arguments

- dat:

  Name of data frame in working environment to save to file.

- project:

  String, project name.

- path:

  String, path or connection to write to. If left empty, the file will
  be written to the dat folder in the project directory.

- file_type:

  String, the type of file to write to. Options include `"csv"`, `"txt"`
  (tab-separated text file), `"xlsx"` (excel), `"rdata"`, `"json"`,
  `"stata"`, `"spss"`, `"sas"`, and `"matlab"`.

- ...:

  Additional arguments passed to writing function. See "details" for the
  list of functions.

## Details

Leave `path = NULL` to save `dat` to the `data` folder in the `project`
directory See [`write.table`](https://rdrr.io/r/utils/write.table.html)
for csv and tab-separated files,
[`save`](https://rdrr.io/r/base/save.html) for R data files,
[`write.xlsx`](https://rdrr.io/pkg/openxlsx/man/write.xlsx.html),
[`read_json`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html)
for json files,
[`st_write`](https://r-spatial.github.io/sf/reference/st_write.html) for
geojson files,
[`read_dta`](https://haven.tidyverse.org/reference/read_dta.html) for
Stata files,
[`read_spss`](https://haven.tidyverse.org/reference/read_spss.html) for
SPSS files,
[`read_sas`](https://haven.tidyverse.org/reference/read_sas.html) for
SAS files, and
[`writeMat`](https://rdrr.io/pkg/R.matlab/man/writeMat.html) for Matlab
files, and
[`st_write`](https://r-spatial.github.io/sf/reference/st_write.html) for
shape files.

## Examples

``` r
if (FALSE) { # \dontrun{
# Save to the default data folder in project directory
write_dat(pollockMainDataTable, type = "csv", "pollock")

# Save to defined directory location
write_dat(pollockMainDataTable, path = "C://data/pollock_dataset.csv", 
          type = "csv", "pollock")
          
# Save shape file
write_dat(ST6, path = "C://data//ST6.shp", type = "shp", project = 'Pollock')
} # }
```
