# Import data from local file directory or webpage into the R environment

Import data from local file directory or webpage into the R environment

## Usage

``` r
read_dat(
  x,
  data.type = NULL,
  is.map = FALSE,
  drv = NULL,
  dbname = NULL,
  user = NULL,
  password = NULL,
  ...
)
```

## Arguments

- x:

  Name and path of dataset to be read in. To load data directly from a
  webpage, `x` should be the web address.

- data.type:

  Optional. Data type can be defined by user or based on the file
  extension. If undefined, `data.type` is the string after the last
  period or equal sign. `data.type` must be defined if `x` is the path
  to a shape folder, if the file is a Google spreadsheet use
  `data.type = 'google'`, or if the correct extension cannot be derived
  from `x`. R, comma-delimited, tab-delimited, excel, Matlab, json,
  geojson, sas, spss, stata, and html, and XML data extensions do not
  have to be specified.

- is.map:

  logical, for .json file extension, set `is.map = TRUE` if data is a
  spatial file. Spatial files ending in .json will not be read in
  properly unless `is.map = TRUE`.

- drv:

  Use with sql files. Database driver.

- dbname:

  Use with sql files. If required, database name.

- user:

  Use with sql files. If required, user name for SQL database.

- password:

  Use with sql files. If required, SQL database password.

- ...:

  Optional arguments

## Details

Uses the appropriate function to read in data based on data type. Use
[`write_dat`](write_dat.md) to save data to the `data` folder in the
`project` directory. Supported data types include shape, csv, json,
matlab, R, spss, and stata files. Use `data.type = 'shape'` if `x` is
the path to a shape folder. Use `data.type = 'google'` if the file is a
Google spreadsheet.

For sql files, use `data.type = 'sql'`. The function will connect to the
specified DBI and pull the table. Users must specify the DBI driver
(`drv`), for example:
[`RSQLite::SQLite()`](https://rsqlite.r-dbi.org/reference/SQLite.html),
`RPostgreSQL::PostgreSQL()`, `odbc::odbc()`. Further arguments may be
required, including database name (`dbname`), user id (`user`), and
password (`password`).

Additional arguments can be added, such as skip lines `skip = 2` and
header `header = FALSE`. To specify the separator argument for a
delimited file, include tab-delimited, specify `data.type = 'delim'`.

For more details, see [`load`](https://rdrr.io/r/base/load.html) for
loading R objects,
[`read_csv`](https://readr.tidyverse.org/reference/read_delim.html) for
reading in comma separated value files,
[`read_tsv`](https://readr.tidyverse.org/reference/read_delim.html) for
reading in tab separated value files,
[`read_delim`](https://readr.tidyverse.org/reference/read_delim.html)
for reading in delimited files,
[`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)
for reading in excel files (xls, xlsx),
[`st_read`](https://r-spatial.github.io/sf/reference/st_read.html) for
reading in geojson , GeoPackage files, and shape files,
[`readMat`](https://rdrr.io/pkg/R.matlab/man/readMat.html) for reading
in matlab data files,
[`read_dta`](https://haven.tidyverse.org/reference/read_dta.html) for
reading in stata data files,
[`read_spss`](https://haven.tidyverse.org/reference/read_spss.html) for
reading in spss data files,
[`read_sas`](https://haven.tidyverse.org/reference/read_sas.html) for
reading in sas data files, and
[`fromJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
for reading in json files.
[`read_xml`](http://xml2.r-lib.org/reference/read_xml.md) for reading in
XML files. Further processing may be required.
[`read_html`](http://xml2.r-lib.org/reference/read_xml.md) for reading
in html tables. See `read_sheet` in
[`range_read`](https://googlesheets4.tidyverse.org/reference/range_read.html)
for reading in google spreadsheets. Google spreadsheets require
`data.type` be specified. Use `data.type = 'google'`.
[`read_ods`](https://docs.ropensci.org/readODS/reference/read_ods.html)
for reading in open document spreadsheets.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read in shape file
dat <- read_dat('C:/data/nmfs_manage_simple', data.type = 'shape')

# Read in spatial data file in json format
dat <- read_dat('C:/data/nmfs_manage_simple.json', is.map = TRUE)

# read in data directly from web page
dat <- read_dat("https://s3.amazonaws.com/assets.datacamp.com/blog_assets/test.txt", 
                data.type = 'delim', sep = '', header = FALSE)
} # }
```
