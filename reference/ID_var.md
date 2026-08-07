# Create ID variable

Create ID variable from one or more variables

## Usage

``` r
ID_var(
  dat,
  project,
  vars,
  name = NULL,
  type = "string",
  drop = FALSE,
  sep = "_",
  log_fun = TRUE
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in
  FishSET database contains the string \`MainDataTable\`.

- project:

  Project name.

- vars:

  Character string, additional column(s) in `dat` that define unique
  observations.

- name:

  String, name of new ID column.

- type:

  String, the class type of the new ID column. Choices are \`string“ or
  \`integar\`. \`string\` returns a character vector where each column
  in `vars` is combined and separated by `sep`. \`integer\` returns an
  integer vector where each value corresponds to a unique group in
  `vars`.

- drop:

  Logical, whether to drop columns in `vars`.

- sep:

  Symbol used to combined variables.

- log_fun:

  Logical, whether to log function call (for internal use).

## Value

Returns the \`MainDataTable\` with the ID variable included.

## Details

ID variable can be based on a single or multiple variables. Use
`sep = TRUE` if dropping variables that create the ID variable.

## Examples

``` r
if (FALSE) { # \dontrun{
pcodMainDataTable <- ID_var(pcodMainDataTable, "pcod", name = "PermitID", 
        vars = c("GEAR_TYPE", "TRIP_SEQ"), type = 'integar')
pcodMainDataTable <- ID_var(pcodMainDataTable, "pcod", name = "PermitID", 
        vars = c("GEAR_TYPE", "TRIP_SEQ"), type = 'string', sep="_")
} # }
```
