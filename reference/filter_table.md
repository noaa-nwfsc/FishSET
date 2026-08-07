# Define and store filter expressions

Define and store filter expressions

## Usage

``` r
filter_table(dat, project, x, exp)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- x:

  Variable in `dat` over which filter will be applied.

- exp:

  Filter expression. Should take on the form of `"x < 100"` or
  `"is.na(x) == FALSE"`.

## Value

Filter expressions saved as a table to the FishSET database.

## Details

This function allows users to define and store data filter expressions
which can then be applied to the data. The filter table will be saved in
the FishSET database under the `project` name and \`filterTable\`. The
new filter functions are added each time the function is run and the
table is automatically updated in the FishSET database. The function
call will be logged in the log file.

## Examples

``` r
if (FALSE) { # \dontrun{
filter_table(pcodMainDataTable, 'pcod', x = 'PERFORMANCE_Code',
             exp = 'PERFORMANCE_Code == 1')
} # }
```
