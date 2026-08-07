# Load data from FishSET database into the R environment

Load data from FishSET database into the R environment

## Usage

``` r
load_data(project, name = NULL)
```

## Arguments

- project:

  String, name of project.

- name:

  Optional, name of table in FishSET database. Use this argument if
  pulling raw or dated table (not the working table).

## Value

Data loaded to working environment as the project and ‘MainDataTable’.

## Details

Pulls the primary data table from the FishSET database and loads it into
the working environment as the project and MainDataTable. For example,
if the project was pollock, then data would be saved to the working
environment as 'pollockMainDataTable'.

## Examples

``` r
if (FALSE) { # \dontrun{
load_data('pollock')

load_data('pollock', 'pollockMainDataTable20190101')
} # }
```
