# Create one or more binned frequency tables

Create one or more binned frequency, relative frequency, or density
table.

## Usage

``` r
nfreq_table(
  dataset,
  var,
  group = NULL,
  bins = 30,
  type = "dens",
  v_id = NULL,
  format_lab = "decimal",
  format_tab = "wide"
)
```

## Arguments

- dataset:

  Primary data containing information on hauls or trips. Table in
  FishSET database should contain the string \`MainDataTable\`.

- var:

  String, name of numeric variable to bin.

- group:

  String, name of variable(s) to group `var` by.

- bins:

  Integer, the number of bins to create.

- type:

  String, the type of binned frequency table to create. `"freq"` creates
  a frequency table, `"perc"` creates a relative frequency table, and
  `"dens"` creates a density table.

- v_id:

  String, name of vessel ID column (used to detect confidential
  information).

- format_lab:

  Formatting option for bin labels. Options include `"decimal"` or
  `"scientific"`.

- format_tab:

  Format table "wide" or "long"
