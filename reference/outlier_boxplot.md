# Boxplot to assess outliers

Boxplot to assess outliers

## Usage

``` r
outlier_boxplot(dat, project, x = NULL)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  Project name.

- x:

  Variables in `dat` to check for outliers. Leave as `x = NULL` to plot
  all numeric variables. To specify multiple variables use
  `c('var1', 'var2')`

## Value

Box and whisker plot for all numeric variables. Saved to \`output\`
folder.

## Details

Creates a visual representation of five summary statistics: median, two
hinges (first and third quartiles), two whiskers (extends to 1.5\*IQR
where IQR is the distance between the first and third quartiles.
"Outlying" points, those beyond the two whiskers (1.5\*IQR) are shown
individually.
