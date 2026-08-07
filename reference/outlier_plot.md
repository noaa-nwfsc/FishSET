# Evaluate outliers in plot format

Visualize spread of data and measures to identify outliers.

## Usage

``` r
outlier_plot(
  dat,
  project,
  x,
  dat.remove = "none",
  sd_val = NULL,
  x.dist = "normal",
  date = NULL,
  group = NULL,
  pages = "single",
  output.screen = FALSE,
  log_fun = TRUE
)
```

## Arguments

- dat:

  Primary data containing information on hauls or trips. Table in the
  FishSET database contains the string 'MainDataTable'.

- project:

  String, name of project.

- x:

  Variable in `dat` to check for outliers.

- dat.remove:

  Outlier measure. Values outside the measure are removed. Users can use
  the predefined values (see below) or user-defined distance from the
  mean. For user-defined values, `dat.remove` should be a numeric value.
  For example, `dat.remove = 6` would would result in value outside 6SD
  from the mean being class as outliers. User-defined standard
  deviations from the mean can also be applied using `sd_val`.
  Pre-defined choices: `"none"`, `"5_95_quant"`, `"25_75_quant"`,
  `"mean_2SD"`, `"median_2SD"`, `"mean_3SD"`, `"median_3SD"`. See the
  *Details* section for more information.

- sd_val:

  Optional. Number of standard deviations from mean defining outliers.
  Example, `sd_val = 6` would mean values outside +/- 6 SD from the mean
  would be outliers.

- x.dist:

  Distribution of the data. Choices include: `"normal"`, `"lognormal"`,
  `"exponential"`, `"Weibull"`, `"Poisson"`, `"negative binomial"`.

- date:

  (Optional) date variable to group the histogram by year.

- group:

  (Optional) additional variable to group the histogram by.

- pages:

  Whether to output plots on a single page (`"single"`, the default) or
  multiple pages (`"multi"`).

- output.screen:

  Logical, if true, return plots to the screen. If `FALSE`, returns plot
  to the 'output' folder as a png file.

- log_fun:

  Logical, whether to log function call (for internal use).

## Value

Plot of the data

## Details

The function returns three plots: the data, a probability plot, and a
Q-Q plot. The *data plot* returns `x` against row number. Red points are
data points that would be removed based on `dat.remove`. Blue points are
data points within the bounds of `dat.remove`. If `dat.remove` is
`"none"`, then only blue points will be shown. The *probability plot* is
a histogram of the data, after applying `dat.remove`, with the fitted
probability distribution based on `x.dist`. `group` groups the histogram
by a variable from `dat`, `date` groups the histogram by year. The *Q-Q
plot* plots are sampled quantiles against theoretical quantiles, after
applying `dat.remove`.  
  
The `dat.remove` choices are:

- numeric value: Remove data points outside +/- \`x\`SD of the mean

- none: No data points are removed

- 5_95_quant: Removes data points outside the 5th and 95th quantiles

- 25_75_quant: Removes data points outside the 25th and 75th quantiles

- mean_2SD: Removes data points outside +/- 2SD of the mean

- median_2SD: Removes data points outside +/- 2SD of the median

- mean_3SD: Removes data points outside +/- 3SD of the mean

- median_3SD: Removes data points outside +/- 3SD of the median

The distribution choices are:

- normal

- lognormal

- exponential

- Weibull

- Poisson

- negative binomial

## Examples

``` r
if (FALSE) { # \dontrun{

outlier_plot(pollockMainDataTable, 'pollock', x = 'Haul', dat.remove = 'mean_2SD', 
             x.dist = 'normal', output.screen = TRUE)
# user-defined outlier        
outlier_plot(pollockMainDataTable, 'pollock', x = 'Haul', dat.remove = 6, 
             x.dist = 'lognormal', output.screen = TRUE)
} # }
```
