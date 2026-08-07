# create a polygon

create a polygon

## Usage

``` r
make_polygon(coord)
```

## Arguments

- coord:

  Longitude and latitude coordinates forming a polygon. Can be a numeric
  vector of even length or a numeric matrix with two columns.

## Details

If the coordinates create an open polygon the function automatically
closes it and gives a warning.
