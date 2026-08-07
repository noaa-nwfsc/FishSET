# Display dataset summary table

Display dataset summary table

## Usage

``` r
summary_table(project, output = "print")
```

## Arguments

- project:

  Name of project.

- output:

  Output type. "print" returns formatted notes. "table" returns a
  dataframe. "print" is recommended for displaying summary table in a
  report.

## Details

Displays the most recent table created by
[`summary_stats`](summary_stats.md) as a dataframe. Can be used in
console or notebook.
