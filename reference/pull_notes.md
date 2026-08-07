# Pull notes from output folder

Pull notes from output folder

## Usage

``` r
pull_notes(project, date = NULL, output = "print")
```

## Arguments

- project:

  String, the project name.

- date:

  String, date to pull notes from. If NULL, most recent note file is
  retrieved.

- output:

  Output type. "print" returns formatted notes. "string" returns a
  character vector of the notes. "print" is recommended for displaying
  notes in a report.

## Details

Notes are saved to the output folder by project name and date. If date
is not specified then the most recent notes file with the project name
is pulled. Notes are are also saved by FishSET app session; if more than
one session occurred in the same day, each session's notes are pulled
and listed in chronological order.
