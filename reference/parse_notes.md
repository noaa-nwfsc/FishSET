# Selectively display a note section

Selectively display a note section

## Usage

``` r
parse_notes(project, date = NULL, section, output = "print")
```

## Arguments

- project:

  The project name.

- date:

  Date to pull notes from. If NULL then the most recent version of notes
  from the project are retrieved.

- section:

  The note section to display. Options include "upload" for Upload data,
  "quality" for Data quality evaluation, "explore" for Data exploration,
  "fleet" for Fleet functions, "analysis" for Simple analysis,
  "new_variable" for Create new variable, "alt_choice" for Alternative
  choice, "models", and "bookmark".

- output:

  Output type. "print" returns formatted notes. "string" returns a
  character vector of the notes. "print" is recommended for displaying
  notes in a report.

## Examples

``` r
if (FALSE) { # \dontrun{
parse_notes("pollock", type = "explore")
} # }
```
