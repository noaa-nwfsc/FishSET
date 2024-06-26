% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_variables.R
\name{set_quants}
\alias{set_quants}
\title{Create factor variable from quantiles}
\usage{
set_quants(
  dat,
  project,
  x,
  quant.cat = c(0.1, 0.2, 0.25, 0.33, 0.4),
  custom.quant = NULL,
  name = "set_quants"
)
}
\arguments{
\item{dat}{Primary data containing information on hauls or trips.
Table in FishSET database contains the string 'MainDataTable'.}

\item{project}{Project name.}

\item{x}{Variable to transform into quantiles.}

\item{quant.cat}{Quantile options: \code{"0.2"}, \code{"0.25"}, \code{"0.33"}, and \code{"0.4"}
\itemize{
  \item{0.1:  (0\%, 10\%, 20\%, 30\%, 40\%, 50\%, 60\%, 70\%, 80\%, 90\%, 100\%)}
  \item{0.2:  (0\%, 20\%, 40\%, 60\%, 80\%, 100\%)}
  \item{0.25: (0\%, 25\%, 50\%, 75\%, 100\%)}
  \item{0.33: (0\%, 33\%, 66\%, 100\%)}
  \item{0.4:  (0\%, 10\%, 50\%, 90\%, 100\%)}
  }}

\item{custom.quant}{Vector, user defined quantiles.}

\item{name}{String, name of created vector. Defaults to name of the function if not defined.}
}
\value{
Primary dataset with quantile variable added.
}
\description{
Create a factor variable from numeric data.  Numeric variable is split into categories based on quantile categories.
}
\examples{
\dontrun{
pollockMainDataTable <- set_quants(pollockMainDataTable, 'pollock', 'HAUL', 
   quant.cat=.2, 'haul.quant')
}
}
