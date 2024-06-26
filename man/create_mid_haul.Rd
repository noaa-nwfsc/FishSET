% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_variables.R
\name{create_mid_haul}
\alias{create_mid_haul}
\title{Creates haul midpoint latitude and longitude variables}
\usage{
create_mid_haul(
  dat,
  project,
  start = c("lon", "lat"),
  end = c("lon", "lat"),
  name = "mid_haul"
)
}
\arguments{
\item{dat}{Primary data containing information on hauls or trips.
Table in the FishSET database contains the string 'MainDataTable'.}

\item{project}{Project name.}

\item{start}{Character string, variables in \code{dat} defining the longitude and latitude of
the starting location of haul. Must be in decimal degrees.}

\item{end}{Character string, variables in \code{dat} defining the longitude and latitude of the
ending location of haul.  Must be in decimal degrees.}

\item{name}{String, name of new variable. Defaults to `mid_haul`.}
}
\value{
Returns primary dataset with two new variables added: latitude and longitude of haul midpoint.
}
\description{
Calculates latitude and longitude of the haul midpoint and adds two variables
to the primary data set: the midpoint latitude and the midpoint longitude.
}
\details{
Each row of data must be a unique haul. Requires a start and end point for each observation.
}
\examples{
\dontrun{
pollockMainDataTable <- create_mid_haul(pollockMainDataTable, 'pollock', 
    start = c('LonLat_START_LON', 'LonLat_START_LAT'), 
   end = c('LonLat_END_LON', 'LonLat_END_LAT'), name = 'mid_haul')
}
}
