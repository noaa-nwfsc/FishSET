% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_fishing_centroid.R
\name{find_fishing_centroid}
\alias{find_fishing_centroid}
\title{Create fishing or weighted fishing centroid}
\usage{
find_fishing_centroid(
  dat,
  project,
  zoneID,
  weight.var = NULL,
  lon.dat,
  lat.dat,
  names = NULL,
  cent.name = NULL,
  log.fun = TRUE
)
}
\arguments{
\item{dat}{Primary data containing information on hauls or trips. Table in 
FishSET database contains the string 'MainDataTable'.}

\item{project}{Name of project}

\item{zoneID}{Variable in \code{dat} that identifies zonal assignments or the If 
\code{spat} is class sf, \code{zoneID} should be name of list containing 
information on zones.}

\item{weight.var}{Variable from \code{dat} for weighted average. If 
\code{weight.var} is defined, the centroid is defined by the latitude and 
longitude of fishing locations in each zone weighted by \code{weight.var}.}

\item{lon.dat}{Required. Longitude variable in \code{dat}.}

\item{lat.dat}{Required. Latitude variable in \code{dat}.}

\item{names}{Then names of the fishing centroid columns to be added. A vector
of length two in the order of \code{c("lon", "lat")}. The default is 
\code{c("fish_cent_lon", "fish_cent_lat")} and 
\code{c("weight_cent_lon", "weight_cent_lat")} if \code{weight.var} is used.}

\item{cent.name}{A string to include in the centroid table name. Table names 
take the form of `"projectNameFishCentroid"` for fishing centroids.}

\item{log.fun}{Logical, whether to log function call (for internal use).}
}
\value{
Returns primary dataset with fishing centroid and, if \code{weight.var} 
  is specified, the weighted fishing centroid.
}
\description{
Create fishing or weighted fishing centroid
}
\details{
Fishing centroid defines the centroid by mean latitude and longitude 
  of fishing locations in each zone. Weighted centroid defines the centroid 
  by the mean latitude and longitude of fishing locations in each zone weighted 
  by the \code{weight.var}. The fishing and weighted centroid variables can be 
  used anywhere latitude/longitude variables appear. Each observation in 
  \code{dat} must be assigned to a fishery or regulatory area/zone. If the zone 
  identifier exists in \code{dat} and is not called \code{'ZoneID'}, then  
  \code{zoneID} should be the variable name containing the zone identifier. If a 
  zone identifier variable does not exist in \code{dat}, \code{spat} must be 
  be specified and \code{zoneID} must be zone identifier in \code{spat}. The 
  \code{assignment_column} function will be run and a zone identifier variable 
  added to \code{dat}.
}
\keyword{centroid,}
\keyword{zone}
