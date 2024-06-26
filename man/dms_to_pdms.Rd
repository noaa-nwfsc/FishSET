% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/degree.R
\name{dms_to_pdms}
\alias{dms_to_pdms}
\title{Convert DMS to Packed DMS}
\usage{
dms_to_pdms(x, type, dec = FALSE, as_num = FALSE)
}
\arguments{
\item{x}{Latitude or longitude vector.}

\item{type}{\code{"lat"} or \code{"lon"}.}

\item{dec}{Logical, whether to keep decimal if present.}

\item{as_num}{Logical, whether to convert to numeric. If \code{FALSE}, a 
character string is outputted.}
}
\description{
Convert DMS to Packed DMS
}
\details{
Primarily used for testing whether \code{degree()} can convert
  Packed DMS to decimal degrees.
}
\keyword{internal}
