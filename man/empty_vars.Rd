% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{empty_vars}
\alias{empty_vars}
\title{Check for empty variables}
\usage{
empty_vars(dat, remove = TRUE)
}
\arguments{
\item{dat}{The data.frame to check.}

\item{remove}{Logical, whether to remove empty variables.}
}
\description{
Detects variables that contain all \code{NA}s and removes them if 
\code{remove = TRUE}.
}
\examples{
\dontrun{
dat <- empty_vars(dat)
}
}
\keyword{internal}
