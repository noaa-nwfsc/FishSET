% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{quiet_safe_test}
\alias{quiet_safe_test}
\title{Quietly and safely test function}
\usage{
quiet_safe_test(.f)
}
\arguments{
\item{.f}{Name of a function to quietly and safely test.}
}
\description{
Used for package functions: safely catches errors and quietly catches warnings
and messages. Both capture results.
}
\seealso{
\code{\link[purrr]{quietly}} and \code{\link[purrr]{safely}}
}
\keyword{internal}
