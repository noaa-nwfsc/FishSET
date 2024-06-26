% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confidentiality.R
\name{reset_confid_cache}
\alias{reset_confid_cache}
\title{Reset confidentiality cache tables}
\usage{
reset_confid_cache(project)
}
\arguments{
\item{project}{Project name}
}
\description{
This function deletes all confidentiality check tables stored in 
the \code{"confid_cache.json"} file located in the project output folder. 
Resetting this cache is recommended after a long period of use as check 
tables can accumulate over time.
}
\seealso{
\code{\link{get_confid_cache}}
}
