% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.R
\name{filter_summary}
\alias{filter_summary}
\title{Select function calls to display}
\usage{
filter_summary(sum_tab, filter_list)
}
\arguments{
\item{sum_tab}{Summary table to filter.}

\item{filter_list}{A named list of integers. Each list entry should
contain the name of the function and the row number(s) to filter by.
For example, \code{list(temporal_mod = 2)} will display the second row of
 the temporal_mod dataframe in a summary list.}
}
\description{
Select function calls to display
}
\examples{
\dontrun{
filter_summary(function_summary(),
              filter_list = list(set_quants = 2, temporal_mod = 2))
}
}
\seealso{
\code{\link{function_summary}}
}
\keyword{internal}
