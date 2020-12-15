% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{agg_helper}
\alias{agg_helper}
\title{Aggregating function}
\usage{
agg_helper(dataset, value, period = NULL, group = NULL, fun = "sum")
}
\arguments{
\item{dataset}{`MainDataTable` to aggregate.}

\item{value}{String, name of variable to aggregate.}

\item{period}{String, name of period variable to aggregate by.}

\item{group}{String, name of grouping variable(s) to aggregate by.}

\item{fun}{String, function name to aggregate by. Also accepts anonymous functions.}
}
\description{
Aggregating function
}
\keyword{internal}