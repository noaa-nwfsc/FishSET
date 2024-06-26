% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{facet_period}
\alias{facet_period}
\title{Create date variables for facetting}
\usage{
facet_period(dataset, facet_date, date, period = NULL)
}
\arguments{
\item{dataset}{Dataset used to create tables/plots in function.}

\item{facet_date}{String, period to facet by ("year", "month", and "week").}

\item{date}{String, Data variable used to convert to periods.}

\item{period}{String, period name. Only needed if summarizing over time.}
}
\description{
Create date variables for facetting
}
\keyword{internal}
