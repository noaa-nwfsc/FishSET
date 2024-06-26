% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bycatch.R
\name{bycatch_plot}
\alias{bycatch_plot}
\title{Bycatch plot helper}
\usage{
bycatch_plot(
  dat,
  cpue,
  catch,
  period,
  group,
  facet_by,
  names,
  value,
  scale,
  conv,
  tran,
  format_lab
)
}
\arguments{
\item{dat}{Data used to create plot.}

\item{cpue}{String, cpue variable(s) passed from \code{bycatch}.}

\item{catch}{String, catch variable(s) passed from \code{bycatch}.}

\item{period}{String, period passed from \code{bycatch}.}

\item{group}{String, grouping variable(s) passed from \code{bycatch}.}

\item{facet_by}{String, facet variable(s) passed from \code{bycatch}.}

\item{names}{String, species names for plot labels passed from \code{bycatch}.}

\item{value}{String, whether to return percent or sum of catch.}

\item{scale}{String, facet scale passed from \code{bycatch}.}

\item{conv}{Convert catch variable to \code{"tons"}, \code{"metric_tons"}, or 
by using a function entered as a string. Defaults to \code{"none"} for no 
conversion.}

\item{tran}{String, scale transformation passed from \code{bycatch}.}

\item{format_lab}{Formatting option for y-axis labels. Options include 
\code{"decimal"} or \code{"scientific"}.}
}
\description{
Creates and formats plots for \code{bycatch}.
}
\keyword{internal}
