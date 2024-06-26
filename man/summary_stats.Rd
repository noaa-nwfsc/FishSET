% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_stats.R
\name{summary_stats}
\alias{summary_stats}
\title{View summary statistics}
\usage{
summary_stats(dat, project, x = NULL, log_fun = TRUE)
}
\arguments{
\item{dat}{Primary data containing information on hauls or trips.
Table in FishSET database contains the string 'MainDataTable'.}

\item{project}{Name of project}

\item{x}{Optional. Variable in \code{dat} to view summary statistics for. If not 
defined, summary stats are displayed for all columns in the dataset.}

\item{log_fun}{Logical, whether to log function call (for internal use).}
}
\description{
View summary statistics in table format for entire dataset or for a specific
variable.
}
\details{
Prints summary statistics for each variable in the data set. If 
  \code{x} is specified, summary stats will be returned only for that variable.
  Numeric variables are summarized by minimum, median, mean, maximum, and the 
  number of NA's, unique values, and zeros. Non-numeric variables are summarized
  by first value and the number of NA's, unique values, and empty values. 
  Function is called in the \code{\link{data_check}} function.
}
\examples{
\dontrun{
summary_stats(pcodMainDataTable, project = "pcod")

summary_stats(pcodMainDataTable, project = "pcod", x = "HAUL")
}

}
\keyword{statistics}
\keyword{summary}
