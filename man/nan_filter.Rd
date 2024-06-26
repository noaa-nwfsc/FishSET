% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nan_filter.R
\name{nan_filter}
\alias{nan_filter}
\title{Identify, remove, or replace NaNs}
\usage{
nan_filter(
  dat,
  project,
  x = NULL,
  replace = FALSE,
  remove = FALSE,
  rep.value = "mean",
  over_write = FALSE
)
}
\arguments{
\item{dat}{Primary data containing information on hauls or trips. Table in 
the FishSET database contains the string 'MainDataTable'.}

\item{project}{Project name.}

\item{x}{Character string of variables to remove or replace NaNs.}

\item{replace}{Logical, If \code{TRUE}, NaNs are replaced. Defaults to \code{FALSE}.}

\item{remove}{Logical, if \code{TRUE}, removes the entire row of the dataset 
where NaN is present. Defaults to \code{FALSE}.}

\item{rep.value}{Value to replace all NaNs in a numeric column. Defaults to 
the mean value of the column. Other options include \code{"median"} or a
numeric value, e.g. \code{rep.value = 0}.}

\item{over_write}{Logical, If \code{TRUE}, saves data over previously saved 
data table in the FishSET database. Defaults to \code{FALSE}.}
}
\value{
If \code{replace} and \code{remove} are \code{FALSE} then a statement 
of whether NaNs are found is returned. If either \code{replace} or \code{remove} 
is \code{TRUE} the modified primary dataset is returned.
}
\description{
Replaces NaNs in the primary data with the chosen value or removes rows 
containing NaNs
}
\details{
To check for NaNs across \code{dat} run the function specifying only 
  \code{dat} (\code{nan_filter(dataset, project)}). The function will return 
  a statement of which variables, if any, contain NaNs. To remove NaNs, use 
  \code{remove = TRUE}. All rows containing NaNs in \code{x} will be removed 
  from \code{dat}. To replace NaNs, use \code{replace = TRUE}. If both 
  \code{replace} and \code{remove} are \code{TRUE} then \code{replace} is used. 
  If \code{replace} is \code{FALSE} and \code{rep.value} is not defined, then 
  NaNs are replaced  with mean value. The modified dataset will be returned 
  if \code{replace = TRUE} or \code{remove = TRUE}. Save the modified data 
  table to the FishSET database by setting \code{over_write = TRUE)}.
}
\examples{
\dontrun{
nan_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT')

mod.dat <- nan_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT', 
                      replace = TRUE)
                      
mod.dat <- nan_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT',
                      replace = TRUE, rep.value = 0)
                      
mod.dat <- nan_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT', 
                      remove = TRUE)
}
}
\keyword{NaN}
