% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fleet_assign.R
\name{fleet_assign}
\alias{fleet_assign}
\title{Create fleet variable using fleet definition table}
\usage{
fleet_assign(
  dat,
  project,
  fleet_tab,
  assign = NULL,
  overlap = FALSE,
  format_var = "string"
)
}
\arguments{
\item{dat}{Primary data containing information on hauls or trips. Table in 
FishSET database contains the string 'MainDataTable'.}

\item{project}{String, name of project.}

\item{fleet_tab}{String, name of the fleet table stored in FishSET database. 
Should contain the string `FleetTable`.}

\item{assign}{Integer, a vector of row numbers from \code{fleet_tab}. Only 
fleet definitions in these rows will be used and added to `MainDataTable`. 
If \code{assign = NULL} (the default), all fleet definitions in the table 
will be used.}

\item{overlap}{Logical; whether overlapping fleet assignments are allowed. 
Defaults to \code{FALSE}.}

\item{format_var}{String. If \code{format_var = "string"}, a single column named 
"fleet" will be added to `MainDataTable`. If \code{overlap = TRUE},
  observations with multiple fleet assignments are duplicated. \code{format_var ="dummy"} 
  outputs a binary column for each fleet in the fleet table. Defaults to \code{"string"}.}
}
\value{
Returns the primary dataset with added fleet variable(s).
}
\description{
Add a fleet ID column to the primary data using a fleet table (see \code{\link{fleet_table}} 
for details).
}
\examples{
\dontrun{
fleet_assign(pollockMainDataTable, 'pollock', fleet_tab = 'pollockFleetTable', 
             overlap = TRUE)
}
}
\seealso{
\code{\link{fleet_table}}
}
