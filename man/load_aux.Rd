% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_parser.R
\name{load_aux}
\alias{load_aux}
\title{Save auxiliary data}
\usage{
load_aux(dat, x, over_write = TRUE, project = NULL)
}
\arguments{
\item{dat}{Main data frame. Table in fishset_db database should contain the string `MainDataTable`.}

\item{x}{Name of auxiliary data frame to be saved.}

\item{over_write}{TRUE/FALSE Save over previously saved file or not.}

\item{project}{Name of project for attaching to name of the table.}
}
\description{
Save auxiliary data
}
\details{
Auxiliary data is any additional data required beyond the main data and the port data. Auxiliary data can be anything you want to merge with the main data frame (ex. prices by date, vessel characteristics, or fishery season).
The auxilliary data does not have to be at a haul or trip level but must contain a variable to connect the auxilliary data to the main data.
The function checks that at least one column name of the auxiliary data matches a column name in the main data table.  Further checks are run using the data_verification_call function before saving the new data frame to the fishset_db database.
The data is saved in the fishset_db database as the raw data and the working data. In both cases, the table name is the `project`, if defined, and the file name `x`. Date is also attached to the name for the raw data.
}
\examples{
\dontrun{  
load_aux(dataset='pcodMainDataTable', x=FisherySeason, over_write=TRUE, project='pcod') 
}
}