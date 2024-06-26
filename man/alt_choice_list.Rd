% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql_functions.R
\name{alt_choice_list}
\alias{alt_choice_list}
\title{Get Alternative Choice List}
\usage{
alt_choice_list(project, name = NULL)
}
\arguments{
\item{project}{Name of project.}

\item{name}{Name of Alternative Choice list in the FishSET database. 
The table name will contain the string "AltMatrix". If \code{NULL}, the 
default table is returned. Use \code{\link{tables_database}} to see a list 
of FishSET database tables by project.}
}
\description{
Returns the Alternative Choice list from the FishSET database.
}
