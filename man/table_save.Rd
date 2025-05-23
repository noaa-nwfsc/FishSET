% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql_functions.R
\name{table_save}
\alias{table_save}
\title{Save an existing FishSET DB table}
\usage{
table_save(table, project, type, name = NULL)
}
\arguments{
\item{table}{A dataframe to save to the FishSET Database.}

\item{project}{Name of project.}

\item{type}{The table type. Options include, \code{"main"} for primary data tables,
\code{"port"} for port tables, \code{"grid"} for gridded tables, \code{"aux"}
for auxiliary tables.}

\item{name}{String, table name. Applicable only for gridded, auxiliary, and
spatial tables.}
}
\description{
\code{table_save()} updates existing FishSET DB tables. If the table doesn't
exist, the user is reminded to use the appropriate \code{load_} function.
}
