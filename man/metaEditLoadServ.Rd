% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata_modules.R
\name{metaEditLoadServ}
\alias{metaEditLoadServ}
\title{Load server for metadata gui (Edit section)
Contains actions needed to view, edit, and delete metadata entries.}
\usage{
metaEditLoadServ(id, cols, meta)
}
\arguments{
\item{id}{An ID string. It is recommended to use an informative and unique 
string such as \code{"meta_edit"}.}

\item{cols}{Reactive values object containing column names from a FishSET 
table.}

\item{meta}{Reactive values object to store metadata in.}
}
\description{
Load server for metadata gui (Edit section)
Contains actions needed to view, edit, and delete metadata entries.
}
\details{
This module is used in the \code{\link{metadata_gui}} function 
  and in the FishSET app. It loads FishSET tables and creates metadata
  fields (text boxes) that users can edit.
}
\keyword{internal}
