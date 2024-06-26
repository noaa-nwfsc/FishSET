% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view_grid_dat.R
\name{view_grid_dat}
\alias{view_grid_dat}
\title{Visualize gridded data on a map}
\usage{
view_grid_dat(
  grid,
  project,
  lon,
  lat,
  value,
  split_by = NULL,
  group = NULL,
  agg_fun = "mean"
)
}
\arguments{
\item{grid}{Gridded data table to visualize. Use string if visualizing 
a gridded data table in the FishSET Database.}

\item{project}{String, project name.}

\item{lon}{String, variable name containing longitude.}

\item{lat}{String, variable name containing latitude.}

\item{value}{String, variable name containing gridded values, e.g. sea surface 
temperature, wind speed, etc.}

\item{split_by}{String, variable in gridded data table to split by.}

\item{group}{String, variable in gridded data table to group \code{value} by.
In addition to the variable(s) in \code{group}, \code{value} is also 
aggregated by each longitude-latitude pair. The string \code{"lonlat"} is 
a shortcut for \code{group = c("lon", "lat")} which aggregates the \code{value} 
for each longitude-latitude pair across the entire dataset.}

\item{agg_fun}{Aggregating function applied to \code{group}. Defaults to mean.}
}
\description{
Visualize gridded data on a map
}
