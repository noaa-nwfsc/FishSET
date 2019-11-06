% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_kernel.r
\name{map_kernel}
\alias{map_kernel}
\title{Wrapper function to map kernel densities}
\usage{
map_kernel(type, latlon, group = NULL, facet = FALSE, minmax = NULL)
}
\arguments{
\item{type}{Type of plot ("point", "contours", "gradient"). Note if you
have a group, you must facet when choosing "gradient" (cannot overlap
polygons clearly).}

\item{latlon}{A matrix of (# of observations) x 2 corresponding to 
latitude/longitude pair coordinates in decimal degrees.}

\item{group}{Optional group parameter if user maps based on a factor 
group. Should be a factor with length of (# of observations), 
where each observation corresponds to the latlon coordinate of the same 
index. Recall that the legend will output the names of
factor levels as you have named them. (See ?factor).}

\item{facet}{Optional facet parameter if user maps each group as a 
separate facet ("TRUE", "FALSE"), default = "FALSE".}

\item{minmax}{Optional map extent parameter, a vector (num) of length 4 
corresponding to c(minlat, maxlat, minlon, maxlon).}
}
\value{
mapout: ggplot2 object
}
\description{
Wrapper function to map kernel densities using ggplot2
}
\examples{
\dontrun{
type <- "contours"
latlon <- as.matrix(cbind(datatomap.2001$lat, datatomap.2001$lon))
group <- datatomap.2001$Vessel
minmax = c(50, max(datatomap.2001$lat,datatomap.2001$lat)*1.001,
 min(datatomap.2001$lon,datatomap.2001$lon)*1.001 , -142.5)

map_kernel(type,latlon,group=group,minmax=minmax)

type <- "gradient"
map_kernel(type,latlon,group=group,facet=TRUE,minmax=minmax)

type <- "point"
map_kernel(type,latlon)
}    
}