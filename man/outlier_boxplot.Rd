% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier_check.R
\name{outlier_boxplot}
\alias{outlier_boxplot}
\title{Boxplot to assess outliers}
\usage{
outlier_boxplot(dat, project, x = NULL)
}
\arguments{
\item{dat}{Primary data containing information on hauls or trips.
Table in the FishSET database contains the string 'MainDataTable'.}

\item{project}{Project name.}

\item{x}{Variables in \code{dat} to check for outliers. Leave as 
\code{x = NULL} to plot all numeric variables. To specify multiple variables 
use \code{c('var1', 'var2')}}
}
\value{
Box and whisker plot for all numeric variables. Saved to `output` folder.
}
\description{
Boxplot to assess outliers
}
\details{
Creates a visual representation of five summary statistics: 
  median, two hinges (first and third quartiles), two whiskers (extends to 
  1.5*IQR where IQR is the distance between the first and third quartiles.
  "Outlying" points, those beyond the two whiskers (1.5*IQR) are shown 
  individually.
}
