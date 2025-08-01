% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logit_predict.R
\name{logit_predict}
\alias{logit_predict}
\title{Logit predict}
\usage{
logit_predict(
  project,
  mod.name,
  use.scalers = FALSE,
  scaler.func = NULL,
  outsample = FALSE,
  outsample.mod.name = NULL
)
}
\arguments{
\item{project}{Name of project}

\item{mod.name}{Name of saved model to use. Argument can be the name of the model or can pull 
the name of the saved "best" model. Leave \code{mod.name} empty to use the saved "best" 
model. If more than one model is saved, \code{mod.name} should be the numeric indicator of
which model to use. Use \code{table_view("modelChosen", project)} to view a table of 
saved models.}

\item{use.scalers}{Input for \code{create_model_input()}. Logical, should data be normalized? 
Defaults to \code{FALSE}. Rescaling factors are the mean of the numeric vector unless 
specified with \code{scaler.func}.}

\item{scaler.func}{Input for \code{create_model_input()}. Function to calculate 
rescaling factors.}

\item{outsample}{Logical, \code{FALSE} if predicting probabilities for primary data, and 
\code{TRUE} if predicting for out-of-sample data. \code{outsample = FALSE} is the 
default setting.}

\item{outsample.mod.name}{If predicting out-of-sample data, provide the out-of-sample model 
design name. \code{outsample.mod.name = NULL} by default.}
}
\value{
Returns probability of logit model by choice
}
\description{
Prediction component from logit models (non mixed) called in run_policy()
}
\keyword{internal}
