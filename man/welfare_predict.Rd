% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/welfare_predict.R
\name{welfare_predict}
\alias{welfare_predict}
\title{Welfare analysis}
\usage{
welfare_predict(
  project,
  mod.name,
  closures,
  betadraws = 1000,
  marg_util_income = NULL,
  income_cost = NULL,
  expected.catch = NULL,
  enteredPrice = NULL
)
}
\arguments{
\item{project}{Name of project}

\item{mod.name}{Name of selected model (mchoice)}

\item{closures}{Closure scenarios}

\item{betadraws}{Integer indicating the numer of times to run the welfare simulation. Default value is
\code{betadraws = 1000}}

\item{marg_util_income}{For conditional and zonal logit models. Name of the coefficient to use as 
marginal utility of income}

\item{income_cost}{For conditional and zonal logit models. Logical indicating whether the coefficient 
for the marginal utility of income relates to cost (\code{TRUE}) or revenue (\code{FALSE})}

\item{expected.catch}{Name of expectedchatch table to use}

\item{enteredPrice}{Price for welfare}
}
\description{
Simulate the welfare loss/gain from changes in policy or changes in other factors that influence fisher 
location choice.
}
\details{
To simulate welfare loss/gain, the model coefficients are sampled 1000 times using a multivariate random
   number generator (\code{\link{mvgrnd}}) and the welfare loss/gain for each observation is calculated (see section 9.3 
   in the user manual) for each of the sampled coefficients, and all of the estimated welfare values are saved to a file
   in the project outputs folder. \cr\cr
   Note that this function is called by \code{\link{run_policy}}.
}
