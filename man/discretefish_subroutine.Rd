% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discretefish_subroutine.R
\name{discretefish_subroutine}
\alias{discretefish_subroutine}
\title{Run discrete choice model}
\usage{
discretefish_subroutine(
  project,
  run = "new",
  select.model = FALSE,
  explorestarts = TRUE,
  max.iterations = 500,
  breakearly = TRUE,
  space = NULL,
  dev = NULL,
  use.scalers = FALSE,
  scaler.func = NULL,
  CV = FALSE
)
}
\arguments{
\item{project}{String, name of project.}

\item{run}{String, how models should be run. \code{'new'} will only run models
that exist in the model design file but not in the model output table. \code{'all'}
will run all models in the model design file, replacing existing model output. 
The third option is to enter a vector of model names to run (use 
\code{model_names()} to see current model names). 
If the specified model already has output it will be replaced.}

\item{select.model}{Return an interactive data table that allows users to 
select and save table of best models based on measures of fit.}

\item{explorestarts}{Logical, should starting parameters value space be explored? 
Set to \code{TRUE} if unsure of the number of starting parameter values to 
include or of reasonable starting parameters values. Better starting parameter 
values can help with model convergence.}

\item{max.iterations}{If \code{explorestarts = TRUE}, max.iterations indicates the
maximum number of iterations to run in search of valid starting parameter values.
If the maximum is reached before valid parameter values are found (i.e., likelihood 
= Inf) the loop will terminate and an error message will be reported for that model.}

\item{breakearly}{Logical, if \code{explorestarts = TRUE}, should the first set 
of starting parameter values that returns a valid (numeric) loglikelihood 
value be returned (\code{TRUE}) or should the entire parameter space be considered 
and the set of starting parameter values that return the lowest loglikelihood 
value be returned (\code{FALSE}).}

\item{space}{Specify if \code{explorestarts = TRUE}. List of length 1 or length 
equal to the number of models to be evaluated. \code{space} is the number of 
starting value permutations to test (the size of the space to explore). The 
greater the \code{dev} argument, the larger the \code{space} argument should be.}

\item{dev}{Specify if \code{explorestarts = TRUE}. List of length 1 or length 
equal to the number of models to be evaluated. \code{dev} refers to how far 
to deviate from the average parameter values when exploring (random normal 
deviates). The less certain the average parameters are, the greater the 
\code{dev} argument should be.}

\item{use.scalers}{Logical, should data be normalized? Defaults to \code{FALSE}. 
Rescaling factors are the mean of the numeric vector unless specified with 
\code{scaler.func}.}

\item{scaler.func}{Function to calculate rescaling factors. Can be a generic 
function, such as mean, or a user-defined function. User-defined functions 
must be specified as \code{scaler.fun = function(x, FUN = sd) 2*FUN(x)}. 
This example returns two times the standard deviation of \code{x}.}

\item{CV}{Logical, \code{CV = TRUE} when running \code{discretefish_subroutine} for k-fold cross validation,
and the default value is \code{CV = FALSE}.}
}
\value{
\tabular{rlll}{
  OutLogit: \tab [outmat1 se1 EPM2] (coefs, ses, tstats) \cr
  optoutput: \tab optimization information \cr
  seoumat2: \tab ses \cr
  MCM: \tab Model Comparison metrics \cr
}
}
\description{
Subroutine to run chosen discrete choice model. Function pulls necessary data 
generated in \code{\link{make_model_design}} and loops through model design 
choices and expected catch cases. Output is saved to the FishSET database.
}
\details{
Runs through model design choices generated by \code{\link{make_model_design}} 
  and stored as `ModelInputData` in FishSET database.  Data matrix is created 
  in \code{\link{create_model_input}}. Required data, optional data, and details 
  on likelihood functions are outlined in \code{\link{make_model_design}}. \cr\cr

Likelihood-specific initial parameter estimates: \cr

\enumerate{
 \item{\strong{Conditional logit likelihood (\code{\link{logit_c}})}\cr
   Starting parameter values takes the order of: c([alternative-specific parameters],  [travel-distance parameters]).
   The alternative-specific parameters and travel-distance parameters are of length (# of alternative-specific variables) and (# of
   travel-distance variables) respectively.} 
 \item{\strong{Zonal logit with area specific constants (\code{\link{logit_zonal}})} \cr
   Starting parameters takes the order of: c([average-catch parameters], [travel-distance parameters]).
   The average-catch and travel-distance parameters are of length (# of average-catch variables)*(k-1) and (# of travel-distance variables)
   respectively, where (k) equals the number of alternative fishing choices.}
 \item{\strong{Full information model with Dahl's correction function (\code{\link{logit_correction}})} \cr
   Starting parameter values takes the order of: c([marginal utility from catch], [catch-function parameters],
   [polynomial starting parameters], [travel-distance parameters], [catch sigma]).
   The number of polynomial interaction terms is currently set to 2, so given the chosen degree 'polyn' there should be
   \code{"(((polyn+1)*2)+2)*(k)"} polynomial starting parameters, where (k) equals the number of alternative fishing choices.
   The marginal utility from catch and catch sigma are of length equal to unity respectively. The catch-function and
   travel-distance parameters are of length (# of catch variables)*(k) and (# of cost variables) respectively.}
 \item{\strong{Expected profit model with normal catch function (\code{\link{epm_normal}})} \cr
   Starting parameters values take the order of: c([catch-function parameters], [travel-distance parameters], [catch sigma(s)], [scale parameter]).
   The catch-function and travel-distance parameters are of length (# of catch-function variables)*(k) and (# of travel-distance
   variables) respectively, where (k) equals the number of alternative fishing choices. The catch sigma(s) are either of length equal
   to unity or length (k) if the analyst is estimating location-specific catch sigma parameters. The scale parameter is of length
   equal to unity.}
 \item{\strong{Expected profit model with Weibull catch function (\code{\link{epm_weibull}})} \cr
   Starting parameter values takes the order of: c([catch-function parameters], [travel-distance parameters], [catch sigma(s)], [scale parameter]).
   The catch-function and travel-distance parameters are of length (# of catch-function variables)*(k) and (# of travel-distance variables)
   respectively, where (k) equals the number of alternative fishing choices. The catch sigma(s) are either of length equal to unity or
   length (k) if the analyst is estimating location-specific catch sigma parameters. The scale parameter is of length equal to unity.}
 \item{\strong{Expected profit model with log-normal catch function (\code{\link{epm_lognormal}})} \cr
   Starting parameter values takes the order of: c([catch-function parameters], [travel-distanceparameters],
   [catch sigma(s)], [scale parameter]).
   The catch-function and travel-distance parameters are of length (# of
   catch-function variables)*(k) and (# of travel-distance variables) respectively, where (k) equals the number of alternative fishing choices.
   The catch sigma(s) are either of length equal to unity or length (k) if the analyst is estimating location-specific catch sigma parameters.
   The scale parameter is of length equal to unity.}  
}
   \cr
   
   Model output are saved to the FishSET database and can be loaded to the console with:
   \tabular{rlll}{
   \code{\link{model_out_view}}: \tab model output including optimization information, standard errors, coefficients, and t- statistics. \cr
   \code{\link{model_params}}: \tab model estimates and standard error \cr
   \code{\link{model_fit}}: \tab model comparison metrics \cr
   \code{\link{globalcheck_view}}: \tab  model error message \cr
     }
   For obtaining catch, choice, distance, and otherdat data generated from \code{make_model_design} function.
   \code{ModelInputData} table will be pulled from FishSET database.
}
\examples{
\dontrun{
results <- discretefish_subroutine("pcod", run = 'all', select.model = TRUE)
}

}
