\name{timed.concurrent.sensitivity}
\alias{timed.concurrent.sensitivity}
\title{timed concurrent sensitivity}
\description{function calculate PRCC sensitivity coefficients of the model parameters along whole timeline of the model execution,}
\usage{timed.concurrent.sensitivity(res, obsSens, outName = "Prot[0-9]+", 
    nboot = 0, plot = FALSE)}
\arguments{
  \item{res}{data frame of parameter sets. Each row is a set, each column is a parameter}
  \item{obsSens}{data frame of observables obtained in the model simulations}
  \item{outName}{name of observable to calculate sensitivity}
  \item{nboot}{number of bootstrap runs}
  \item{plot}{flag should be true, if graphical representation is required}
}

\value{data frame of parameter PRCC coefficients at each time point}

\author{Anatoly Sorokin}





