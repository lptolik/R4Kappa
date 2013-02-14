\name{parallel.sensitivity}
\alias{parallel.sensitivity}
\title{parallel sensitivity}
\description{function calculate PRCC sensitivity coefficients of the model parameters}
\usage{parallel.sensitivity(res, obsSens, outName = "Prot[0-9]+", nboot = 0)}
\arguments{
  \item{res}{data frame of parameter sets. Each row is a set, each column is a parameter}
  \item{obsSens}{data frame of observables obtained in the model simulations }
  \item{outName}{name of observable to calculate sensitivity}
  \item{nboot}{number of bootstrap runs}
}



\author{Anatoly Sorokin}





