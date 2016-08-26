\name{concurrent.sensitivity}
\alias{concurrent.sensitivity}
\title{concurrent sensitivity}
\description{function calculates PRCC sensitivity coefficients of the model parameters}
\usage{concurrent.sensitivity(res, obsSens, outName = "Prot[0-9]+", 
    nboot = 0)}
\arguments{
  \item{res}{data frame of parameter sets. Each row is a set, each column is a parameter}
  \item{obsSens}{data frame of observables obtained in the model simulations }
  \item{outName}{name of observable to calculate sensitivity}
  \item{nboot}{number of bootstrap runs}
}

\value{list of parameter sensitivities}

\author{Anatoly Sorokin}





