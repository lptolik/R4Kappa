\name{significancePVal}
\alias{significancePVal}
\title{significancePVal}
\description{function calculates significance level of the PRCC sensitivity coefficients. 
At the moment there is no correction for multiple testing in significance calculation.
This feature will be added in the next version of the software.}
\usage{significancePVal(gamma, N, p)}
\arguments{
  \item{gamma}{the PRCC coefficient}
  \item{N}{the number of samples}
  \item{p}{the number of discarded parameters}
}

\value{list with T-statistics value and Pvalue.}

\author{Anatoly Sorokin}





