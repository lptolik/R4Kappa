\name{prccConfidenceInterval}
\alias{prccConfidenceInterval}
\title{prccConfidenceInterval}
\description{function calulates confidence interval for a PRCC sensitivity coefficients.
Finding expression for coefficient values with Maxima
(\%i6) solve(T^2-g^2*(N-2-p)/(1-g^2),g);

T                         T
(\%o6)       [g = - --------------------, g = --------------------]
2                         2
sqrt(T  + N - p - 2)      sqrt(T  + N - p - 2)
}
\usage{prccConfidenceInterval(pval, N, p)}
\arguments{
  \item{pval}{significance levels to calculate interval at}
  \item{N}{the number of samples}
  \item{p}{the number of discarded parameters}
}

\value{data.frame with lower and upper boundaries for each significance level}

\author{Anatoly Sorokin}





