\name{getSmoothTS}
\alias{getSmoothTS}
\title{getSmoothTS}
\description{utility function to calculate smoothed version of time series observables}
\usage{getSmoothTS(dat, timeC = "time", nC = "N", dataC = setdiff(names(dat), 
    c("time", "N")))}
\arguments{
  \item{dat}{data.frame with oblervables for paramset in long format (i.e. try sets stacked together)}
  \item{timeC}{name of the time column}
  \item{nC}{name of try number column}
  \item{dataC}{names of the columns to make smoothing over}
}



\author{Anatoly Sorokin}





