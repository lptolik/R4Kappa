\name{levelplot.point.sensitivity}
\alias{levelplot.point.sensitivity}
\title{levelplot point sensitivity}
\description{Function to plot sensitivity valueso of several experiments with levelplot}
\usage{levelplot.point.sensitivity(sens, pval = NA, npar = 30, conf.value = 0.05, 
    main, cnames = NA, b1 = NA, ...)}
\arguments{
  \item{sens}{data frame of sensitivity coefficients with experiments on the columns and parameters on the rows}
  \item{pval}{conf.value is ignored and npar parameters with greatest absolute values in the first column is plotted.
data frame of P-values with experiments on the columns and parameters on the rows. If NA then}
  \item{npar}{number of best parameters to show}
  \item{conf.value}{confidence value to}
  \item{main}{title of the plot}
  \item{cnames}{column names to be used on the X axis of the plot, if NA use column names}
  \item{b1}{color palette to be used with plot. By default 'RdGy' is used}
  \item{\dots}{
}
}



\author{Anatoly Sorokin}
\note{in the case that npar value is less that the number of parameters within confidence interval defined or there is no pval 
provided, levelplot of npar best parameters will be plotted. }




