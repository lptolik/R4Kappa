\name{validate.kproject}
\alias{validate.kproject}
\title{validate kproject}
\description{Prepare validate batch, write content of the \code{kproject} to the temp folder and run the project}
\usage{validate.kproject(kproject, dir = tempdir(), exe = kproject$execPath)}
\arguments{
  \item{kproject}{object to write}
  \item{dir}{optional new destination for the writing}
  \item{exe}{optional local KaSim executable path to validate the model, if different from the execPath of the main project}
}

\value{return the output of simulation }

\author{Anatoly Sorokin}





