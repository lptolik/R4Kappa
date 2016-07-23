\name{validate.kproject}
\alias{validate.kproject}
\title{validate kproject}
\description{Prepare validation batch, write content of the \code{kproject} to the temp folder and run the project}
\usage{validate.kproject(kproject, dir = tempdir(), nrep = 2, nsets = 1, 
    save = FALSE, exe = kproject$execPath)}
\arguments{
  \item{kproject}{object to write}
  \item{dir}{optional destination directory to execute validation package}
  \item{nrep}{optional number of repetitive evaluation calls to KaSim. 2 is recommended as it allows to check proper generation and invocation of simulation package}
  \item{nsets}{optional number of test parameter sets to execute. In the case nsets > 1 validation is assumed to be a in concurrent mode.}
  \item{save}{logical which indicates wether to save results of the simulation}
  \item{exe}{optional local KaSim executable path to validate the model, if different from the execPath of the main project}
}

\value{return the output of simulation }

\author{Anatoly Sorokin}





