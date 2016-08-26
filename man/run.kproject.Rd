\name{run.kproject}
\alias{run.kproject}
\title{run kproject}
\description{Prepare content of the \code{kproject} to the temp folder and run particular paramset}
\usage{run.kproject(kproject, dir = tempdir(), nrep = 10, ntime = 10, 
    nset = 1, save = FALSE, exe = kproject$execPath)}
\arguments{
  \item{kproject}{object to write}
  \item{dir}{optional destination directory to execute validation package}
  \item{nrep}{optional number of repetitive evaluation calls to KaSim. 10 is recommended as it allows to check proper generation and invocation of simulation package}
  \item{ntime}{optional number of repetitive evaluation calls to KaSim. 10 is recommended as it allows to check proper generation and invocation of simulation package}
  \item{nset}{optional index of the parameter set to execute}
  \item{save}{logical which indicates wether to save results of the simulation}
  \item{exe}{optional local KaSim executable path to validate the model, if different from the execPath of the main project}
}

\value{return the output of simulation }

\author{Anatoly Sorokin}





