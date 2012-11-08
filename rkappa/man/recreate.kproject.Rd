\name{recreate.kproject}
\alias{recreate.kproject}
\title{recreate kproject}
\description{Function creates clone of existing project with modification according to 
values of arguments. NA values will be kept the same as original project}
\usage{recreate.kproject(project, name = NA, constantfiles = NA, templatefiles = NA, 
    paramfile = NA, exec.path = NA, shFile = NA, jobFile = NA, 
    jobCFile = NA, repReg = NA, type = project$type)}
\arguments{
  \item{project}{original project to be used as template}
  \item{name}{name for the new project}
  \item{constantfiles}{list of file names containing constant part of the model}
  \item{templatefiles}{list of template file names to be used to create model}
  \item{paramfile}{list of names for parameter file to be generated}
  \item{exec.path}{path to kappa language simulator executables in simulation environment}
  \item{shFile}{run script template file name}
  \item{jobFile}{job management job file template}
  \item{jobCFile}{job management job template file for concurrent simulations}
  \item{repReg}{regular expression to be replaced with number of parameter set}
  \item{type}{type of the project,c('parallel','concurrent','both')}
}



\author{Anatoly Sorokin}
\note{The main reason to have \dQuote{recreate} method is to have an ability to compare behaviour of various model setups. During the process of \dQuote{recreation} the only part of the project that is never change is \code{kproject$paramSet}. So updated model will be executed with the same set of parameter values make an appropriate comparisons.}




