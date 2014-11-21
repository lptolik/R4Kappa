\name{make.kproject}
\alias{make.kproject}
\title{make kproject}
\description{Creates a infrastructure required to simulate kappa model with various parameter sets 
and generate correspondent folder infrastructure}
\usage{make.kproject(project = paste("multi", format(Sys.time(), "\%Y\%m\%d\%H\%M\%S"), 
    sep = ""), numSets = 500, exec.path = "~/kasim3/KaSim", repReg = "_-", 
    type = c("parallel", "concurrent", "both"))}
\arguments{
  \item{project}{name of the project to be created, new folder will be created to contain the project 
files}
  \item{numSets}{list of parameter file names}
  \item{exec.path}{path to kappa language simulator executables in simulation environment}
  \item{repReg}{regular expression to be replaced with number of parameter set}
  \item{type}{type of the project}
}



\author{Anatoly Sorokin}





