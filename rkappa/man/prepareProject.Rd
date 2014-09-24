\name{prepareProject}
\alias{prepareProject}
\title{prepareProject}
\description{Creates a infrastructure required to simulate kappa model with various parameter sets 
and generate correspondent folder infrastructure}
\usage{prepareProject(project = paste("multi", format(Sys.time(), "\%Y\%m\%d\%H\%M\%S"), 
    sep = ""), numSets = 500, pTable, constantfiles = c("main_rnap_def_rule.ka", 
    "main_rnap_init.ka", "main_rnap_param.ka"), templatefiles = c("var_prom_def.ka", 
    "var_prom_init.ka", "var_prom_rule.ka"), paramfile = c("var_prom_param.ka"), 
    k_min = 0.1, k_max = 10, exec.path = "~/kasim3/KaSim", shFile = "run.sh.templ", 
    jobFile = "job.sh.templ", jobCFile = "jobConc.sh", repReg = "_-", 
    type = c("parallel", "concurrent", "both"), writeDir = FALSE)}
\arguments{
  \item{project}{name of the project to be created, new folder will be created to contain the project 
files}
  \item{numSets}{number of parameter sets to be generated}
  \item{pTable}{Parameter ranges data frame. Should contain columns \code{param} with parameter names, 
\code{Min} and \code{Max} with parameter ranges. Names in \code{param} column should 
match names in the content of \code{paramfile} files.}
  \item{constantfiles}{list of file names containing constant part of the model }
  \item{templatefiles}{list of template file names to be used to create model}
  \item{paramfile}{list of parameter file names}
  \item{k_min}{minimum parameter value factor to be used if \code{pTable==NA}}
  \item{k_max}{maximum parameter value factor to be used if \code{pTable==NA}}
  \item{exec.path}{path to kappa language simulator executables in simulation environment}
  \item{shFile}{run script template file name}
  \item{jobFile}{job management job file template}
  \item{jobCFile}{job management job template file for concurrent simulations}
  \item{repReg}{regular expression to be replaced with number of parameter set}
  \item{type}{type of the project}
  \item{writeDir}{if TRUE project directory and its content will be created}
}

\value{project object}

\author{Anatoly Sorokin}





