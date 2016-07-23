\name{prepare.validation.project}
\alias{prepare.validation.project}
\title{prepare validation project}
\description{Prepare project with validation batch}
\usage{prepare.validation.project(kproject, nrep = 2, nsets = 1, exe = kproject$execPath)}
\arguments{
  \item{kproject}{object to write}
  \item{nrep}{optional number of repetitive evaluation calls to KaSim. 2 is recommended as it allows to check proper generation and invocation of simulation package}
  \item{nsets}{optional number of test parameter sets to execute. In the case nsets > 1 validation is assumed to be a in concurrent mode.}
  \item{exe}{optional local KaSim executable path to validate the model, if different from the execPath of the main project}
}

\value{return the validation project}

\author{Anatoly Sorokin}





