prepare.validation.project<-function(
  ###Prepare project with validation batch
  kproject,##<<object to write
  nrep=2,##<<optional number of repetitive evaluation calls to KaSim. 2 is recommended as it allows to check proper generation and invocation of simulation package
  nsets=1,##<<optional number of test parameter sets to execute. In the case nsets > 1 validation is assumed to be a in concurrent mode.
  exe=kproject$execPath##<<optional local KaSim executable path to validate the model, if different from the execPath of the main project
){
  vproject<-kproject
  vLines<-vproject$shLines[['run.sh.templ']]
  #  emptylines<-which(grepl('^\\s*$',vLines))
  #  vLines[emptylines[1]]<-"export KASIM_EXE=%execPath%"
  vproject$shLines$validate<-vLines
  vproject$execPath=exe
  vproject$nRep<-nrep
  vproject$nSets<-nsets
  return(vproject)
  ###return the validation project
}

validate.kproject<-function(
  ###Prepare validation batch, write content of the \code{kproject} to the temp folder and run the project
  kproject,##<<object to write
  dir=tempdir(),##<<optional destination directory to execute validation package
  nrep=2,##<<optional number of repetitive evaluation calls to KaSim. 2 is recommended as it allows to check proper generation and invocation of simulation package
  nsets=1,##<<optional number of test parameter sets to execute. In the case nsets > 1 validation is assumed to be a in concurrent mode.
  save=FALSE,##<<logical which indicates wether to save results of the simulation
  exe=kproject$execPath##<<optional local KaSim executable path to validate the model, if different from the execPath of the main project
){
  vproject<-prepare.validation.project(kproject,nrep,nsets,exe)
  write.kproject(vproject,dir)
  cwd<-getwd()
  setwd(dir)
  system2('./validate.sh',args=c(nrep,0.01),stderr=TRUE,stdout=TRUE)->out
  setwd(cwd)
  if(!save){
    system2('rm',args=c('-rf',dir),stderr=FALSE,stdout=FALSE,wait=FALSE)
  }
  return(out)
  ###return the output of simulation 
}

run.kproject<-function(
  ###Prepare content of the \code{kproject} to the temp folder and run particular paramset
  kproject,##<<object to write
  dir=tempdir(),##<<optional destination directory to execute validation package
  nrep=10,##<<optional number of repetitive evaluation calls to KaSim. 10 is recommended as it allows to check proper generation and invocation of simulation package
  ntime=10,##<<optional number of repetitive evaluation calls to KaSim. 10 is recommended as it allows to check proper generation and invocation of simulation package
  nset=1,##<<optional index of the parameter set to execute
  save=FALSE,##<<logical which indicates wether to save results of the simulation
  exe=kproject$execPath##<<optional local KaSim executable path to validate the model, if different from the execPath of the main project
){
  write.kproject(kproject,dir)
  cwd<-getwd()
  setwd(dir)
  system2(paste0('./run',nset,'.sh'),args=c(nrep,ntime),stderr=TRUE,stdout=TRUE)->out
  respr<-read.observables.kproject(kproject = kproject,dir = dir)
  respr$simout<-out
  setwd(cwd)
  if(!save){
    system2('rm',args=c('-rf',dir),stderr=FALSE,stdout=FALSE,wait=FALSE)
  }
  return(respr)
  ###return the output of simulation 
}

