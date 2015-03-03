write.kproject<-function(
  ###Write content of the \code{kproject} to the folder
  kproject,##<<object to write
  projectdir=kproject$name##<<optional new destination for the writing
){
  system(paste('mkdir -p',projectdir))
  shLines<-gsub('numEv=[0-9]+',paste('numEv=',kproject$nRep,sep=''),kproject$shLines[['run.sh.templ']])
  jLines<-kproject$shLines[['job.sh.templ']];
  jCLines<-kproject$shLines[['jobConc.sh.templ']];
  repReg<-kproject$replaceRegexp
  cLine<-''
  if(length(kproject$constLines)>0){
    for(i in 1:length(kproject$constLines)){
      writeLines(kproject$constLines[[i]],paste(projectdir,'/',names(kproject$constLines[i]),sep=''))
      cLine<-paste(cLine,' -i ../../',names(kproject$constLines[i]),sep='')
    }
  }
  constLine<-cLine
  if(kproject$nSets<dim(kproject$paramSets)[1]){
    warning("nSets is less than a number of parameter sets, not all sets will be written down!!!")
  }else if(kproject$nSets>dim(kproject$paramSets)[1]){
    stop("nSets is larger than a number of parameter sets!!! Create more parameter sets with 'addSets' function and try again")
  }
  for(i in 1:kproject$nSets){
    cLineL<-paste(' -i ../../',paste('param.ka.',i,sep=''),sep='')
    #		cLineL<-''
    for(j in names(kproject$templateLines)){
      tLines<-gsub(repReg,i,kproject$templateLines[[j]])
      writeLines(tLines,paste(projectdir,'/',j,'.',i,sep=''))
      cLineL<-paste(cLineL,' -i ../../',paste(j,'.',i,sep=''),sep='')
    }
    #		browser()
    pLines<-c(paste('#parameters for set',i))
    for(k in 1:dim(kproject$pTable)[1]){
      pLines[k+1]<-paste("%var: '",gsub(repReg,i,kproject$pTable[k,'name']),"' ",kproject$paramSets[i,k],sep='')
    }
    writeLines(pLines,paste(projectdir,'/param.ka.',i,sep=''))
    cLine<-paste(cLine,cLineL)
    if(kproject$type=='parallel'|kproject$type=='both'){
      shLinesL<-gsub('\\*\\*\\*',i,gsub(repReg,paste(constLine,cLineL),shLines))
      writeLines(shLinesL,paste(projectdir,'/run',i,'.sh',sep=''))
      system(paste('chmod a+x ',projectdir,'/run',i,'.sh',sep=''))
    }
  }
  if(kproject$type=='concurrent'|kproject$type=='both'){
    shLines<-gsub('\\*\\*\\*','Conc',gsub(repReg,cLine,shLines))
    writeLines(shLines,paste(projectdir,'/runConc.sh',sep=''))
    system(paste('chmod a+x ',projectdir,'/runConc.sh',sep=''))
    writeLines(gsub('KKKKKK',kproject$execPath,jCLines),
               paste(projectdir,'/jobConc.sh',sep=''))
    system(paste('chmod a+x ',projectdir,'/jobConc.sh',sep=''))
  }
  if(kproject$type=='parallel'|kproject$type=='both'){
    writeLines(gsub('KKKKKK',kproject$execPath,jLines),
               paste(projectdir,'/job.sh',sep=''))
    system(paste('chmod a+x ',projectdir,'/job.sh',sep=''))
  }
  if(!is.null(kproject$shLines$validate)){
    shLines<-gsub('KKKKKK',kproject$execPath,gsub('\\*\\*\\*','Val',gsub(repReg,cLine,kproject$shLines$validate)))
    writeLines(shLines,paste(projectdir,'/validate.sh',sep=''))
    system(paste('chmod a+x ',projectdir,'/validate.sh',sep=''))
  }
  r4kproject<-kproject
  save(r4kproject,file=paste(projectdir,'/project.Rdat',sep=''))
}

prepare.validation.project<-function(
  ###Prepare project with validation batch
  kproject,##<<object to write
  nrep=2,##<<optional number of repetitive evaluation calls to KaSim. 2 is recommended as it allows to check proper generation and invocation of simulation package
  nsets=1,##<<optional number of test parameter sets to execute. In the case nsets > 1 validation is assumed to be a in concurrent mode.
  exe=kproject$execPath##<<optional local KaSim executable path to validate the model, if different from the execPath of the main project
){
  vproject<-kproject
  vLines<-vproject$shLines[['run.sh.templ']]
  vLines[13]<-"export KASIM_EXE=KKKKKK"
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
  exe=kproject$execPath##<<optional local KaSim executable path to validate the model, if different from the execPath of the main project
){
  vproject<-prepare.validation.project(kproject,nrep,nsets,exe)
  write.kproject(vproject,dir)
  cwd<-getwd()
  setwd(dir)
  system2('./validate.sh',args=c(nrep,0.01),stderr=TRUE,stdout=TRUE)->out
  setwd(cwd)
  return(out)
  ###return the output of simulation 
}

