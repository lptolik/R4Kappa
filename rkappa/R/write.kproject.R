write.kproject<-function(
  ###Write content of the \code{kproject} to the folder
  kproject,##<<object to write
  projectdir=kproject$name##<<optional new destination for the writing
){
  system(paste('mkdir -p',projectdir))
  rep<-function(pname,lines){return(replaceProperty(kproject,pname,lines))}
  shLines<-kproject$shLines[['run.sh.templ']]
  shLines<-rep('nRep',
               rep('execPath',shLines))
  jLines<-kproject$shLines[['job.sh.templ']];
  jLines<-rep('nRep',
               rep('execPath',jLines))
  jCLines<-kproject$shLines[['jobConc.sh.templ']];
  jCLines<-rep('nRep',
               rep('execPath',jCLines))
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
      if(!is.na(repReg)){
        tLines<-gsub(repReg,i,kproject$templateLines[[j]])
      }else{
        tLines<-kproject$templateLines[[j]]
      }
      writeLines(tLines,paste(projectdir,'/',j,'.',i,sep=''))
      cLineL<-paste(cLineL,' -i ../../',paste(j,'.',i,sep=''),sep='')
    }
    #		browser()
    pLines<-c(paste('#parameters for set',i))
    for(k in 1:dim(kproject$pTable)[1]){
      if(!is.na(repReg)){
      pLines[k+1]<-paste("%var: '",gsub(repReg,i,kproject$pTable[k,'name']),"' ",kproject$paramSets[i,k],sep='')
      }else{
        pLines[k+1]<-paste("%var: '",kproject$pTable[k,'name'],"' ",kproject$paramSets[i,k],sep='')        
      }
    }
    writeLines(pLines,paste(projectdir,'/param.ka.',i,sep=''))
    cLine<-paste(cLine,cLineL)
    if(kproject$type=='parallel'|kproject$type=='both'){
      shLinesL<-replaceValue('setIndx',i,
                             replaceValue('inputs',paste(constLine,cLineL),shLines))
      writeLines(shLinesL,paste(projectdir,'/run',i,'.sh',sep=''))
      system(paste('chmod a+x ',projectdir,'/run',i,'.sh',sep=''))
    }
  }
  if(kproject$type=='concurrent'|kproject$type=='both'){
    shLines<-replaceValue('setIndx','Conc',
                          replaceValue('inputs',cLine,shLines))
    writeLines(shLines,paste(projectdir,'/runConc.sh',sep=''))
    system(paste('chmod a+x ',projectdir,'/runConc.sh',sep=''))
    writeLines(jCLines,
               paste(projectdir,'/jobConc.sh',sep=''))
    system(paste('chmod a+x ',projectdir,'/jobConc.sh',sep=''))
  }
  if(kproject$type=='parallel'|kproject$type=='both'){
    writeLines(jLines,
               paste(projectdir,'/job.sh',sep=''))
    system(paste('chmod a+x ',projectdir,'/job.sh',sep=''))
  }
  if(!is.null(kproject$shLines$validate)){
    shLines<-replaceValue('setIndx','Val',
                          replaceValue('inputs',cLine,
                                       rep('nRep',
                                           rep('execPath',kproject$shLines$validate))))
    writeLines(shLines,paste(projectdir,'/validate.sh',sep=''))
    system(paste('chmod a+x ',projectdir,'/validate.sh',sep=''))
  }
  r4kproject<-kproject
  save(r4kproject,file=paste(projectdir,'/project.Rdat',sep=''))
}

replaceProperty<-function(
  ###Replace named parameter with project property
  kproject,##<<project to take property from
  pname,##<<name of the property to substitute with value
  lines##<<string vector to make substitution in
){
  if("character"!=class(lines)) stop('Lines should be a vector of characters')
  if(length(kproject[[pname]])==0) stop(paste('There is no property "',pname,'" in the project, or its length==0',sep=''))
  else if(length(kproject[[pname]])>1) warning(paste('Length of the property "',pname,'" is greater than 1, only first element  is used',sep=''))
  return(replaceValue(pname,kproject[[pname]][1],lines))
}

replaceValue<-function(
  ###Replace named parameter with value
  pname,##<<name of the property to substitute with value
  value,##<<value to replace with
  lines##<<string vector to make substitution in
){
  return(gsub(paste("%",pname,"%",sep=''),value,lines))
}