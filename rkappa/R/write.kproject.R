write.kproject<-function(
  ###Write content of the \code{kproject} to the folder
  kproject,##<<object to write
  projectdir=kproject$name##<<optional new destination for the writing
){
  system(paste('mkdir -p',projectdir))
  shLines<-kproject$shLines[['run.sh.templ']]
  jLines<-kproject$shLines[['job.sh.templ']];
  jCLines<-kproject$shLines[['jobConc.sh.templ']];
  repReg<-kproject$replaceRegexp
  cLine<-''
  if(length(kproject$constLines)>0){
    for(i in 1:length(kproject$constLines)){
      writeLines(kproject$constLines[[i]],paste(projectdir,'/',names(kproject$constLines[i]),sep=''))
      cLine<-paste(cLine,'-i',names(kproject$constLines[i]))
      cLine<-paste(cLine,' -i ../../',names(kproject$constLines[i]),sep='')
    }
  }
  constLine<-cLine
  for(i in 1:dim(kproject$paramSets)[1]){
    cLineL<-paste('-i',paste('param.ka.',i,sep=''))
    cLineL<-paste(' -i ../../',paste('param.ka.',i,sep=''),sep='')
    #		cLineL<-''
    for(j in names(kproject$templateLines)){
      tLines<-gsub(repReg,i,kproject$templateLines[[j]])
      writeLines(tLines,paste(projectdir,'/',j,'.',i,sep=''))
      cLineL<-paste(cLineL,'-i',paste(j,'.',i,sep=''))
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
  r4kproject<-kproject
  save(r4kproject,file=paste(projectdir,'/project.Rdat',sep=''))
}
