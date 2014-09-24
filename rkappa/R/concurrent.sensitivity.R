parallel.sensitivity <-function(
  ###function calculate PRCC sensitivity coefficients of the model parameters
  res,
  ###data frame of parameter sets. Each row is a set, each column is a parameter
  obsSens,
  ###data frame of observables obtained in the model simulations 
  outName="Prot[0-9]+",
  ###name of observable to calculate sensitivity
  nboot=0
  ###number of bootstrap runs
){
  if(!require(sensitivity)){
    stop('Function is required package "sensitivity"');
  }
  grep(outName,names(obsSens))->prot
  if(length(prot)==0){
    stop(paste('There is no columns in obsSens named like "',outName,'"',sep=''))
  }
  ind<-which(diff(sapply(res,range))!=0)
  nm<-sort(unique(sub(outName,'',names(obsSens)[prot])),decreasing=TRUE)
    p<-(length(ind)-1)
    N<-dim(res)[1]
  i<-1
    if(nm[i] !=''){
    pn<-grep(nm[i],names(obsSens)[prot])
    c<-t(obsSens[,prot[pn]])
    cname<-sub(outName,sub('\\[.*\\]\\*?\\+?','',outName), names(obsSens)[prot[pn[1]]])
    prot<-prot[-pn]
    }else{
    c<-t(obsSens[,prot])
    cname<-sub(outName,sub('\\[.*\\]\\*?\\+?','',outName), names(obsSens)[prot[1]])
    }
    colnames(c)<-cname
    sens<-pcc(res[,ind],c, rank = TRUE,nboot=nboot);
    prcc<-data.frame(sc=sens$PRCC$original)
    rownames(prcc)<-rownames(sens$PRCC)
    prcc$T<-prcc$sc*sqrt((N-2-p)/(1-prcc$sc^2))
    prcc$pval<-1-pt(prcc$T,(N-2-p))
    names(prcc) <- paste(names(prcc),cname,sep='.')
  for(i in 2:length(nm)){
    if(nm[i] !=''){
    pn<-grep(nm[i],names(obsSens)[prot])
    c<-t(obsSens[,prot[pn]])
    cname<-sub(outName,sub('\\[.*\\]\\*?\\+?','',outName), names(obsSens)[prot[pn[1]]])
    prot<-prot[-pn]
    }else{
    c<-t(obsSens[,prot])
    cname<-sub(outName,sub('\\[.*\\]\\*?\\+?','',outName), names(obsSens)[prot[1]])
    }
    colnames(c)<-cname
    sens<-pcc(res[,ind],c, rank = TRUE,nboot=nboot);
    pc<-data.frame(sc=sens$PRCC$original)
    rownames(pc)<-rownames(sens$PRCC)
    pc$T<-pc$sc*sqrt((N-2-p)/(1-pc$sc^2))
    pc$pval<-1-pt(pc$T,(N-2-p))
    names(pc) <- paste(names(pc),cname,sep='.')
    prcc<-cbind(prcc,pc)
  }
  out<-list(N=N,p=p,prcc=prcc)
  class(out)<-'kappasens'
  return(out)
}

concurrent.sensitivity <-function(
###function calculate PRCC sensitivity coefficients of the model parameters
res,
###data frame of parameter sets. Each row is a set, each column is a parameter
obsSens,
###data frame of observables obtained in the model simulations 
outName="Prot[0-9]+",
###name of observable to calculate sensitivity
nboot=0
###number of bootstrap runs
){
  if(!require(sensitivity)){
    stop('Function is required package "sensitivity"');
  }
  grep(outName,names(obsSens))->prot
  if(length(prot)==0){
    stop(paste('There is no columns in obsSens named like "',outName,'"',sep=''))
  }
  ind<-which(diff(sapply(res,range))!=0)
  i<-prot[1]
  c<-as.vector(t(obsSens[,i]))
  sens<-pcc(res[,ind],c, rank = TRUE,nboot=nboot);
  p<-(length(ind)-1)
  N<-dim(res)[1]
  prcc<-data.frame(sc=sens$PRCC$original)
  rownames(prcc)<-rownames(sens$PRCC)
  prcc$T<-prcc$sc*sqrt((N-2-p)/(1-prcc$sc^2))
  prcc$pval<-dt(prcc$T,(N-2-p))
  names(prcc)<-paste(names(prcc),names(obsSens)[i],sep='.')
  for(k in 2:length(prot)){
    i<-prot[k]
    c<-as.vector(t(obsSens[,i]))
    sens<-pcc(res[,ind],c, rank = TRUE,nboot=nboot);
    pc<-data.frame(sc=sens$PRCC$original)
    rownames(pc)<-rownames(sens$PRCC)
    pc$T<-pc$sc*sqrt((N-2-p)/(1-pc$sc^2))
    pc$pval<-dt(pc$T,(N-2-p))
    names(pc)<-paste(names(pc),names(obsSens)[i],sep='.')
    prcc<-cbind(prcc,pc)
  }
  
  out<-list(N=N,p=p,prcc=prcc)
  class(out)<-'kappasens'
  return(out)
  ###list of parameter sensitivities
}
