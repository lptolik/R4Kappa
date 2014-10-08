parallel.sensitivity <-function(
  ###function calculates PRCC sensitivity coefficients of the model parameters
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
    sig<-significancePVal(prcc$sc,N,p)
    prcc$T<-sig$T
    prcc$pval<-sig$pval
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
    sig<-significancePVal(pc$sc,N,p)
    pc$T<-sig$T
    pc$pval<-sig$pval
    names(pc) <- paste(names(pc),cname,sep='.')
    prcc<-cbind(prcc,pc)
  }
  out<-list(N=N,p=p,prcc=prcc)
  class(out)<-'kappasens'
  return(out)
}

significancePVal<-function(
###function calculates significance level of the PRCC sensitivity coefficients. 
### At the moment there is no correction for multiple testing in significance calculation.
### This feature will be added in the next version of the software.
gamma, ##<< the PRCC coefficient
N, ##<< the number of samples
p ##<< the number of discarded parameters
){
    T<-gamma*sqrt((N-2-p)/(1-gamma^2))
    pval<-2*(1-pt(abs(T),(N-2-p)))
    return(list(T=T,pval=pval))
    ###list with T-statistics value and Pvalue.
}

prccConfidenceInterval<-function(
###function calulates confidence interval for a PRCC sensitivity coefficients.
### Finding expression for coefficient values with Maxima
###(%i6) solve(T^2-g^2*(N-2-p)/(1-g^2),g);
###
###                            T                         T
###(%o6)       [g = - --------------------, g = --------------------]
###                         2                         2
###                   sqrt(T  + N - p - 2)      sqrt(T  + N - p - 2)
###
pval,##<< significance levels to calculate interval at
N, ##<< the number of samples
p ##<< the number of discarded parameters
){
    res<-data.frame(pval=pval,lower=-Inf,upper=Inf)
    tval<-qt(pval/2,(N-2-p))
    g<-abs(tval/sqrt(tval^2+N-p-2))
    res$lower=-g
    res$upper=g
    return(res)
    ### data.frame with lower and upper boundaries for each significance level
}


concurrent.sensitivity <-function(
###function calculates PRCC sensitivity coefficients of the model parameters
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
  sig<-significancePVal(prcc$sc,N,p)
  prcc$T<-sig$T
  prcc$pval<-sig$pval
  names(prcc)<-paste(names(prcc),names(obsSens)[i],sep='.')
  for(k in 2:length(prot)){
    i<-prot[k]
    c<-as.vector(t(obsSens[,i]))
    sens<-pcc(res[,ind],c, rank = TRUE,nboot=nboot);
    pc<-data.frame(sc=sens$PRCC$original)
    rownames(pc)<-rownames(sens$PRCC)
    sig<-significancePVal(pc$sc,N,p)
    pc$T<-sig$T
    pc$pval<-sig$pval
    names(pc)<-paste(names(pc),names(obsSens)[i],sep='.')
    prcc<-cbind(prcc,pc)
  }
  
  out<-list(N=N,p=p,prcc=prcc)
  class(out)<-'kappasens'
  return(out)
  ###list of parameter sensitivities
}
