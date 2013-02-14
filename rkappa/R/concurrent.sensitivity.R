concurrent.sensitivity <-function(
  ###function calculate PRCC sensitivity coefficients of the model parameters
  res,
  ###data frame of parameter sets. Each row is a set, each column is a parameter
  obsSens,
  ###data frame of observables obtained in the model simulations 
  time=max(obsSens$time),
  ###time point at which sensitiviti is going to be calculated
  outName="Prot[0-9]+",
  ###name of observable to calculate sensitivity
  nboot=0
  ###number of bootstrap runs
){
  return(concurrent.sensitivity(res=res,obsSens=obsSens,time=time,outName=outName,nboot=nboot))
}
concurrent.sensitivity <-function(
###function calculate PRCC sensitivity coefficients of the model parameters
res,
###data frame of parameter sets. Each row is a set, each column is a parameter
obsSens,
###data frame of observables obtained in the model simulations 
time=max(obsSens$time),
###time point at which sensitiviti is going to be calculated
outName="Prot[0-9]+",
###name of observable to calculate sensitivity
nboot=0
###number of bootstrap runs
){
       if(!require(sensitivity)){
         stop('Function is required package "sensitivity"');
       }
grep(outName,names(obsSens))->prot
c<-as.vector(t(obsSens[obsSens$time==time,prot]))
ind<-which(diff(sapply(res,range))!=0)
sens<-pcc(res[,ind],c, rank = TRUE,nboot=nboot);
p<-(length(ind)-1)
N<-dim(res)[1]
prcc<-data.frame(sc=sens$PRCC$original)
rownames(prcc)<-rownames(sens$PRCC)
prcc$T<-prcc$sc*sqrt((N-2-p)/(1-prcc$sc^2))
prcc$pval<-dt(prcc$T,(N-2-p))
out<-list(N=N,p=p,prcc=prcc)
class(out)<-'kappasens'
return(out)
###list of parameter sensitivities
}
