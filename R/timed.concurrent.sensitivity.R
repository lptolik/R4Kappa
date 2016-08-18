timed.parallel.sensitivity <-function(
  ###function calculate PRCC sensitivity coefficients of the model parameters along whole timeline of the model execution,
  res,##<<data frame of parameter sets. Each row is a set, each column is a parameter
  obsSens,##<<data frame of observables obtained in the model simulations 
  outName="Prot[0-9]+",##<<name of observable to calculate sensitivity
  nboot=0,##<<number of bootstrap runs
  plot=FALSE##<<flag should be true, if graphical representation is required
){
  return(timed.concurrent.sensitivity(res=res,obsSens=obsSens,outName=outName,nboot=nboot,plot=plot))
}
  timed.concurrent.sensitivity <-function(
    ###function calculate PRCC sensitivity coefficients of the model parameters along whole timeline of the model execution,
    res,##<<data frame of parameter sets. Each row is a set, each column is a parameter
    obsSens,##<<data frame of observables obtained in the model simulations 
    outName="Prot[0-9]+",##<<name of observable to calculate sensitivity
    nboot=0,##<<number of bootstrap runs
    plot=FALSE##<<flag should be true, if graphical representation is required
  ){
    timedSC<-data.frame(time=0,param='',value=0,pval=0)[FALSE,]
  for(i in unique(obsSens$time)){
    cs0<-concurrent.sensitivity(res,obsSens[obsSens$time==i,],outName,nboot)
    timedSC<-rbind(timedSC,cbind(data.frame(time=i,param = rownames(cs0$prcc)),cs0$prcc))
    cat(paste(i,'\n'))
  }
  attr(timedSC,'N')<-cs0$N
  attr(timedSC,'p')<-cs0$p
  return(timedSC);
  ###data frame of parameter PRCC coefficients at each time point
}
