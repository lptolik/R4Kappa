tdcc<-function(
  ###function calculates PRCC sensitivity coefficients of the model parameters
  res,
  ###data frame of parameter sets. Each row is a set, each column is a parameter
  obsSens,
  ###data frame of observables obtained in the model simulations 
  outName="Prot[0-9]+",
  ###name of observable to calculate sensitivity
  nsets=10
  ###number of points to evaluate TDCC, either integer giving the number, or vector giving the numbers at which TDCC will be evaluated 
){
  ind<-which(diff(sapply(res,range))!=0)
  p<-(length(ind)-1)
  N<-dim(res)[1]
  if(length(nsets)==1){
#    nsets=c(floor(N/nsets)*(1:(nsets-1)),N)
    nsets=round(seq(from=p+4,to=N,length.out = nsets))
  }else{
    if(any(nsets<(p+4))) 
      stop("Number of sets should be larger then p+3, where p is number of non-zero range columns in res")
  }
  l<-length(nsets)
  sm<-matrix(NA,nrow = p+1,ncol = l)
  ps<-concurrent.sensitivity(res,obsSens,outName)
  nm<-grep('sc.',names(ps$prcc))
  i<-nm[1]
  S<-.savage(p+1)
  out<-lapply(nm,function(.x)sm)
  nam<-names(ps$prcc)[nm]
  names(out)<-nam
  rt<-data.frame(N=nsets)
  for(i in length(nam)){rt[,nam[i]]<-NA}
  for(i in 1:length(nm)){
    out[[i]][,l]<-S[rank(-abs(ps$prcc[,nm[i]]))]
  }
  for(n in 1:(l-1)){
    ps<-concurrent.sensitivity(res[1:nsets[n],],obsSens[1:nsets[n],],outName)
    nm<-grep('sc.',names(ps$prcc))
    for(i in 1:length(nm)){
      out[[i]][,n]<-S[rank(-abs(ps$prcc[,nm[i]]))]
    }
  }
  for(n in l:2){
    for(i in 1:length(nm)){
      rt[n,nam[i]]<-(sum(out[[i]][,n]*out[[i]][,n-1])-p-1)/(p+1-S[1])
    }
  }
  out$rt<-rt
  return(out)
}

.savage<-function(n){
  S<-sum(1/(1:n))-c(0,cumsum(1/(1:(n-1))))
  return(S)
}