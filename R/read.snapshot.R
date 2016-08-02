read.snapshot.kproject<-function(
###function to read and parse KaSim simulation results
kproject,##<<project to handle
dir,##<<name of the folder with project simulation results
getAll=TRUE##<< should all snapshots be read. If False only last one will be considered
) {
  if (!require(futile.logger)) {
    stop('Function is required package "futile.logger"')
    
  }
  flog.info('start')
  psets<-dir(dir,pattern='pset.*')
  f<-psets[1]
  i<-as.integer(gsub('pset','',f))
    res <- read.snap.folder(paste(dir, f, sep = '/'),getAll=getAll)
    res$Set <- i
    
    flog.info(paste('snap', 1))
    for (f in psets[-1]) {
      i<-as.integer(gsub('pset','',f))
      r <- read.snap.folder(paste(dir, f, sep = '/'),getAll=getAll)
      if (!any(is.na(r))) {
        r$Set <- i
        
        res <- rbind(res, r)
        
      }
      flog.info(paste('snap', i))
    }
  return(res)
  
}

read.snap.folder<-function(
###utulity function to read content of one \code{pset} folder
file,##<<location of the folder to read
getAll=TRUE##<< should all snapshots be read. If False only last one will be considered
){
	dir(file,pattern='try*')->tries
	res<-NA
	for(t in tries){
	  fka<-dir(paste(file,t,sep='/'),pattern='*.ka$');
	  if(getAll){
		for(f in fka){
			r<-read.snapshot(paste(file,t,f,sep='/'));
			r$Try<-t;
			if(all(is.na(res))){
				res<-r;
			}else{
				res<-rbind(res,r);
			}
		}
	  }else{
	    i<-as.integer(gsub("^.*_([0-9]+).*ka$","\\1",fka))
	    r<-read.snapshot(paste(file,t,fka[which.max(i)],sep='/'));
	    r$Try<-t;
	    res<-r;
	  }
		flog.info(paste('snap',file,t,sep='.'))
	}
	return(res);
}
read.snapshot <-function(
###function to read KaSim snapshot kappa files
file
###name of the snapshot file
){
l<-readLines(file)
N<-length(l)
dat<-data.frame(Ev=1,T=1,Num=1,Kappa='s', Brutto='bstring', Weight=1, Comp=1,stringsAsFactors =FALSE)[FALSE,]
ev.T<-as.numeric(unlist(strsplit(sub('^.*Event: +([0-9.]+), *Time: +([0-9.]+).*$','\\1,\\2',l[1]),',')))
is.Kappa<-FALSE
indx<-1
for(i in 2:N){
  	if(is.Kappa){
	  kappa<-l[i]
	  table(gsub('\\(.*$','',unlist(strsplit(kappa,'),',fixed=TRUE))))->brutto
	  bstring<-paste(c(rbind(names(brutto),paste(brutto))),collapse='.')
	  dat[indx,]<-data.frame(Ev=ev.T[1],T=ev.T[2], Num=num,Kappa=kappa,  Brutto=bstring, Weight=sum(brutto), Comp=length(brutto), stringsAsFactors =FALSE)
	  indx<-indx+1
	  is.Kappa<-FALSE
	} else if(grepl('\\\\$',l[i])){
	  num<-as.integer(sub('^.*: +([0-9]+).*','\\1',l[i]))
		is.Kappa<-TRUE
	} 

}
return(dat)

}
makeBrutto<-function(
### calculate brutto formula table from kappa complex definition string
  kappa##<<kappa complex definition string
  ){
 table(gsub('\\(.*$','',unlist(strsplit(kappa,'),',fixed=TRUE))))->brutto
 return(brutto)
}
makeBruttoStr<-function(
### create string representation of brutto formula from kappa complex definition string  
  kappa##<<kappa complex definition string
  ){
 brutto<-makeBrutto(kappa)
 bstring<-paste(c(rbind(names(brutto),paste(brutto))),collapse='.')
 return(bstring)
}

igraph.brutto.str<-function(graph){
  if(!require(igraph)){
    stop('Function is required package "igraph" version 0.6');
  }
  brutto<-table(V(graph)$type) ;
	bstring<-paste(c(rbind(names(brutto),paste(brutto))),collapse='.')
	return(bstring)
}
