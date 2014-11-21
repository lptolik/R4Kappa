read.snapshot.kproject<-function(
###function to read and parse KaSim simulation results
kproject,##<<project to handle
dir##<<name of the folder with project simulation results
){
	if(!require(futile.logger)){
         stop('Function is required package "futile.logger"');
	}
	config_logger()
	logger<-getLogger()
	flog.info('start')
	res<-read.snap.folder(paste(dir,'pset1',sep='/'))
	res$Set<-1;
	flog.info(paste('snap',1))
	for(i in 2:kproject$nSets){
		r<-read.snap.folder(paste(dir,paste('pset',i,sep=''),sep='/'))
		if(!is.na(r)){
			r$Set<-i;
			res<-rbind(res,r);
		}
	flog.info(paste('snap',i))
	}
	return(res);
}
read.snap.folder<-function(
###utulity function to read content of one \code{pset} folder
file##<<location of the folder to read
){
	dir(file,pattern='try*')->tries
	res<-NA
	for(t in tries){
		fka<-dir(paste(file,t,sep='/'),pattern='*.ka$');
		for(f in fka){
			r<-read.snapshot(paste(file,t,f,sep='/'));
			r$Try<-t;
			if(all(is.na(res))){
				res<-r;
			}else{
				res<-rbind(res,r);
			}
		}
		flog.info(paste('snap',t,sep='.'))
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
for(i in 1:(N/2-1)){
  num<-as.integer(sub('^.*: +([0-9]+).*','\\1',l[2*i]))
  kappa<-l[2*i+1]
  table(gsub('\\(.*$','',unlist(strsplit(kappa,'),',fixed=TRUE))))->brutto
  bstring<-paste(c(rbind(names(brutto),paste(brutto))),collapse='.')
  dat[i,]<-data.frame(Ev=ev.T[1],T=ev.T[2], Num=num,Kappa=kappa,  Brutto=bstring, Weight=sum(brutto), Comp=length(brutto), stringsAsFactors =FALSE)
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
