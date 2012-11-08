read.observables <-function(
###function to read and parse KaSim simulation results
object,##<<object to read from
...##<<other parameters
){
UseMethod("read.observables")
}
read.observables.default <-function(
###function to read and parse KaSim simulation results
file,##<<name of the observable file
...##<<other parameters
){
l<-readLines(file)
l[1]<-sub('#','s',l[1])
top<-options(show.error.messages=FALSE)
dat<-try(read.table(header=TRUE,text=l,sep=' '))
if(inherits(dat,'try-error')){
l<-l[1:(length(l)-1)]
dat<-read.table(header=TRUE,text=l,sep=' ')
}
options(top)
return(dat)
###data frame of observables
}

read.observables.kproject<-function(
###function to read and parse KaSim simulation results
kproject,##<<project to handle
dir##<<name of the folder with project simulation results
){
	if(!require(plyr)){
         stop('Function is required package "plyr"');
	}
	if(!require(futile.logger)){
         stop('Function is required package "futile.logger"');
	}
	config_logger()
	logger<-getLogger()
	logger(INFO,'start')
	pConc<-NA
	if(kproject$type=='concurrent'|kproject$type=='both'){
		logger(INFO,'Concurrent file is reading')
		p<-read.sim.folder(paste(dir,'psetConc',sep='/'),aggregate=TRUE)
		if(length(p)>0){
			pConc<-p
#			pConc<-ddply(p, .(time), colMeans,.progress = "text")
			logger(INFO,'Concurrent is done')
			gc()
		}
	}
	pPar<-NA
	if(kproject$type=='parallel'|kproject$type=='both'){
		p<-read.sim.folder(paste(dir,'pset1',sep='/'))
		pPar<-ddply(p, .(time), colMeans)
		names(pPar)->n
		logger(INFO,paste('parallel',1))
		for(i in 2:kproject$nSets){
			p<-read.sim.folder(paste(dir,paste('pset',i,sep=''),sep='/'))
			if(length(p)>0){
				s<-ddply(p,.(time),colMeans,.progress = "text")
				if(any(names(s) %in% n)){
					names(s)[names(s) %in% n]<-paste(names(s)[names(s) %in% n],i,sep='')
				}
				if(dim(pPar)[1]!=dim(s)){
					if(dim(pPar)[1]>dim(s)[1]){
						s[dim(pPar)[1],]<-NA
					}else if(dim(pPar)[1]<dim(s)[1]){
						pPar[dim(s)[1],]<-NA
					}
				}
				pPar<-cbind(pPar,s)
				logger(INFO,paste('parallel',i))
			}
		}
	}
	kprojectSimExp<-list(project=kproject,resConc=pConc,resPar=pPar)
	class(kprojectSimExp)<-'kprojectSimExp'
	return(kprojectSimExp)
###object of \code{kprojectSimExp} class that stores both project and simulation experiment results
}

read.sim.folder<-function(
###utulity function to read content of one \code{pset} folder
file,##<<location of the folder to read
aggregate=FALSE,##<<wether aggregate data on the fly or return raw dataset
block.size=10##<<number of simulations to aggregate
){
	if(!require(plyr)){
         stop('Function is required package "plyr"');
	}
	if(!require(futile.logger)){
         stop('Function is required package "futile.logger"');
	}
	config_logger()
	logger<-getLogger()
	dir(file,pattern='try*')->tries
	res<-c();
	if(length(tries)>0){
		res<-read.observables(paste(file,tries[1],'data.out',sep='/'))
		res<-cbind(res,data.frame(N=1))
		logger(INFO,paste(file,tries[1],'data.out',sep='/'))
		block<-1;
		for(i in tries[-1]){
			r<-read.observables(paste(file,i,'data.out',sep='/'))
			if(dim(r)[1]==0) next;
			res<-rbind(res,cbind(r,
				data.frame(N=1)))
			if(aggregate){
				if(block>=block.size){
					block=0;
					logger(INFO,'aggregate start')
					res<-ddply(res,.(time),function(.x)colSums(.x[,names(.x)!='time']))
					logger(INFO,'aggregate over')
				}
			}
			block<-block+1;
			logger(INFO,paste(block,paste(file,i,'data.out',sep='/')))
		}
		if(aggregate){
			logger(INFO,'aggregate start')
			res<-ddply(res,.(time),function(.x)colSums(.x[,names(.x)!='time']))
			logger(INFO,'aggregate over')
			ind<-!names(res)%in%c('time','N')
			res[,ind]<-res[,ind]/res$N;
		}
	}
	return(res)
###data from the folder
}

accum<-function(
###utility function to calculate cumulative statistics of multiple simulations of the same parameter set. It suppose to be called from \code{ddply} function of \code{plyr} package. 
.x,templ){
	res<-templ
	for(cl in names(.x)[-(1:3)]){
		res[,cl]<-mean(.x[,cl])
		res[,paste('sd',cl,sep='.')]<-sd(.x[,cl])
	}
}
