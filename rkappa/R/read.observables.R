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
  top<-options(show.error.messages=FALSE)
  l<-try(readLines(file))
  if(!inherits(l,'try-error')){
    
    l[1]<-sub('#','',l[1])
    l<-sub('^ *','',l)
    dat<-try(read.table(header=TRUE,text=l,sep=' ',na.strings = "NAN"))
    if(inherits(dat,'try-error')){
      l<-l[1:(length(l)-1)]
      dat<-read.table(header=TRUE,text=l,sep=' ',na.strings = "NAN")
    }
  }else{
    dat<-data.frame()
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
	flog.threshold(WARN)
	flog.info("This one won't")
	pConc<-NA
	if(kproject$type=='concurrent'|kproject$type=='both'){
		flog.info('Concurrent file is reading')
		p<-read.sim.folder(paste(dir,'psetConc',sep='/'),aggregate=TRUE)
		if(length(p)>0){
			pConc<-p
#			pConc<-ddply(p, .(time), colMeans,.progress = "text")
			flog.info('Concurrent is done')
			gc()
		}
	}
	pPar<-NA
	if(kproject$type=='parallel'|kproject$type=='both'){
		p<-read.sim.folder(paste(dir,'pset1',sep='/'))
		#pPar<-ddply(p, .(time), colMeans)
		pst<-data.frame(pset=1)
		pOrig<-cbind(p,pst)
		pPar<-cbind(getSmoothTS(p),pst)
		names(pPar)->n
		flog.info(paste('parallel',1))
		for(i in 2:kproject$nSets){
		  pst<-data.frame(pset=i)
			p<-read.sim.folder(paste(dir,paste('pset',i,sep=''),sep='/'))
			if(length(p)>0){
				s<-cbind(getSmoothTS(p),pst)#ddply(p,.(time),colMeans,.progress = "text")
				pPar<-rbind(pPar,s)
				pOrig<-rbind(pOrig,cbind(p,pst))
				flog.info(paste('parallel',i))
			}
		}
	}
	kprojectSimExp<-list(project=kproject,resConc=pConc,resPar=pPar,orgPar=pOrig)
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
  dir(file,pattern='try*')->tries
  res<-c();
  if(length(tries)>0){
    res<-read.observables(paste(file,tries[1],'data.out',sep='/'))
    if(dim(res)[1]>0){
      res<-cbind(res,data.frame(N=1))
    }
    flog.info(paste(file,tries[1],'data.out',sep='/'))
    block<-1;
    if(length(tries)>1){
      for(j in 2:length(tries)){
        i<-tries[j]
        r<-read.observables(paste(file,i,'data.out',sep='/'))
        if(dim(r)[1]==0) next;
        if(dim(res)[1]>0){
          res<-rbind(res,cbind(r,
                               data.frame(N=j)))
        }else{
          res<-cbind(r,
                     data.frame(N=j))
        }
        if(aggregate){
          if(block>=block.size){
            block=0;
            flog.info('aggregate start')
            res<-ddply(res,.(time),function(.x)colSums(.x[,names(.x)!='time']))
            flog.info('aggregate over')
          }
        }
        block<-block+1;
        flog.info(paste(block,paste(file,i,'data.out',sep='/')))
      }
    }
    if(aggregate){
      flog.info('aggregate start')
      res<-ddply(res,.(time),function(.x)colSums(.x[,names(.x)!='time']))
      flog.info('aggregate over')
      ind<-!names(res)%in%c('time','N')
      res[,ind]<-res[,ind]/res$N;
    }
  }
  return(res)
  ###data from the folder
}

getSmoothTS<-function(
### utility function to calculate smoothed version of time series observables
  dat,##<< data.frame with oblervables for paramset in long format (i.e. try sets stacked together)
  timeC='time',##<< name of the time column
  nC='N',##<< name of try number column
  dataC=setdiff(names(dat),c('time','N'))##<< names of the columns to make smoothing over
){
  ddply(dat,.(N),
        summarize,
        mTime=max(time),
        timeStep=max(time)/length(time),
        num=length(time)
        )->timeSum
  tStep<-mean(timeSum$timeStep)
  nStep<-round(mean(timeSum$num))
  nTime<-seq(from=0,by=tStep,length.out = nStep+1)
  res<-data.frame(time=nTime,N=max(timeSum$N))
  for(c in dataC){
    indx<-which(!is.na(dat[,c]))
    ny<-predict(smooth.spline(x = dat$time[indx],y=dat[indx,c]),nTime)
    res[,c]<-ny$y
  }
  return(res)
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
