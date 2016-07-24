prepareProject <-function(
###Creates a infrastructure required to simulate kappa model with various parameter sets 
###and generate correspondent folder infrastructure
project=NA,
###name of the project to be created, new folder will be created to contain the project 
###files, if not specified multi_current_date stub will be used.
numSets=500,
###number of parameter sets to be generated
pTable,
###Parameter ranges data frame. Should contain columns \code{name} with parameter names, 
###\code{min} and \code{max} with parameter ranges. Names in \code{name} column should 
###match names in the content of \code{paramfile} files.
constantfiles=c("main_rnap_def_rule.ka","main_rnap_init.ka","main_rnap_param.ka"),
###list of file names containing constant part of the model 
templatefiles=c("var_prom_def.ka","var_prom_init.ka","var_prom_rule.ka"),
###list of template file names to be used to create model
paramfile=c("var_prom_param.ka"),
###list of parameter file names
k_min= 0.1,
###minimum parameter value factor to be used if \code{pTable==NA}
k_max= 10, 
###maximum parameter value factor to be used if \code{pTable==NA}
exec.path="~/kasim3/KaSim",
###path to kappa language simulator executables in simulation environment
shFile=NA,#'run.sh.templ',
###run script template file name
jobFile=NA,#'job.sh.templ',
###job management job file template
jobCFile=NA,#'jobConc.sh',
###job management job template file for concurrent simulations
repReg="_-",
###regular expression to be replaced with number of parameter set
type=c('parallel','concurrent','both'),
###type of the project
writeDir=FALSE
###if TRUE project directory and its content will be created
){
#       if(!require(randtoolbox)){
#         stop('Function is required package "randtoolbox"');
#       }
#       if(!require(gdata)){
#         stop('Function is required package "gdata"');
#       }
##files that do not need to be modified by the code
  if(is.na(project)){
    project<-paste("multi",format(Sys.time(), "%Y%m%d%H%M%S"),sep='')
  }
	kproject<-make.kproject(project,numSets,exec.path,repReg,type)
	if(missing(pTable)){
		pTable<-data.frame(name='s',min=1,max=1)[FALSE,]
	}
#replace regex, string supposed to be replaced with index of the param set
  if(!is.na(repReg)){
    parTest<-paste("^'[0-9A-Za-z_-]+",repReg,"'$",sep="")
  }else{
    parTest<-"^'[0-9A-Za-z_-]+'$"
  }
	paramTab<-data.frame(i=0,name="n",min=0.00,max=0.00,stringsAsFactors=FALSE)[FALSE,]
	n<-1
	nL<-list()
	if(!any(is.na(constantfiles))){
		kproject$constLines<-readFiles(constantfiles)
	}
	if(!any(is.na(templatefiles))){
		kproject$templateLines<-readFiles(templatefiles)
	}
	for(file in paramfile){
		nmLines<-readLines(file)
		nL[file]<-list(nmLines)
		for(i in 1:length(nmLines)){
			if(nmLines[i]!=''){
				s<-trim(strsplit(nmLines[i],"#")[[1]])
				if(s[1]!=''){
					pd<-strsplit(s[1],' +')[[1]]
					if(length(pd)!=3||pd[1]!='%var:'||length(grep(parTest,pd[2]))!=1){
						stop(paste("wrong parameter file format: '",s,"'\n suppose to be '%var: 'param' value # comment'",sep=''))
					}
					v<-as.numeric(pd[3])
					if(is.na(v)){
						stop(paste("wrong parameter value format: '",s,"'\n suppose to be '%var: 'param' value # comment'",sep=''))
					}
					if(dim(pTable)[1]>0){
						paramTab[n,]<-data.frame(i=n,name=gsub("'",'',pd[2]),min=v,max=v,stringsAsFactors=FALSE)
					}else{
						paramTab[n,]<-data.frame(i=n,name=gsub("'",'',pd[2]),min=v*k_min,max=v*k_max,stringsAsFactors=FALSE)
					}
					n<-n+1
				}
			}
		}
	}
	kproject$paramLines<-nL
#	browser()
	if(dim(pTable)[1]>0){
		pTable$param=trim(pTable$param)
		for(i in 1:dim(pTable)[1]){
			paramTab[paramTab$name==pTable[i,'param'],c('min','max')]<-pTable[i,c('min','max')]
		}
	}
	kproject$pTable<-paramTab
  kproject$seed<-as.integer(Sys.time())
  pr<-addSets(kproject,nStart=1,nSets=kproject$nSets,seed =kproject$seed)
  kproject$paramSets<-pr$paramSets
  rm(pr)
# 	ind<-which((paramTab$max-paramTab$min)>0)
# 	#x<-randtoolbox::sobol(numSets,dim=dim(paramTab)[1],scrambling=1,seed=100)
# 	x<-randtoolbox::sobol(numSets,dim=length(ind),scrambling=1,seed=100)
# 	#x1<-sapply(1:dim(paramTab)[1],function(.x) x[,.x]*(paramTab$max[.x]-paramTab$min[.x])+paramTab$min[.x])
# 	x1<-matrix(nrow=numSets,ncol=dim(paramTab)[1])
# 	colnames(x1)<-gsub(repReg,'',paramTab$name)
# 	x1[,ind]<-sapply(1:length(ind),function(.x) x[,.x]*(paramTab$max[ind[.x]]-paramTab$min[ind[.x]])+paramTab$min[ind[.x]])
# 	
# 	ind<-which(is.na(x1[1,]))
# 	for(i in ind){
# 		x1[,i]<-paramTab$max[i]
# 	}
# 	kproject$paramSets<-data.frame(x1)
	if(is.na(shFile) | is.na(jobFile) | is.na(jobCFile)){
		data(templProject)
			kproject$shLines<-templProject$shLines
	}
	if(!is.na(shFile)) kproject$shLines[['run.sh.templ']]<-readFiles(shFile)[[1]]
	if(!is.na(jobFile)) kproject$shLines[['job.sh.templ']]<-readFiles(jobFile)[[1]]
	if(!is.na(jobCFile)) kproject$shLines[['jobConc.sh.templ']]<-readFiles(jobCFile)[[1]]
	if(writeDir){
		write.kproject(kproject)
	}
#	save(x1,paramTab,project,numSets,ptFile,constantfiles,templatefiles,paramfile,exec.path,shFile,jobFile,repReg,file=paste(project,'/param.Rdat',sep=''))
  kproject$updateDate=format(Sys.time(), "%Y%m%d%H%M%S")
	return(kproject)
###project object
}

make.kproject<-function(
###Creates a infrastructure required to simulate kappa model with various parameter sets 
###and generate correspondent folder infrastructure
project=NA,
###name of the project to be created, new folder will be created to contain the project 
###files, if not specified multi_current_date stub will be used.
numSets=500,
###list of parameter file names
exec.path="~/kasim3/KaSim",
###path to kappa language simulator executables in simulation environment
repReg="_-",
###regular expression to be replaced with number of parameter set
type=c('parallel','concurrent','both')
###type of the project
){
  if(is.na(project)){
    project<-paste("multi",format(Sys.time(), "%Y%m%d%H%M%S"),sep='')
  }
  kproject<-list(name=project,createDate=format(Sys.time(), "%Y%m%d%H%M%S"))
	class(kproject)<-'kproject'
	paramTab<-data.frame(i=0, name="n", min=0.00, max=0.00, stringsAsFactors=FALSE)[FALSE,]
	kproject$constLines<-list()
	#files that need to be modified
	kproject$templateLines<-list()
	kproject$replaceRegexp<-repReg
	kproject$nRep<-10
	kproject$nSets<-numSets
	kproject$execPath<-exec.path
	kproject$type<-type
	kproject$shLines<-list()
	kproject$paramLines<-list()
	kproject$pTable<-paramTab
	kproject$paramSets<-NULL
  kproject$dateC<-format(Sys.time(), "%Y%m%d%H%M%S")
	return(kproject)
}

recreate.kproject<-function(
###Function creates clone of existing project with modification according to 
###values of arguments. NA values will be kept the same as original project
project,##<<original project to be used as template
name=NA,##<<name for the new project
constantfiles=NA,##<<list of file names containing constant part of the model 
templatefiles=NA,##<<list of template file names to be used to create model
paramfile=NA,##<<list of names for parameter file to be generated
exec.path=NA,##<<path to kappa language simulator executables in simulation environment
shFile=NA,##<<run script template file name
jobFile=NA,##<<job management job file template
jobCFile=NA,##<<job management job template file for concurrent simulations
repReg=NA,##<<regular expression to be replaced with number of parameter set
type=project$type
###type of the project,c('parallel','concurrent','both')
){
##note<<The main reason to have \dQuote{recreate} method is to have an ability to compare behaviour of various model setups. During the process of \dQuote{recreation} the only part of the project that is never change is \code{kproject$paramSet}. So updated model will be executed with the same set of parameter values make an appropriate comparisons.
#       if(!require(gdata)){
#         stop('Function is required package "gdata"');
#       }
	res<-project
	res$updateDate=format(Sys.time(), "%Y%m%d%H%M%S")
	res$dateC=format(Sys.time(), "%Y%m%d%H%M%S")
	if(!is.na(name)){
		res$name<-name
	}else{
	#to prevent name collapse
		res$name<-paste(res$name,'cl',res$date,sep='_')
	}
	if(!any(is.na(constantfiles))){
		res$constLines<-readFiles(constantfiles)
	}
	if(!any(is.na(templatefiles))){
		res$templateLines<-readFiles(templatefiles)
	}
	if(!is.na(paramfile)){
	}
	if(!is.na(exec.path)){
		res$execPath<-exec.path
	}
	if(!is.na(repReg)){
		res$replaceRegexp<-repReg
	}
	if(!is.na(shFile)){
		res$shLines[['run.sh.templ']]<-readFiles(shFile)
	}
	if(!is.na(jobFile)){
		res$shLines[['job.sh.templ']]<-readFiles(jobFile)
	}
	if(!is.na(jobCFile)){
		res$shLines[['jobConc.sh.templ']]<-readFiles(jobCFile)
	}
	res$type<-type
	return(res)
}
readFiles<-function(
###Read file contents into character list
files##<<vector of file names to read
){
	cF<-list();
	fNames<-gsub('.*[^/]+/([^/]+)$','\\1',files)
	if(!any(is.na(files))){
		for(i in 1:length(files)){
		  file<-files[i]
		  fn<-fNames[i]
			nmLines<-readLines(file)
			cF[fn]<-list(nmLines)
		}
	}
	return(cF)
###list of contents of the files. Each element of the list named after a file it contains.
}

addSets<-function(
###Prepare new set of parameter vectors for \code{kproject}
kproject,##<<project to prepare sets for
nStart=1,##<<start index of the set
nSets=500,##<<number of the sets to be added
seed = 100##<<random generator seed to expand existing set in the project use \code{kproject$seed}
){
  kproject$pTable->paramTab
  ind<-which((paramTab$max-paramTab$min)>0)
  numSets<-nStart+nSets-1
  x<-randtoolbox::sobol(numSets,dim=length(ind),scrambling=1,seed=seed)
  #x1<-sapply(1:dim(paramTab)[1],function(.x) x[,.x]*(paramTab$max[.x]-paramTab$min[.x])+paramTab$min[.x])
  x1<-matrix(nrow=numSets,ncol=dim(paramTab)[1])
  colnames(x1)<-gsub(kproject$replaceRegexp,'',paramTab$name)
  x1[,ind]<-sapply(1:length(ind),function(.x) x[,.x]*(paramTab$max[ind[.x]]-paramTab$min[ind[.x]])+paramTab$min[ind[.x]])
  if(any(is.na(x1[1,]))){
  ind<-which(is.na(x1[1,]))
  for(i in ind){
    x1[,i]<-paramTab$max[i]
  }
  }
  paramSets<-data.frame(x1)
  if(is.null(kproject$paramSets)||dim(kproject$paramSets)[2]!=dim(paramSets)[2]){
    kproject$paramSets<-paramSets[nStart:numSets,]
  }else{
    kproject$paramSets[nStart:numSets,]<-paramSets[nStart:numSets,]
  }
  kproject$nSets<-dim(kproject$paramSets)[1]
  kproject$updateDate=format(Sys.time(), "%Y%m%d%H%M%S")
  invisible(kproject) 
}
