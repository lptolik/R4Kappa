prepareProject <-function(
###Creates a infrastructure required to simulate kappa model with various parameter sets 
###and generate correspondent folder infrastructure
project=paste("multi",format(Sys.time(), "%Y%m%d%H%M%S"),sep=''),
###name of the project to be created, new folder will be created to contain the project 
###files
numSets=500,
###number of parameter sets to be generated
pTable=NA,
###Parameter ranges data frame. Should contain columns \code{param} with parameter names, 
###\code{Min} and \code{Max} with parameter ranges. Names in \code{param} column should 
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
shFile='run.sh.templ',
###run script template file name
jobFile='job.sh.templ',
###job management job file template
jobCFile='jobConc.sh',
###job management job template file for concurrent simulations
repReg="_-",
###regular expression to be replaced with number of parameter set
type=c('parallel','concurrent','both')
###type of the project
){
       if(!require(randtoolbox)){
         stop('Function is required package "randtoolbox"');
       }
       if(!require(gdata)){
         stop('Function is required package "gdata"');
       }
#files that do not need to be modified by the code
kproject<-list(name=project,date=format(Sys.time(), "%Y%m%d%H%M%S"))
	class(kproject)<-'kproject'
	if(is.na(pTable)){pTable<-data.frame(param='s',Min=1,Max=1)[FALSE,]}
	cF<-list();
	for(file in constantfiles){
		nmLines<-readLines(file)
		cF[file]<-list(nmLines)
	}
	kproject$constLines<-cF
	#files that need to be modified
	tF<-list();
	for(file in templatefiles){
		nmLines<-readLines(file)
		tF[file]<-list(nmLines)
	}
	kproject$templateLines<-tF
	kproject$replaceRegexp<-repReg
	kproject$nRep<-10
	kproject$nSets<-numSets
	kproject$execPath<-exec.path
	kproject$type<-type
	kproject$shLines<-list()
	#replace regex, string supposed to be replaced with index of the param set
	parTest<-paste("^'[0-9A-Za-z_-]+",repReg,"'$",sep="")
	paramTab<-data.frame(i=0,name="n",str=0,min=0.00,max=0.00,stringsAsFactors=FALSE)[FALSE,]
	n<-1
	nL<-list()
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
						paramTab[n,]<-data.frame(i=n,name=gsub("'",'',pd[2]),str=i,min=v,max=v,stringsAsFactors=FALSE)
					}else{
						paramTab[n,]<-data.frame(i=n,name=gsub("'",'',pd[2]),str=i,min=v*k_min,max=v*k_max,stringsAsFactors=FALSE)
					}
					n<-n+1
				}
			}
		}
	}
	kproject$paramLines<-nL
#	browser()
	if(dim(pTable)[1]>0){
		for(i in 1:dim(pTable)[1]){
			paramTab[paramTab$name==pTable[i,'param'],c('min','max')]<-pTable[i,c('Min','Max')]
		}
	}
	kproject$pTable<-paramTab
	ind<-which((paramTab$max-paramTab$min)>0)
	#x<-randtoolbox::sobol(numSets,dim=dim(paramTab)[1],scrambling=1,seed=100)
	x<-randtoolbox::sobol(numSets,dim=length(ind),scrambling=1,seed=100)
	#x1<-sapply(1:dim(paramTab)[1],function(.x) x[,.x]*(paramTab$max[.x]-paramTab$min[.x])+paramTab$min[.x])
	x1<-matrix(nrow=numSets,ncol=dim(paramTab)[1])
	colnames(x1)<-gsub(repReg,'',paramTab$name)
	x1[,ind]<-sapply(1:length(ind),function(.x) x[,.x]*(paramTab$max[ind[.x]]-paramTab$min[ind[.x]])+paramTab$min[ind[.x]])
	
	ind<-which(is.na(x1[1,]))
	for(i in ind){
		x1[,i]<-paramTab$max[i]
	}
	kproject$paramSets<-data.frame(x1)
	system(paste('mkdir -p',project))
	shLines<-readLines(shFile)
	kproject$shLines[shFile]<-list(shLines)
	cLine<-''
	if(length(cF)>0){
		for(i in 1:length(cF)){
			writeLines(cF[[i]],paste(project,'/',names(cF[i]),sep=''))
			cLine<-paste(cLine,'-i',names(cF[i]))
		}
	}
	constLine<-cLine
	for(i in 1:dim(x1)[1]){
		cLineL<-''
		for(j in names(tF)){
			tLines<-gsub(repReg,i,tF[[j]])
			writeLines(tLines,paste(project,'/',j,'.',i,sep=''))
			cLineL<-paste(cLineL,'-i',paste(j,'.',i,sep=''))
		}
#		browser()
		pLines<-c(paste('#parameters for set',i))
		for(k in 1:dim(paramTab)[1]){
			pLines[k+1]<-paste("%var: '",gsub(repReg,i,paramTab[k,'name']),"' ",x1[i,k],sep='')
		}
		writeLines(pLines,paste(project,'/param.ka.',i,sep=''))
		cLineL<-paste(cLineL,'-i',paste('param.ka.',i,sep=''))
		cLine<-paste(cLine,cLineL)
		if(type=='parallel'|type=='both'){
			shLinesL<-gsub('\\*\\*\\*',i,gsub(repReg,paste(constLine,cLineL),shLines))
			writeLines(shLinesL,paste(project,'/run',i,'.sh',sep=''))
			system(paste('chmod a+x ',project,'/run',i,'.sh',sep=''))
		}
	}
	if(type=='concurrent'|type=='both'){
		shLines<-gsub('\\*\\*\\*','Conc',gsub(repReg,cLine,shLines))
		writeLines(shLines,paste(project,'/runConc.sh',sep=''))
		system(paste('chmod a+x ',project,'/runConc.sh',sep=''))
		jLines<-readLines(jobCFile);
		kproject$shLines[jobCFile]<-list(jLines)
		writeLines(gsub('KKKKKK',exec.path,jLines),paste(project,'/jobConc.sh',sep=''))
	}
	if(type=='parallel'|type=='both'){
		jLines<-readLines(jobFile);
		kproject$shLines[jobFile]<-list(jLines)
		writeLines(gsub('KKKKKK',exec.path,jLines),paste(project,'/job.sh',sep=''))
	}
#	save(x1,paramTab,project,numSets,ptFile,constantfiles,templatefiles,paramfile,exec.path,shFile,jobFile,repReg,file=paste(project,'/param.Rdat',sep=''))
	return(kproject)
###project object
}
