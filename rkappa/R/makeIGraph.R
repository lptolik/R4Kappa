makeIGraph <-function(
###function converts kappa string, defining the complex structure into iGraph graph
kappa
###complex definition in kappa language
){
       if(!require(igraph0)){
         stop('Function is required package "igraph"');
       }
       k<-sub('\\)$','',kappa)
       unlist(strsplit(k,'),',fixed=TRUE))->parts
       edges<-list()
       g <- graph.empty(n = 0, directed =FALSE)
       cl<-colors()
       vcl<-list()
       strs<-lapply(strsplit(parts,'[(,]'),function(x) strsplit(x,'!'))
       for(i in 1:length(strs)){
               n<-strs[[i]][[1]]
               nname=paste(n,i,sep='_')
               if(!(n %in% names(vcl))){
                       vcl[[n]]<-colors()[8+length(vcl)*3]
                       #cat(paste(n,length(vcl),vcl[[n]],'\n'))
               }
               g<-add.vertices(g,1,attr=list(name=strs[[i]][[1]],name2=nname,color=vcl[[n]]))
               for(j in 2:length(strs[[i]])){
                       if(length(strs[[i]][[j]])>1){
                               e<-strs[[i]][[j]][2]
                               if(e %in% names(edges)){
                                       g<-add.edges(g,c(edges[[e]],i-1))

                               }else{
                                       edges[e]<-i-1
                               }
                       }
               }
       }
       return(g)
###simplified iGraph graph of the complex
}
