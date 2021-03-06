makeSiteGraph<-function(
  ###function to create igraph representation of kappa string with sites being separate nodes
  kappa
  ###complex definition in kappa language
  , num = 1
  ### number of complex instances in the mixture
){
  edges<-list()
  agents<-list()
  agmarks<-list()
  g <- graph.empty(n = 0, directed =FALSE) 
  cl<-colors()
  vcl<-list()
  idx<-0
  subg<-0
  subg<-subg+1
  if (length(num) == 1)
    num = rep(num, length(kappa))
  for (kpi in 1:length(kappa)) {
    kp <- kappa[kpi]
    instN <- num[kpi]
    for (ii in 1:instN) {
      k<-sub('\\)$','',kp) 
      unlist(strsplit(k,'),',fixed=TRUE))->parts 
      strs<-lapply(strsplit(parts,'[(,]'),function(x) strsplit(x,'!')) 
      for(i in 1:length(strs)){
        idx<-idx+1
        n<-strs[[i]][[1]] 
        nname=paste(n,idx,sep='_') 
        agmarks[[nname]]<-idx 
        nidx<-idx
        if(!(n %in% names(vcl))){
          vcl[[n]]<-colors()[8+length(vcl)*3] 
        }
        if(!(n %in% names(agents))){ 
          agents[[n]]<-list()
        } 
        g<-add.vertices(g,1,attr=list(name=n,name2=nname,color=vcl[[n]],type='agent',size=30)) 
        for(j in 2:length(strs[[i]])){
          s<-strs[[i]][[j]][1]
          if(!(s %in% names(agents[[n]]))){
            agents[[n]][[s]]<-list() 
          }
          if(!(s %in% names(vcl))){ 
            vcl[[s]]<-colors()[8+length(vcl)*3]
          }
          idx<-idx+1
          agmarks[[nname]]<-c(agmarks[[nname]],idx) 
          g<-add.vertices(g,1,attr=list(name=s,name2=paste0('site_',s),color=vcl[[s]],type='site',size=15)) 
          g<-add.edges(g,c(nidx,idx),type='site',weight=10,color='grey40',width=10) 
          if(length(strs[[i]][[j]])>1){
            agents[[n]][[s]]<-append(agents[[n]][[s]],strs[[i]][[j]][2]) 
            e<-paste(strs[[i]][[j]][2],subg,sep='_')
            if(e %in% names(edges)){
              g<-add.edges(g,c(edges[[e]],idx),type='bond',weight=1,color='black',width=3)
            }else{ 
              edges[e]<-idx
            } 
          }
        } 
      }
    }
  }
  g$marks<-agmarks
  return(g) 
  ###site graph in igraph 
}

plotRuleGraph<-function(
  ### Creates site graph and plot it
  kp##<< kappa string to convert
){
  gLHS<-makeSiteGraph(kp)
  lL<-layout_with_fr(gLHS)
  lL<-layout_(gLHS,with_fr(coords = lL),normalize())
  plot(gLHS,layout=lL,vertex.shape='sphere',rescale=FALSE,vertex.label.cex=2)
}
