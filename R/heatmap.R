heatmap.pop<-function (x, 
                       ...) 
{
  which(V(felg)$name %in% fen)->nm
  indR<-which(rownames(p) %in% fen_name)
  x<-as.matrix(p[indR,])
  Rowv <- rowMeans(x, na.rm = TRUE)
  hcr <- hclust(dist(x))
  ddr <- as.dendrogram(hcr)
  ddrr <- reorder(ddr, Rowv)
  cLab<-unique(comSet$membership[nm])
  m<-matrix(data=0, ncol=length(unique(comSet$membership[nm])), nrow=length(order.dendrogram(ddr)))
  for(i in 1:length(indR)){
    j<-which(rownames(x) == fen_name[i])
    k<-which(cLab==comSet$membership[V(felg)$name == fen[i]])
    m[j,k]<-m[j,k]+1
  }
}

levelplot.point.sensitivity<-function(
  ### Function to plot sensitivity values of several experiments with levelplot
  sens,##<< data frame of sensitivity coefficients with experiments on the columns and parameters on the rows
  pval=NA,##<< data frame of P-values with experiments on the columns and parameters on the rows. If NA then 
  ### conf.value is ignored and npar parameters with greatest absolute values in the first column is plotted.
  npar=30,##<< number of best parameters to show
  conf.value=0.05,##<< confidence value to 
  main,##<< title of the plot
  cnames=NA,##<< column names to be used on the X axis of the plot, if NA use column names
  b1=NA,##<< color palette to be used with plot. By default 'RdGy' is used
  ...#<<< additional parameters to be sent to lattice levelplot function
){
  if(!require(lattice)){
    stop('Function is required package "lattice"');
  }
  if(!require(RColorBrewer)){
    stop('Function is required package "RColorBrewer"');
  }
  if(is.na(b1)){
    b1<-colorRampPalette(brewer.pal(11,'RdGy'),interpolate='spline')#!!! middle is white
  }
  if(!is.na(pval)){
    indCV<-c()
    for(i in 1:dim(sens)[2]){
      indCV<-c(indCV,which(pval[,i]<=conf.val))
    }
    indCV<-unique(indCV)
  }else{
    indCV<-rep(-1,times=npar+1)
  }
  ##note<< in the case that npar value is less that the number of parameters within confidence interval defined or there is no pval 
  ## provided, levelplot of npar best parameters will be plotted. 
  if(length(indCV)<=npar){
    ss<-sens[indCV,]
    ss<-ss[order(ss[,1]),]
    x<-as.matrix(ss);
    if(!is.na(cnames)){
      dimnames(x)->dn
      dn[[2]]<-cnames
      dimnames(x)<-dn
    }
    res<-levelplot(t(x),axis=.axisOrt,xlab='',ylab='',at=seq(-1,1,by=0.1),col.regions=b1(200),main=main)
  }else{
    ss1<-sens[order(sens[,1]),]
    x<-as.matrix(ss1);
    scTRO<-sort(rowSums(abs(x)),decreasing=TRUE);
    if(!is.na(cnames)){
      dimnames(x)->dn
      dn[[2]]<-cnames
      dimnames(x)<-dn
    }
    res<-levelplot(t(x[(rowSums(abs(x)))>scTRO[npar],]),axis=axisOrt,xlab='',ylab='',at=seq(-1,1,by=0.1),col.regions=b1(200),main='TotRObs')
    
  }
  return(res)
}

.matrix.axes <- function(data) {
  # Do the rows, las=2 for text perpendicular to the axis
  x <- (1:dim(data)[1] - 1) / (dim(data)[1] - 1);
  axis(side=1, at=x, labels=rownames(data), las=2);
  # Do the columns
  x <- (1:dim(data)[2] - 1) / (dim(data)[2] - 1);
  axis(side=2, at=x, labels=sub('_integral','',colnames(data)), las=2);
  # Add a solid black grid
  grid(nx=(dim(data)[1]-1), ny=(dim(data)[2]-1), col="black", lty="solid");
}
.axisOrt<-function(side,components,...){
  if(side == 'bottom'){
    comp.list <- components[["bottom"]]
    panel.axis(side=side,outside=TRUE,at = comp.list$ticks$at, 
               labels = sub('perint','',comp.list$labels$labels),rot=90)
  }else axis.default(side,components,...);
}
.axiSing<-function(side,components,...){
  if(side == 'bottom' || side == 'top'){
  }else axis.default(side,components,...);
}
