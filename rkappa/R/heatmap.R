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

plot.point.sensitivity<-function(kasens,npar=30,conf.value=0.05){
  rn<-rownames(outK.SynGAP)
  sensTotRObs<-data.frame(wt=outK.wt[rn,'totRObs'],IRSp53=outK.IRSp53[rn,'totRObs'],PSD93=outK.PSD93[rn,'totRObs'],PSD95=outK.PSD95[rn,'totRObs'],SAP97=outK.SAP97[rn,'totRObs'],SynGAP=outK.SynGAP[rn,'totRObs'])
  rownames(sensTotRObs)<-rn
  conf.val<-0.01
  indTRO5p<-unique(c(which(outK.wt[rn,'pval_totRObs']<=conf.val),which(outK.IRSp53[rn,'pval_totRObs']<=conf.val),which(outK.PSD93[rn,'pval_totRObs']<=conf.val),which(outK.PSD95[rn,'pval_totRObs']<=conf.val),which(outK.SAP97[rn,'pval_totRObs']<=conf.val),which(outK.SynGAP[rn,'pval_totRObs']<=conf.val)))
  ss<-sensTotRObs[indTRO5p,]
  ss<-ss[order(ss$wt),]
  x<-as.matrix(ss);
  dimnames(x)->dn
  dn[[2]]<-c("wt","mut1","mut2","mut3","mut4","mut5")
  dimnames(x)<-dn
  levelplot(t(x),axis=axisOrt,xlab='',ylab='',at=seq(-1,1,by=0.1),col.regions=b1(200),main='TotRObs')
  ss1<-sensTotRObs[order(sensTotRObs$wt),]
  x1<-as.matrix(ss1);
  dimnames(x1)->dn
  dn[[2]]<-c("wt","IRSp53","PSD93","PSD95","SAP97","SynGAP")
  dn[[2]]<-c("wt","mut1","mut2","mut3","mut4","mut5")
  dimnames(x1)<-dn
  scTRO<-sort(abs(x1[,1])+abs(x1[,2]),decreasing=TRUE);
  levelplot(t(x1[(abs(x1[,1])+abs(x1[,2]))>scTRO[20],]),axis=axisOrt,xlab='',ylab='',at=seq(-1,1,by=0.1),col.regions=b1(200),main='TotRObs')
  
}

