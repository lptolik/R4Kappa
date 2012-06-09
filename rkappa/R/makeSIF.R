makeSIF <-function(
### function creates SIF (Simple Interaction Format) from kappa string
kappa
###kappa string defining the complex structure
){
makeIGraph(kappa)->cplxg
simplify(cplxg)->cplxg
write.graph(cplxg,'test.ncol',format='ncol',names='name2')
t<-read.table('test.ncol',header=F)
t1<-data.frame(v1=t$V1,e='pp',v2=t$V2)
write.table(t1,'test_1.sif',row.names=F,col.names=F,quote=F,sep='\t')
return(cplxg)
###simplified iGraph graph of the complex
}
