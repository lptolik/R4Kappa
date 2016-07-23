print.kproject<-function(
###Kproject class print method
x
###object to print
, ...){
	cat("Name: '",x$name,"'\n")
	cat("Created:",x$date,"\n")
	cat("Parameters:",dim(x$pTable)[1],"\n")
	cat("Sets:",x$nSets,"\n")
	cat("---------------\n")
}

summary.kproject<-function(
###Kproject class summary method
object
###object to create summary for
, ...){
	x<-object
	res<-list(name=x$name,date=x$date,type=x$type)
	res$param<-summary(x$pTable)
	res$sets<-summary(x$paramSets)
	class(res)<-'summary.kproject'
###kproject.summary object
}

print.summary.kproject<-function(
###Kproject.summary print method
x,
###object to print
...){
	cat("Kappa Project: '",x$name,"Created:",x$date,"\n")
	cat("Parameter ranges:\n")
	print(x$param)
	cat("Parameter sets:\n")
	print(x$sets)
}
