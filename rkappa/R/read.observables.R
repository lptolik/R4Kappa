read.observables <-function(
###function to read and parse KaSim simulation results
file
###name of the observable file
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
