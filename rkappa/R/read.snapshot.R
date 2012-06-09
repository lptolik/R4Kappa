read.snapshot <-function(
###function to read KaSim snapshot kappa files
file
###name of the snapshot file
){
l<-readLines(file)
N<-length(l)
dat<-data.frame(Ev=1,T=1,Num=1,Kappa='s', Brutto='bstring', Weight=1, Comp=1,stringsAsFactors =FALSE)[FALSE,]
ev.T<-as.numeric(unlist(strsplit(sub('^.*Event: +([0-9.]+), *Time: +([0-9.]+).*$','\\1,\\2',l[1]),',')))
for(i in 1:(N/2-1)){
  num<-as.integer(sub('^.*: +([0-9]+).*','\\1',l[2*i]))
  kappa<-l[2*i+1]
  table(gsub('\\(.*$','',unlist(strsplit(kappa,'),',fixed=TRUE))))->brutto
  bstring<-paste(c(rbind(names(brutto),paste(brutto))),collapse='.')
  dat[i,]<-data.frame(Ev=ev.T[1],T=ev.T[2], Num=num,Kappa=kappa,  Brutto=bstring, Weight=sum(brutto), Comp=length(brutto), stringsAsFactors =FALSE)
}
return(dat)

}
