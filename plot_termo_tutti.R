nomefile<-list.files(path=".", pattern="*_R_[1,2,3]*.txt")
nomefile<-sort(nomefile)
print(nomefile)
colore<-rep(palette(),1,length(nomefile))
IDsensore<-NA
IDfunzione<-NA
for (i in 1:length(nomefile)) {IDsensore[i]<-as.character(strsplit(nomefile[i], "_")[[1]][1])}
for (i in 1:length(nomefile)) {IDfunzione[i]<-strsplit(as.character( strsplit(nomefile[i], "_")[[1]][3]),".",fixed=T)[[1]][1]}
for (i in 1:length(nomefile)){
  lettura<-try(read.csv(nomefile[i],header=FALSE,na.strings = "-999",colClasses=c("integer","character","numeric","numeric"),sep="\t"),silent=TRUE)
  if (inherits(lettura,"try-error")) {
    cat("....... ERRORE !! file: ", nomefile[i],"  ...........................\n",file="prova.log",append=T)
    i<-i+1
    break
  }
  print(lettura[1,])
  print(i)
  marche_temporali<-as.POSIXct(strptime(lettura$V2, format="%Y-%m-%d %H:%M:%S"))
  cat("\n",nomefile[i],":   Dal ",min(lettura$V2)," al ", max(lettura$V2) ,"\n",sep="",file="prova.log",append=T)
  cat("\n","minimo:   ", min(lettura$V3,na.rm=TRUE),"  data minimo:  ", lettura$V2[which.min(lettura$V3)],sep="",file="prova.log",append=T)
  cat("\n","massimo:  ", max(lettura$V3,na.rm=TRUE),"  data massimo:  ", lettura$V2[which.max(lettura$V3)],sep="","\n",file="prova.log",append=T)
  cat(IDsensore[i],IDfunzione[i],min(lettura$V3,na.rm=TRUE),lettura$V2[which.min(lettura$V3)],max(lettura$V3,na.rm=TRUE),lettura$V2[which.max(lettura$V3)],"\n",sep=";",file="estremi.txt",append=T)
   if (i==1){
    plot(marche_temporali,(lettura$V3),main=paste("Termometro",IDsensore[i],sep=""),type="l",col=colore[i])}
  else if(IDsensore[i]!=IDsensore[i-1]){
    plot(marche_temporali,(lettura$V3),main=paste("Termometro",IDsensore[i],sep=""),type="l",col=colore[i])}
  else {
    lines(marche_temporali,(lettura$V3),col=colore[i+1])}
  i<-i+1
  par(ask = TRUE)
  }

