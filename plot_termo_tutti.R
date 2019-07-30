#####################################
dir<-"./"
inizio<-as.POSIXct(strptime("2018/01/01", format="%Y/%m/%d"))
fine<-as.POSIXct(strptime("2019/05/01", format="%Y/%m/%d"))
Tmin<--30
Tmax<-45
file_ric <- "Richiesta.txt"
file_out <- "Estremi.csv"
# 0 - creazione pdf, 1 - grafico a video interattivo 
interattivo <- 0
####################################


#preparazione output
cat("TERMOMETRI \n",file=file_out)
cat("---","Nome", "IDsensore","min","oramin","max","oramax","\n",sep="\t",file=file_out,append=T)
file_log<-paste(dir,"plot.log",sep="")
cat("Termometri ", date(),"\n",file=file_log)

# lettura file di richiesta per individuare i sensori
richiesta<- read.csv ( file_ric , header = FALSE, as.is = TRUE, sep="\t")
IDsensore<-richiesta$V2

# lettura dell'anagrafica REM per dare un nome ai sensori
anagrafica <- read.csv ( "AnagraficaSensori.csv" , header = TRUE ,  quote="\"", as.is = TRUE, sep=";", na.strings = "-9999")

# colori per min-avg-max)
colore=c("green","black","red")

# ciclo su sensori elencati nel file di richiesta dell'estrattore e eseguo pdf del grafico e file csv 
for (i in 1:length(IDsensore)){
  
  oo<-which(anagrafica$IdSensore==IDsensore[i])
  if(is.na(anagrafica$Attributo[oo])==TRUE) {
    Nome <- anagrafica$Comune[oo]
  }else{
    Nome <- paste(anagrafica$Comune[oo], anagrafica$Attributo[oo] ,sep="_")
  }
  
  cat("\n",file=file_out,append=T)
  
#########################################################################################
  
## modalità interattiva
  if (interattivo==1){
   par(ask = TRUE)
  }else if(interattivo==0){
   ## direttamente su file
   nomepdf <- paste(IDsensore[i],"_",Nome,".pdf",sep="")
   pdf(nomepdf)
  }else{
   print("ATTENZIONE: interattivo deve essere 0 o 1")
   break
  }
  
#################################################################
  #    
  minimo <- paste(path=dir, IDsensore[i], "_R_2.txt",sep="")
  medie  <- paste(path=dir, IDsensore[i], "_R_1.txt",sep="")
  max    <- paste(path=dir, IDsensore[i], "_R_3.txt",sep="")
  #min_orario<-paste(path=dir, IDsensore[i], "_G_13.txt",sep="")
  #max_orario<-paste(path=dir, IDsensore[i], "_G_12.txt",sep="")
  
  if(file.exists(minimo)){
    lettura_min<-try(read.csv(minimo,header=FALSE,na.strings = "-999",colClasses=c("integer","character","numeric","numeric"),sep="\t"),silent=TRUE)
    if (inherits(lettura_min,"try-error")) {
      cat("....... ERRORE !! file:   ...........................\n",file=file_log,append=T)
    }
    data_del_min <- as.POSIXct(strptime(lettura_min$V2[which.min(lettura_min$V3)], format="%Y-%m-%d %H:%M:%S"))
    data_del_max <- as.POSIXct(strptime(lettura_min$V2[which.max(lettura_min$V3)], format="%Y-%m-%d %H:%M:%S"))
    cat("min",Nome,IDsensore[i],min(lettura_min$V3,na.rm=TRUE),format(data_del_min,format="%Y-%m-%d %H:%M"),max(lettura_min$V3,na.rm=TRUE),format(data_del_max,format="%Y-%m-%d %H:%M"),"\n",sep="\t",file=file_out,append=T)
    marche_temporali_min<-as.POSIXct(strptime(lettura_min$V2, format="%Y-%m-%d %H:%M:%S"))
    plot(marche_temporali_min,lettura_min$V3,xlim=c(inizio,fine),ylim<-c(Tmin,Tmax),main=paste(Nome , IDsensore[i],sep=" - "),type="l",col=colore[1])
   }
  
  lettura_medie<-try(read.csv(medie,header=FALSE,na.strings = "-999",colClasses=c("integer","character","numeric","numeric"),sep="\t"),silent=TRUE)
   if (inherits(lettura_medie,"try-error")) {
    cat("....... ERRORE !! file:  ...........................\n",file=file_log,append=T)
   }
  data_del_min <- as.POSIXct(strptime(lettura_medie$V2[which.min(lettura_medie$V3)], format="%Y-%m-%d %H:%M:%S"))
  data_del_max <- as.POSIXct(strptime(lettura_medie$V2[which.max(lettura_medie$V3)], format="%Y-%m-%d %H:%M:%S"))
  cat("Avg",Nome,IDsensore[i],min(lettura_medie$V3,na.rm=TRUE),format(data_del_min,format="%Y-%m-%d %H:%M"),max(lettura_medie$V3,na.rm=TRUE),format(data_del_max,format="%Y-%m-%d %H:%M"),"\n",sep="\t",file=file_out,append=T)
  marche_temporali_medie<-as.POSIXct(strptime(lettura_medie$V2, format="%Y-%m-%d %H:%M:%S"))
  #
   if(file.exists(minimo)){
    lines(marche_temporali_medie,lettura_medie$V3,col=colore[2])
   }else{
    plot(marche_temporali_medie,lettura_medie$V3,xlim=c(inizio,fine),ylim<-c(Tmin,Tmax),main=paste(Nome , IDsensore[i],sep=" - "),type="l",col=colore[2])
   }
 
   if(file.exists(max)){
    lettura_max<-try(read.csv(max,header=FALSE,na.strings = "-999",colClasses=c("integer","character","numeric","numeric"),sep="\t"),silent=TRUE)
    if (inherits(lettura_max,"try-error")) {
      cat("....... ERRORE !! file:  ...........................\n",file=file_log,append=T)
    }
   
    data_del_min <- as.POSIXct(strptime(lettura_max$V2[which.min(lettura_max$V3)], format="%Y-%m-%d %H:%M:%S"))
    data_del_max <- as.POSIXct(strptime(lettura_max$V2[which.max(lettura_max$V3)], format="%Y-%m-%d %H:%M:%S"))
    cat("Max",Nome,IDsensore[i],min(lettura_max$V3,na.rm=TRUE),format(data_del_min,format="%Y-%m-%d %H:%M"),max(lettura_max$V3,na.rm=TRUE),format(data_del_max,format="%Y-%m-%d %H:%M"),"\n",sep="\t",file=file_out,append=T)
    marche_temporali_max<-as.POSIXct(strptime(lettura_max$V2, format="%Y-%m-%d %H:%M:%S"))
    lines(marche_temporali_max,lettura_max$V3,col=colore[3])
   }
  
  legend(inizio,Tmax,c("min","med","max"), col=c(colore[1],colore[2],colore[3]),lty=1,cex=1,lwd=2)
  
   i<-i+1
   print(Nome)
  }

