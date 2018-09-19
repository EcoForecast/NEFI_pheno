months <- c("June","July","Aug","Sept","Oct","Nov")
startDays <- c(152,182,213,244,274,305)
outputFileName <- paste("GOES_NDVI_DiurnalCurves_ALL.pdf",sep="")
pdf(file=outputFileName,width=20,height=30)

for(k in 1:length(months)){
  par(mfrow=c(10,1))
  par(mar=c(1,1,1,1))
  startDay <- startDays[k]
  endDay <- startDay + 6
  days <- seq(startDay,endDay,1)
  month <- months[k]
  filestr <- paste(month,"_kappaDQF.csv",sep="")
  diurnalFiles <-dir(pattern=filestr)
  for(j in 1:length(diurnalFiles)){
    diurnal.data <- data.frame(read.csv(diurnalFiles[j],header=FALSE))
    siteName <- substr(diurnalFiles[j],18,21)
    diurnal.data <- rbind(diurnal.data,matrix(nrow=4,ncol=dim(diurnal.data)[2]))
    diurnal.data[3,] <- as.numeric(substr(diurnal.data[1,],5,7))
    diurnal.data[4,] <- as.numeric(substr(diurnal.data[1,],8,9))
    diurnal.data[5,] <- as.numeric(substr(diurnal.data[1,],10,11))
    diurnal.data <- data.frame(t(diurnal.data))
    diurnal.data[,6] <- as.numeric(diurnal.data[,3]) +(as.numeric(diurnal.data[,4]) +(as.numeric(diurnal.data[,5])/60))/24
    colnames(diurnal.data) <- c("day.time","NDVI","day","hour","minutes","time")
    plot(diurnal.data$time,diurnal.data$NDVI,main=paste(month,siteName),ylim=c(0,1),pch=19,cex=1.5)
    abline(v=(days+16.95/24),col="red",cex=1.5)
    #for (i in days){
     # day.vals <- diurnal.data[diurnal.data$day==i,]
      #plot(day.vals$time,day.vals$NDVI,ylim=c(0,1),ylab="NDVI",xlab="Time",main=paste(siteName,i),xlim=c(0,24),pch=19)
    #  abline(v=16.95,col="red")
    #}
  }
  
}
dev.off()













months <- c("June","July","Aug","Sept","Oct","Nov")
startDays <- c(152,182,213,244,274,305)

for(k in 1:length(months)){
  startDay <- startDays[k]
  endDay <- startDay + 6
  days <- seq(startDay,endDay,1)
  month <- months[k]
  filestr <- paste(month,"_kappaDQF.csv",sep="")
  diurnalFiles <-dir(pattern=filestr)

  outputFileName <- paste("GOES_NDVI_DiurnalCurves_",month,"2.pdf",sep="")
  pdf(file=outputFileName,width=8,height=8)

  for(j in 1:length(diurnalFiles)){
    diurnal.data <- data.frame(read.csv(diurnalFiles[j],header=FALSE))
    siteName <- substr(diurnalFiles[j],18,21)
    diurnal.data <- rbind(diurnal.data,matrix(nrow=4,ncol=dim(diurnal.data)[2]))
    diurnal.data[3,] <- as.numeric(substr(diurnal.data[1,],5,7))
    diurnal.data[4,] <- as.numeric(substr(diurnal.data[1,],8,9))
    diurnal.data[5,] <- as.numeric(substr(diurnal.data[1,],10,11))
    diurnal.data <- data.frame(t(diurnal.data))
    diurnal.data[,6] <- as.numeric(diurnal.data[,4]) +(as.numeric(diurnal.data[,5])/60)
    
    colnames(diurnal.data) <- c("day.time","NDVI","day","hour","minutes","time")
    for (i in days){
      day.vals <- diurnal.data[diurnal.data$day==i,]
      plot(day.vals$time,day.vals$NDVI,ylim=c(0,1),ylab="NDVI",xlab="Time",main=paste(siteName,i),xlim=c(0,24),pch=19)
      abline(v=16.95,col="red")
    }
  }
  dev.off()
  
}














outputFileName <- "GOES_NDVI_DiurnalCurves_Sept.pdf"
pdf(file=outputFileName,width=8,height=8)
#inputFileName <- "GOES_NDVI_DiurnalHarvardForest_kappaDQF.csv"
#inputFileName <- "GOES_NDVI_DiurnalHarvardForest_June_kappaDQF.csv"
inputFileName <- "GOES_NDVI_DiurnalHarvardForest_Sept_kappaDQF.csv"
diurnal.data <- data.frame(read.csv(inputFileName,header=FALSE))
diurnal.data <- rbind(diurnal.data,matrix(nrow=4,ncol=dim(diurnal.data)[2]))
diurnal.data[3,] <- as.numeric(substr(diurnal.data[1,],5,7))
diurnal.data[4,] <- as.numeric(substr(diurnal.data[1,],8,9))
diurnal.data[5,] <- as.numeric(substr(diurnal.data[1,],10,11))
diurnal.data <- data.frame(t(diurnal.data))
diurnal.data[,6] <- as.numeric(diurnal.data[,4]) +(as.numeric(diurnal.data[,5])/60)

colnames(diurnal.data) <- c("day.time","NDVI","day","hour","minutes","time")
startDay <- 244
endDay <- 250
days <- seq(startDay,endDay,1)
for (i in days){
  day.vals <- diurnal.data[diurnal.data$day==i,]
  plot(day.vals$time,day.vals$NDVI,ylim=c(0,1),ylab="NDVI",xlab="Time",main=i,xlim=c(0,24),pch=19)
  abline(v=16.95,col="red")
  i <- 158
}
dev.off()
