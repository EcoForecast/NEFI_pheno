library("PhenologyBayesModeling")

dayData <- read.csv("sampleDiurnalDays.csv",header=TRUE)
hr.seq <- c("00","01","02","03","04","05","06","07","08","09",as.character(seq(10,23)),"00","01","02","03","04","05","06")
pdf(file="SingleDiurnalCurves.pdf",width=8,height=8)
i=1
for(i in 1:nrow(dayData)){
  siteName <- as.character(dayData[i,]$Site)
  print(siteName)
  yr <- as.character(dayData[i,]$Year)
  mth <- as.character(dayData[i,]$Month)
  dy <- as.character(dayData[i,]$Day)
  TZ <- as.numeric(dayData[i,]$TZ)
  DOY <- as.numeric(dayData[i,]$DOY)
  quality <- as.character(dayData[i,]$Quality)
  inFileName <- paste("GOES_NDVI_Diurnal",siteName,"_",mth,"_kappaDQF.csv",sep="")
  print(inFileName)
  weekDat <- read.csv(inFileName,header=FALSE)
  weekDat <- rbind(weekDat,matrix(nrow=4,ncol=dim(weekDat)[2]))
  rownames(weekDat) <- c("day.time","NDVI","day","hour","minutes","dayhr")
  weekDat[3,] <- substr(weekDat[1,],5,7) #day
  weekDat[4,] <- substr(weekDat[1,],8,9) #hour
  weekDat[5,] <- substr(weekDat[1,],10,11) #minutes
  weekDat[6,] <- paste(weekDat[3,],weekDat[4,],sep="") #dayhr
  hr.seq2 <- hr.seq[TZ:(TZ+23)]
  day.hr.seq <- c(paste(as.character(DOY),hr.seq2[1:(25-TZ)],sep=""),paste(as.character(DOY+1),hr.seq2[(26-TZ):length(hr.seq2)],sep=""))
  time.vals <- character()
  NDVI.vals <- numeric()
  timesComp <- numeric()
  for(j in 1:ncol(weekDat)){
    if(weekDat[6,j] %in% day.hr.seq){
      time.vals <- c(time.vals,weekDat[1,j])
      NDVI.vals <- c(NDVI.vals,weekDat[2,j])
      if(as.numeric(weekDat[4,j])<8){
        weekDat[4,j] <- as.numeric(weekDat[4,j])+24
      }
      timeComp <- as.numeric(weekDat[4,j])+(as.numeric(weekDat[5,j])/60)
      timesComp <- c(timesComp,timeComp)
    }
  }
  outFileName <- paste("GOES_Diurnal_",siteName,"_",yr,mth,dy,".csv",sep="")
  write.table(rbind(time.vals,NDVI.vals),outFileName,row.names = FALSE,col.names = FALSE,sep=",")
  plot(timesComp,NDVI.vals,ylab="NDVI",xlab="Time",main=paste(siteName,yr,mth,dy,sep=" "),ylim=c(0,1))
}
dev.off()

