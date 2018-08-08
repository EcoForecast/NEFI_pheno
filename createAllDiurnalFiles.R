install.packaes("suncalc",repo="https://cloud.r-project.org/")
install.packaes("lubridate",repo="https://cloud.r-project.org/")
library("suncalc")
library("lubridate")

##Combine Files into one file
#NDVI.files <- dir(pattern="GOES_NDVI_DiurnalrussellSage_")
#lat <- 32.457
#long <- -91.9743
lat <- 45.2128
long <- -68.7418

#length(NDVI.files)
dat <- read.csv(NDVI.files[1],header=FALSE)
for(i in 2:length(NDVI.files)){
  print(NDVI.files[i])
  dat <- cbind(dat,read.csv(NDVI.files[i],header=FALSE))
}
dat <- dat[,order(dat[1,])]
#write.table(dat,file="GOES_NDVI_DiurnalHarvardForestALL.csv",row.names=FALSE,col.names=FALSE,sep=",")

##Need update based on the timezone difference
TZ=6
hr.seq <- c("00","01","02","03","04","05","06","07","08","09",as.character(seq(10,23)),"00","01","02","03","04","05","06")
weekDat <- rbind(as.matrix(dat),matrix(nrow=4,ncol=dim(dat)[2]))
rownames(weekDat) <- c("day.time","NDVI","day","hour","minutes","dayhr")
weekDat[3,] <- substr(weekDat[1,],5,7) #day
weekDat[4,] <- substr(weekDat[1,],8,9) #hour
weekDat[5,] <- substr(weekDat[1,],10,11) #minutes
weekDat[6,] <- paste(weekDat[3,],weekDat[4,],sep="") #dayhr
hr.seq2 <- hr.seq[TZ:(TZ+23)]

correctedDat <- weekDat
correctedDat[4,] <- as.numeric(weekDat[4,])-TZ
for(i in 1:ncol(correctedDat)){
  if(as.numeric(correctedDat[4,i])<0){
    #print(correctedDat[3,i])
    correctedDat[3,i] <- as.numeric(correctedDat[3,i])-1
    correctedDat[4,i] <- as.numeric(correctedDat[4,i]) + 24
    #print(correctedDat[4,i])
    #print(correctedDat[3,i])
  }
}
for(i in 1:ncol(correctedDat)){
    if(as.numeric(correctedDat[3,i])<10){
      print(correctedDat[3,i])
      correctedDat[3,i] <- as.character(paste("00",as.numeric(correctedDat[3,i]),sep=""))
      print(correctedDat[3,i])
    }
    else if(as.numeric(correctedDat[3,i])<100){
      correctedDat[3,i] <- as.character(paste("0",as.numeric(correctedDat[3,i]),sep=""))
    }
    else{
      correctedDat[3,i] <- as.character(correctedDat[3,i])
    }
    if(as.numeric(correctedDat[4,i])<10){
      correctedDat[4,i] <- as.character(paste("0",as.numeric(correctedDat[4,i]),sep=""))
    }
    else{
      correctedDat[4,i] <- as.character(correctedDat[4,i])
    }
}

correctedDat[6,] <- paste(correctedDat[3,],correctedDat[4,],sep="") #dayhr
correctedDat[1,] <- paste(substr(correctedDat[1,],1,4),correctedDat[6,],substr(correctedDat[1,],10,11),sep="")
for(q in 1:ncol(correctedDat)){
  if(!is.na(correctedDat[2,q])){
  if(round(as.numeric(correctedDat[2,q]),4)==0.6040 | as.numeric(correctedDat[2,q])<0 | as.numeric(correctedDat[2,q])>0.9){
    #print(correctedDat[2,q])
    correctedDat[2,q] <- NA
  }
  }
}

#pdf(file="SingleDailyNDVI_HarvardForest_OLD.pdf",width=30,height=45)
#pdf(file="SingleDailyNDVI_russellSage.pdf",width=30,height=45)
pdf(file="SingleDailyNDVI_howland.pdf",width=30,height=45)

par(mfrow=c(10,10))
current.dy <- "181"
current.yr.dy <- "2017181"
time.vals <- character()
NDVI.vals <- numeric()
timesComp <- numeric()
for(i in 1:ncol(correctedDat)){
  dy <- substr(correctedDat[1,i],5,7)
  yr.dy <- substr(correctedDat[1,i],1,7)
  print(paste("yr.dy",yr.dy))
  print(paste("current.dy",current.dy))
  if(as.character(yr.dy)==as.character(current.yr.dy)){
    time.vals <- c(time.vals,correctedDat[1,i])
    NDVI.vals <- c(NDVI.vals,correctedDat[2,i])
    timeComp <- as.numeric(correctedDat[4,i])+(as.numeric(correctedDat[5,i])/60)
    timesComp <- c(timesComp,timeComp)
  }
  else{
    dayData <- rbind(time.vals,NDVI.vals,timesComp)
    #print(dim(dayData))
    yr <- substr(current.yr.dy,1,4)
    dy <- substr(current.yr.dy,5,7)
    date.val <- as.Date(as.numeric(dy),origin=as.Date(paste(as.character(as.numeric(yr)-1),"-12-31",sep="")))
    suntimes <- getSunlightTimes(date=date.val,lat=lat,lon=long,keep=c("nauticalDawn","nauticalDusk"),tz = "America/Chicago")
    #print(suntimes)
    dawnTime <- hour(suntimes$nauticalDawn)+(minute(suntimes$nauticalDawn)/60)
    duskTime <- hour(suntimes$nauticalDusk)+(minute(suntimes$nauticalDusk)/60)
    for(d in 1:ncol(dayData)){
        if((as.numeric(dayData[3,d]) < dawnTime) || (as.numeric(dayData[3,d]) > duskTime)){
          dayData[2,d] <- NA
        }
    }
    #outFileName <- paste("dailyNDVI_GOES/GOES_Diurnal_russellSage_",current.yr.dy,".csv",sep="")
    outFileName <- paste("dailyNDVI_GOES/GOES_Diurnal_howland_",current.yr.dy,".csv",sep="")
    write.table(dayData,outFileName,row.names = FALSE,col.names = FALSE,sep=",")
    plot(dayData[3,],dayData[2,],ylab="NDVI",xlab="Time",main=paste("Howland",current.yr.dy,sep=" "),ylim=c(0,1),xlim=c(0,24))
    abline(v=dawnTime,col="green")
    abline(v=duskTime,col="green")
    #print(current.yr.dy)
    current.dy <- substr(correctedDat[1,i],5,7)
    current.yr.dy <- substr(correctedDat[1,i],1,7)
    #print(current.yr.dy)
    time.vals <- character()
    NDVI.vals <- numeric()
    timesComp <- numeric()
  }
}
dayData <- rbind(time.vals,NDVI.vals,timesComp)
#print(dim(dayData))
yr <- substr(current.yr.dy,1,4)
dy <- substr(current.yr.dy,5,7)
date.val <- as.Date(as.numeric(dy),origin=as.Date(paste(as.character(as.numeric(yr)-1),"-12-31",sep="")))
suntimes <- getSunlightTimes(date=date.val,lat=lat,lon=long,keep=c("nauticalDawn","nauticalDusk"),tz = "America/Chicago")
#print(suntimes)
dawnTime <- hour(suntimes$nauticalDawn)+(minute(suntimes$nauticalDawn)/60)
duskTime <- hour(suntimes$nauticalDusk)+(minute(suntimes$nauticalDusk)/60)
for(d in 1:ncol(dayData)){
  if((as.numeric(dayData[3,d]) < dawnTime) || (as.numeric(dayData[3,d]) > duskTime)){
    dayData[2,d] <- NA
  }
}
#outFileName <- paste("dailyNDVI_GOES/GOES_Diurnal_russellSage_",current.yr.dy,".csv",sep="")
outFileName <- paste("dailyNDVI_GOES/GOES_Diurnal_howland_",current.yr.dy,".csv",sep="")
write.table(dayData,outFileName,row.names = FALSE,col.names = FALSE,sep=",")
plot(dayData[3,],dayData[2,],ylab="NDVI",xlab="Time",main=paste("Howland",current.yr.dy,sep=" "),ylim=c(0,1),xlim=c(0,24))
abline(v=dawnTime,col="green")
abline(v=duskTime,col="green")

print(current.yr.dy)
dev.off()
