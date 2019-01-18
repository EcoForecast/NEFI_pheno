#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
library("ncdf4")
library(plyr)
library("PhenologyBayesModeling")
library(doParallel)

#detect cores.
#n.cores <- detectCores()
n.cores <- 4

#register the cores.
registerDoParallel(cores=n.cores)


createNDVI_GOES_midday <- function(lat,long,siteID,startDay,endDay,orbitVersion,TZ){
  #load/calcuate GOES NDVI data
  lat.rd <- lat*2*pi/360
  long.rd <- long*2*pi/360
  
  Ind2 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion=orbitVersion),2,orbitVersion=orbitVersion)
  Ind3 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion=orbitVersion),3,orbitVersion=orbitVersion)
  ACM.ind <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion=orbitVersion),"ACM",orbitVersion=orbitVersion)
  
  i2 <- Ind2[1]
  j2 <- Ind2[2]
  i3 <- Ind3[1]
  j3 <- Ind3[2]
  
  NDVI.vals <- list()
  
  days <- seq(startDay,endDay,1)
  day.time.vals <- list()
  hrs <- as.character(c(seq((4+TZ),(9+TZ),1),seq((15+TZ),(17+TZ),1))) 
  #hrs <- seq((10+TZ),(14+TZ),1)
  for(i in 1:length(days)){
    #print(days)
    days[i] <- as.numeric(days[i])
    if(as.numeric(days[i]) < 10){
      days[i] <- paste("00",as.character(days[i]),sep="")
    }
    else if(as.numeric(days[i]) < 100){
      days[i] <- paste("0",as.character(days[i]),sep="")
    }
  }
  for (i in 1:length(days)){
    print(days[i])
    days[i] <- as.character(days[i])
    filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s2018",days[i],sep="")
    ACM.files <- character()
    for(q in 1:length(hrs)){
      if(hrs[q]<10){
        hrs[q] <- paste("0",hrs[q],sep="")
      }
      print(c("hrs[q]",hrs[q]))
      newFiles <- intersect(dir(path="GOES_Data2017",pattern=filestrACM),dir(path="GOES_Data2017",pattern=paste("s2018",days[i],hrs[q],sep="")))
      ACM.files <- c(ACM.files,newFiles)
    }
    #ACM.files <- c(intersect(dir(path="GOES_Data2017",pattern=filestrACM),dir(path="GOES_Data2017",pattern=paste("s2017",days[i],(TZ+10),sep=""))),intersect(dir(path="GOES_Data2017",pattern=filestrACM),dir(path="GOES_Data2017",pattern=paste("s2017",days[i],(TZ+11),sep=""))),intersect(dir(path="GOES_Data2017",pattern=filestrACM),dir(path="GOES_Data2017",pattern=paste("s2017",days[i],(TZ+12),sep=""))),intersect(dir(path="GOES_Data2017",pattern=filestrACM),dir(path="GOES_Data2017",pattern=paste("s2017",days[i],(TZ+13),sep=""))))
    if(!dir.exists((paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrACM),sep="")))){
      if(length(ACM.files>1)){
        for(j in 1:length(ACM.files)){
          day.time <- substr(ACM.files[j],24,34)
          day.time.vals <- c(day.time.vals,day.time)
          filePath <- paste("GOES_Data2017/",ACM.files[j],sep="")
          ACM.file <-nc_open(paste("GOES_Data2017/",ACM.files[j],sep=""))
          clouds <- ncvar_get(ACM.file,"BCM")[ACM.ind[1],ACM.ind[2]]
          clouds.DQF <- ncvar_get(ACM.file,"DQF")[ACM.ind[1],ACM.ind[2]]
          if(!is.na(clouds) && !is.na(clouds.DQF)){
            if(clouds == 0 && clouds.DQF == 0){
              filestrC03 <- paste("OR_ABI-L1b-RadC-M3C03_G16_s",day.time,sep="")
              filestrC02 <- paste("OR_ABI-L1b-RadC-M3C02_G16_s",day.time,sep="")
              filePathC02 <- paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC02),sep="")
              filePathC03 <- paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC03),sep="")
              if(nchar(filePathC02)>20 & nchar(filePathC03)>20){
                R2.file <- nc_open(paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC02),sep=""))
                R3.file <- nc_open(paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC03),sep=""))
                R3.DQF <- ncvar_get(R3.file,"DQF")
                R2.DQF <- ncvar_get(R2.file,"DQF")
                if(!is.na(R3.DQF[i3,j3]) & !is.na(R2.DQF[i2,j2]) & !is.na(R2.DQF[i2,j2]) & !is.na(R2.DQF[(i2+1),j2]) & !is.na(R2.DQF[i2,(j2+1)]) & !is.na(R2.DQF[(i2+1),(j2+1)])){
                  if(R3.DQF[i3,j3]==0 & R2.DQF[i2,j2]==0 & R2.DQF[i2,j2]==0 & R2.DQF[(i2+1),j2]==0 & R2.DQF[i2,(j2+1)]==0 & R2.DQF[(i2+1),(j2+1)]==0){
                    NDVI.val <- getSpecificNDVI(Ind2,Ind3,day.time)
                  }
                  else{
                    NDVI.val <- NA
                  }
                }
                else{
                  NDVI.val <- NA
                }
              }
              else{
                NDVI.val <- NA
              }
            }
            else{
              NDVI.val <- NA
            }
          }
          else{
            NDVI.val <- NA
          }
          NDVI.vals <- c(NDVI.vals,NDVI.val)
        }
      }
    }
  }
  
  fileName <- paste("GOES_NDVI_Leftover",siteID,"_",startDay,"_",endDay,"_kappaDQF.csv",sep="")
  output <- rbind(t(day.time.vals),NDVI.vals)
  write.table(output,file=fileName,sep=",",col.names=FALSE,row.names=FALSE)
}


siteData <- read.csv("GOES_Paper_Sites.csv",header=TRUE)
num <- 6
siteName <- as.character(siteData[num,1])
lat <- as.numeric(siteData[num,2])
long <- as.numeric(siteData[num,3])
TZ <- as.numeric(siteData[num,6])

timeFrames <- matrix(ncol=3,nrow=25)

timeFrames[1,] <- c(182,186,"OLD")
timeFrames[2,] <- c(187,192,"OLD")
timeFrames[3,] <- c(193,200,"OLD")
timeFrames[4,] <- c(201,207,"OLD")
timeFrames[5,] <- c(208,213,"OLD")
timeFrames[6,] <- c(214,220,"OLD")
timeFrames[7,] <- c(221,228,"OLD")
timeFrames[8,] <- c(229,234,"OLD")
timeFrames[9,] <- c(235,240,"OLD")
timeFrames[10,] <- c(241,246,"OLD")
timeFrames[11,] <- c(247,252,"OLD")
timeFrames[12,] <- c(253,259,"OLD")

timeFrames[13,] <- c(260,266,"OLD")
timeFrames[14,] <- c(267,272,"OLD")
timeFrames[15,] <- c(273,278,"OLD")
timeFrames[16,] <- c(279,283,"OLD")
timeFrames[17,] <- c(284,289,"OLD")
timeFrames[18,] <- c(290,295,"OLD")
timeFrames[19,] <- c(296,301,"OLD")
timeFrames[20,] <- c(302,306,"OLD")
timeFrames[21,] <- c(307,312,"OLD")
timeFrames[22,] <- c(313,320,"OLD")

timeFrames[23,] <- c(348,352,"NEW")
timeFrames[24,] <- c(353,360,"NEW")
timeFrames[25,] <- c(361,365,"NEW")

output <- foreach(i = 1:nrow(timeFrames)) %dopar% {
  # for(i in 1:nrow(timeFrames)){
  startDay <- timeFrames[i,1]
  endDay <- timeFrames[i,2]
  orbitVersion <- timeFrames[i,3]
  print(timeFrames[i,])
  createNDVI_GOES_midday(lat=lat, long=long, siteID=siteName,startDay=startDay,endDay=endDay,orbitVersion = orbitVersion,TZ=TZ)
  print(paste(i, "done",sep=" "))
}
