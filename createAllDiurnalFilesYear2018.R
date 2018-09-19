#!/usr/bin/env Rscript

library("ncdf4")
library(plyr)
library("PhenologyBayesModeling")
#library(doParallel)

#detect cores.
#n.cores <- detectCores()
n.cores <- 6

#register the cores.
registerDoParallel(cores=n.cores)


createNDVI_GOES_diurnal <- function(lat,long,siteID,startDay,endDay,orbitVersion){
  #load/calcuate GOES NDVI data
  lat.rd <- lat*2*pi/360
  long.rd <- long*2*pi/360
  
  Ind2 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),2,orbitVersion=orbitVersion)
  Ind3 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),3,orbitVersion=orbitVersion)
  ACM.ind <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),"ACM",orbitVersion=orbitVersion)
  
  i2 <- Ind2[1]
  j2 <- Ind2[2]
  i3 <- Ind3[1]
  j3 <- Ind3[2]
  
  NDVI.vals <- list()
  
  days <- seq(startDay,endDay,1)
  day.time.vals <- list()
  for(i in 1:length(days)){
    #print(days)
    days[i] <- as.numeric(days[i])
    print(days[i])
    if(as.numeric(days[i]) < 10){
      days[i] <- paste("00",as.character(days[i]),sep="")
    }
    else if(as.numeric(days[i]) < 100){
      days[i] <- paste("0",as.character(days[i]),sep="")
      print(days[i])
    }
  }
  for (i in 1:length(days)){
    print(days[i])
    #days[i] <- as.character(days[i])
    filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s2018",days[i],sep="")
    print(filestrACM)
    ACM.files <- dir(path="GOES_Data2017",pattern=filestrACM)
    print(length(ACM.files))
    if(!dir.exists((paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrACM),sep="")))){
      if(length(ACM.files>1)){
        for(j in 1:length(ACM.files)){
          day.time <- substr(ACM.files[j],24,34)
          #print(j)
          print(day.time)
          day.time.vals <- c(day.time.vals,day.time)
          filePath <- paste("GOES_Data2017/",ACM.files[j],sep="")
          #print(filePath)
          ACM.file <-nc_open(paste("GOES_Data2017/",ACM.files[j],sep=""))
          #print(dim(ncvar_get(ACM.file, "BCM")))
          #print(ACM.ind)
          clouds <- ncvar_get(ACM.file,"BCM")[ACM.ind[1],ACM.ind[2]]
          if(!is.na(clouds)){
            if (clouds ==0){
              filestrC03 <- paste("OR_ABI-L1b-RadC-M3C03_G16_s",day.time,sep="")
              filestrC02 <- paste("OR_ABI-L1b-RadC-M3C02_G16_s",day.time,sep="")
              filePathC02 <- paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC02),sep="")
              filePathC03 <- paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC03),sep="")
              if(nchar(filePathC02)>20 & nchar(filePathC03)>20){
                R2.file <- nc_open(paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC02),sep=""))
                R3.file <- nc_open(paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC03),sep=""))
                R3.DQF <- ncvar_get(R3.file,"DQF")
                R2.DQF <- ncvar_get(R2.file,"DQF")
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
          NDVI.vals <- c(NDVI.vals,NDVI.val)
        }
      }
    }
  }
  
  fileName <- paste("GOES_NDVI_Diurnal",siteID,"_",startDay,"_",endDay,"_kappaDQF.csv",sep="")
  output <- rbind(t(day.time.vals),NDVI.vals)
  write.table(output,file=fileName,sep=",",col.names=FALSE,row.names=FALSE)
}


siteData <- read.csv("GOES_Paper_Sites.csv",header=TRUE)
siteName <- as.character(siteData[1,1])
lat <- as.numeric(siteData[1,2])
long <- as.numeric(siteData[1,3])

timeFrames <- matrix(ncol=3,nrow=1)
timeFrames[1,] <- c(152,158,"NEW") #Jan1
# timeFrames[2,] <- c(14,19,"NEW") #Jan1
# timeFrames[3,] <- c(20,25,"NEW") #Jan2
# timeFrames[4,] <- c(26,31,"NEW") #Jan2
# 
# timeFrames[5,] <- c(39,44,"NEW")
# timeFrames[6,] <- c(45,49,"NEW")
# timeFrames[7,] <- c(50,54,"NEW") #Feb 2
# timeFrames[8,] <- c(55,59,"NEW") #Feb 2
# 
# timeFrames[9,] <- c(67,72,"NEW")
# timeFrames[10,] <- c(73,79,"NEW")
# timeFrames[11,] <- c(80,84,"NEW") #March 2
# timeFrames[12,] <- c(85,90,"NEW") #March 2


# timeFrames[1,] <- c(8,20,"NEW") #Jan1
# timeFrames[2,] <- c(20,31,"NEW") #Jan2
# 
# timeFrames[3,] <- c(39,49,"NEW")
# timeFrames[4,] <- c(50,59,"NEW") #Feb 2
# 
# timeFrames[5,] <- c(67,79,"NEW")
# timeFrames[6,] <- c(80,90,"NEW") #March 2
# 
# timeFrames[7,] <- c(98,109,"NEW")
# timeFrames[8,] <- c(110,120,"NEW") #April 2
# 
# timeFrames[9,] <- c(128,139,"NEW")
# timeFrames[10,] <- c(140,151,"NEW") #May 2
# 
# timeFrames[11,] <- c(152,158,"NEW")
# timeFrames[12,] <- c(159,170,"NEW")
# timeFrames[13,] <- c(171,181,"NEW") #June 2

# timeFrames <- matrix(ncol=2,nrow=5)
# timeFrames[1,] <- c(189,189) #July1
# #timeFrames[2,] <- c(201,202) #July2
# 
# timeFrames[2,] <- c(220,220)
# #timeFrames[4,] <- c(232,233) #Aug 2
# 
# timeFrames[3,] <- c(251,251)
# #timeFrames[6,] <- c(262,263) #Sept 2
# 
# timeFrames[4,] <- c(312,312)
# #timeFrames[8,] <- c(322,323) #Nov 2
# 
# timeFrames[5,] <- c(342,342)
# #timeFrames[10,] <- c(352,353) #Jan 2

output <- foreach(i = 1:1) %dopar% {
  startDay <- timeFrames[i,1]
  endDay <- timeFrames[i,2]
  orbitVersion <- timeFrames[i,3]
  print(timeFrames[i,])
  createNDVI_GOES_diurnal(lat=lat, long=long, siteID=siteName,startDay=startDay,endDay=endDay,orbitVersion=orbitVersion)
  print(paste(i, "done",sep=" "))
}