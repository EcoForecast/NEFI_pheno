#!/usr/bin/env Rscript

library("ncdf4")
library(plyr)

##' @param fileName Desired file name of output
##' @param lat Latitude
##' @param long Longitude
##' @param startDay The start day counted as the day number after 2016-12-31
##' @param endDay The end day counted as the day number after 2016-12-31
##' @param TZ The timezone off of UTC
createNDVI_GOES <- function(lat,long,startDay,endDay,fileName,TZ){
  print(startDay)
  print(endDay)
  #load/calcuate GOES NDVI data
  lat.rd <- as.numeric(lat)*2*pi/360
  long.rd <- as.numeric(long)*2*pi/360
  Tstr <- paste(as.character(12+as.numeric(TZ)),"0",sep="") #The time string, which changes based on the local time zone
  print(Tstr)

  Ind2 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),2,orbitVersion="OLD")
  Ind3 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),3,orbitVersion="OLD")
  ACM.ind <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),"ACM",orbitVersion="OLD")

  NDVI.vals <- numeric()
  days <- numeric()
  days1 <- seq(startDay,333)
  #days1 <- c(seq(110,126),132,seq(134,136),seq(139,143),seq(146,154),seq(156,157),seq(159,161),seq(163,181),seq(182,188),seq(191,200),seq(203,207),209,seq(211,214),216,seq(218,220),seq(222,231),seq(233,264),seq(266,269),seq(273,289),seq(291,295),seq(297,301),seq(303,314),seq(316,324),seq(328,333))

  for (i in days1){
    if(i<100){
      day.time <- paste("20170",i,Tstr,sep="")
    }
    else{
      day.time <- paste("2017",i,Tstr,sep="")
    }

    filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s",day.time,sep="")
    filepath <- paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrACM),sep="")
    if(file.exists(filepath) && !dir.exists(filepath)){
      ACM.file <-nc_open(filepath)

      print(day.time)
      clouds <- ncvar_get(ACM.file,"BCM")[ACM.ind[1],ACM.ind[2]] #BCM= binary cloud mask
      clouds.DQF <- ncvar_get(ACM.file,"DQF")[ACM.ind[1],ACM.ind[2]]
      if(!is.na(clouds) && !is.na(clouds.DQF)){
        if(clouds == 0 && clouds.DQF == 0){
          NDVI.val <- getSpecificNDVI(Ind2,Ind3,day.time)
          print(NDVI.val)
          NDVI.vals <- c(NDVI.vals,NDVI.val)
        }
        else{
          NDVI.vals <- c(NDVI.vals,NA)
        }
      }
      else{
        NDVI.vals <- c(NDVI.vals,NA)
      }
      days <- c(days,i)
    }
  }
  Ind2 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="NEW"),2,orbitVersion="NEW")
  Ind3 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="NEW"),3,orbitVersion="NEW")
  ACM.ind <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="NEW"),"ACM",orbitVersion="NEW")

  days2 <- seq(348,365,1)
  for (i in days2){
    day.time <- paste("2017",i,Tstr,sep="")

    filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s",day.time,sep="")
    filepath <- paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrACM),sep="")
    if(file.exists(filepath) && !dir.exists(filepath)){
      ACM.file <-nc_open(filepath)

      print(day.time)
      clouds <- ncvar_get(ACM.file,"BCM")[ACM.ind[1],ACM.ind[2]]
      clouds.DQF <- ncvar_get(ACM.file,"DQF")[ACM.ind[1],ACM.ind[2]]
      if(!is.na(clouds) && !is.na(clouds.DQF)){
        if(clouds == 0 && clouds.DQF == 0){
          NDVI.val <- getSpecificNDVI(Ind2,Ind3,day.time)
          NDVI.vals <- c(NDVI.vals,NDVI.val)
        }
        else{
          NDVI.vals <- c(NDVI.vals,NA)
        }
      }
      else{
        NDVI.vals <- c(NDVI.vals,NA)
      }
      days <- c(days,i)
    }
  }
  days3 <- c(seq(1,(endDay-365),1))

  for (i in days3){
    print(i)
    if(i<10) {
      day.time <- paste("201800",i,Tstr,sep="")
    }
    else if(i<100){
      day.time <- paste("20180",i,Tstr,sep="")
    }
    else{
      day.time <- paste("2018",i,Tstr,sep="")
    }

    filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s",day.time,sep="")
    filepath <- paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrACM),sep="")
    if(file.exists(filepath) && !dir.exists(filepath)){
      ACM.file <-nc_open(filepath)
      clouds <- ncvar_get(ACM.file,"BCM")[ACM.ind[1],ACM.ind[2]]
      clouds.DQF <- ncvar_get(ACM.file,"DQF")[ACM.ind[1],ACM.ind[2]]
      if(!is.na(clouds) && !is.na(clouds.DQF)){
        if(clouds == 0 && clouds.DQF == 0){
          NDVI.val <- getSpecificNDVI(Ind2,Ind3,day.time)
          NDVI.vals <- c(NDVI.vals,NDVI.val)
        }
        else{
          NDVI.vals <- c(NDVI.vals,NA)
        }
      }
      else{
        NDVI.vals <- c(NDVI.vals,NA)
      }
      days <- c(days,i)
    }
  }
  output <- rbind(t(days),NDVI.vals)
  write.table(output,file=fileName,sep=",",col.names=FALSE,row.names=FALSE)
}


