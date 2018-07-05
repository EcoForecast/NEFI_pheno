#!/usr/bin/env Rscript

library("ncdf4")
library(plyr)

createNDVI_GOES_OLD <- function(lat,long,siteID){
  #load/calcuate GOES NDVI data
  lat.rd <- lat*2*pi/360
  long.rd <- long*2*pi/360

  Ind2 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),2,orbitVersion="OLD")
  Ind3 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),3,orbitVersion="OLD")
  ACM.ind <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),"ACM",orbitVersion="OLD")

  NDVI.vals <- list()

  days1 <- c(seq(110,126),132,seq(134,136),seq(139,143),seq(146,154),seq(156,157),seq(159,161),seq(163,181),seq(182,188),seq(191,200),seq(203,207),209,seq(211,214),216,seq(218,220),seq(222,231),seq(233,264),seq(266,269),seq(271,289),seq(291,295),seq(297,301),seq(303,314),seq(316,324),seq(328,333))

  for (i in days1){
    if(i<100){
      day.time <- paste("20170",i,"165",sep="")
    }
    else{
      day.time <- paste("2017",i,"165",sep="")
    }


    filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s",day.time,sep="")
    ACM.file <-nc_open(paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrACM),sep=""))

    print(i)
    clouds <- ncvar_get(ACM.file,"BCM")[ACM.ind[1],ACM.ind[2]]
    if(clouds == 0){
      NDVI.val <- getSpecificNDVI(Ind2,Ind3,day.time)
      NDVI.vals <- c(NDVI.vals,NDVI.val)
    }
    else{
      NDVI.vals <- c(NDVI.vals,NA)
    }

  }
  Ind2 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="NEW"),2,orbitVersion="NEW")
  Ind3 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="NEW"),3,orbitVersion="NEW")
  ACM.ind <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="NEW"),"ACM",orbitVersion="NEW")

  days2 <- c(348,seq(350,365))
  for (i in days2){
    if(i<100){
      day.time <- paste("20170",i,"165",sep="")
    }
    else{
      day.time <- paste("2017",i,"165",sep="")
    }


    filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s",day.time,sep="")
    ACM.file <-nc_open(paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrACM),sep=""))

    print(i)
    clouds <- ncvar_get(ACM.file,"BCM")[ACM.ind[1],ACM.ind[2]]
    if(clouds == 0){
      NDVI.val <- getSpecificNDVI(Ind2,Ind3,day.time)
      NDVI.vals <- c(NDVI.vals,NDVI.val)
    }
    else{
      NDVI.vals <- c(NDVI.vals,NA)
    }

  }
  days3 <- seq(1,60)

  for (i in days3){
    if(i<10) {
      day.time <- paste("201800",i,"165",sep="")
    }
    else if(i<100){
      day.time <- paste("20180",i,"165",sep="")
    }
    else{
      day.time <- paste("2018",i,"165",sep="")
    }

    filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s",day.time,sep="")
    ACM.file <-nc_open(paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrACM),sep=""))

    print(i)
    clouds <- ncvar_get(ACM.file,"BCM")[ACM.ind[1],ACM.ind[2]]
    if(clouds == 0){
      NDVI.val <- getSpecificNDVI(Ind2,Ind3,day.time)
      NDVI.vals <- c(NDVI.vals,NDVI.val)
    }
    else{
      NDVI.vals <- c(NDVI.vals,NA)
    }

  }

  fileName <- paste("GOES_NDVI_",siteID,"_kappaDQF.csv",sep="")
  output <- rbind(t(days),NDVI.vals)
  write.table(output,file=fileName,sep=",",col.names=FALSE,row.names=FALSE)
}
