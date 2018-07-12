#!/usr/bin/env Rscript

library("ncdf4")
library(plyr)

##' @param fileName Desired file name of output
##' @param lat Latitude
##' @param long Longitude
##' @param startDay The start day counted as the day number after 2016-12-31
##' @param endDay The end day counted as the day number after 2016-12-31
##' @param TZ The timezone off of UTC
createNDVI_GOES_Avg <- function(lat,long,startDay,endDay,fileName,TZ){
  print(startDay)
  print(endDay)
  #load/calcuate GOES NDVI data
  lat.rd <- as.numeric(lat)*2*pi/360
  long.rd <- as.numeric(long)*2*pi/360
  startT <- as.numeric(paste(as.character(10+as.numeric(TZ)),"0",sep=""))
  endT <- as.numeric(paste(as.character(14+as.numeric(TZ)),"0",sep=""))

  Ind2 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),2,orbitVersion="OLD")
  Ind3 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),3,orbitVersion="OLD")
  ACM.ind <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),"ACM",orbitVersion="OLD")

  NDVI.vals <- numeric()
  NDVI.vars <- numeric()
  days <- numeric()
  #days1 <- seq(startDay,333)
  days1 <- seq(200,333)

  for (i in days1){
    NDVI.day.vals <- numeric()
    if(i<100){
      day <- paste("20170",i,sep="")
    }
    else{
      day <- paste("2017",i,sep="")
    }
    for(j in seq(startT,endT,1)){
      day.hr <- paste(as.character(day),j,sep="")
      print(day.hr)
      filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s",day.hr,sep="")
      ACMfiles <- dir(path="GOES_Data2017",pattern=filestrACM)
      print(length(ACMfiles))
      for(f in ACMfiles){
        day.time <- substr(f,24,34)
        print(day.time)
        ACM.file <-nc_open(paste("GOES_Data2017/",f,sep=""))
        clouds <- ncvar_get(ACM.file,"BCM")[ACM.ind[1],ACM.ind[2]]
        if(!is.na(clouds)){
          if(clouds == 0){
            NDVI.val <- getSpecificNDVI(Ind2,Ind3,day.time)
            #print(NDVI.val)
            NDVI.day.vals <- c(NDVI.day.vals,NDVI.val)
          }
          else{
            NDVI.day.vals <- c(NDVI.day.vals,NA)
          }
        }
        else{
          NDVI.day.vals <- c(NDVI.day.vals,NA)
        }
      }
    }
    NDVI.vals <- c(NDVI.vals,mean(NDVI.day.vals,na.rm = TRUE))
    NDVI.vars <- c(NDVI.vars,var(NDVI.day.vals,na.rm = TRUE))
    days <- c(days,i)
  }

   # Ind2 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="NEW"),2,orbitVersion="NEW")
   # Ind3 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="NEW"),3,orbitVersion="NEW")
   # ACM.ind <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="NEW"),"ACM",orbitVersion="NEW")
   #
   # days2 <- seq(348,365,1)
   #
   # for (i in days2){
   #   NDVI.day.vals <- numeric()
   #   if(i<100){
   #     day <- paste("20170",i,sep="")
   #   }
   #   else{
   #     day <- paste("2017",i,sep="")
   #   }
   #   for(j in seq(startT,endT,1)){
   #     day.hr <- paste(as.character(day),j,sep="")
   #     print(day.hr)
   #     filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s",day.hr,sep="")
   #     ACMfiles <- dir(path="GOES_Data2017",pattern=filestrACM)
   #     print(length(ACMfiles))
   #     for(f in ACMfiles){
   #       day.time <- substr(f,24,34)
   #       print(day.time)
   #       ACM.file <-nc_open(paste("GOES_Data2017/",f,sep=""))
   #       clouds <- ncvar_get(ACM.file,"BCM")[ACM.ind[1],ACM.ind[2]]
   #       if(!is.na(clouds)){
   #         if(clouds == 0){
   #           NDVI.val <- getSpecificNDVI(Ind2,Ind3,day.time)
   #           #print(NDVI.val)
   #           NDVI.day.vals <- c(NDVI.day.vals,NDVI.val)
   #         }
   #         else{
   #           NDVI.day.vals <- c(NDVI.day.vals,NA)
   #         }
   #       }
   #       else{
   #         NDVI.day.vals <- c(NDVI.day.vals,NA)
   #       }
   #     }
   #   }
   #   NDVI.vals <- c(NDVI.vals,mean(NDVI.day.vals,na.rm = TRUE))
   #   NDVI.vars <- c(NDVI.vars,var(NDVI.day.vals,na.rm = TRUE))
   #   days <- c(days,i)
   # }
   #
   # days3 <- c(seq(1,(endDay-365),1))
   #
   # for (i in days3){
   #   NDVI.day.vals <- numeric()
   #   if(i<100){
   #     day <- paste("20180",i,sep="")
   #   }
   #   else{
   #     day <- paste("2018",i,sep="")
   #   }
   #   for(j in seq(startT,endT,1)){
   #     day.hr <- paste(as.character(day),j,sep="")
   #     print(day.hr)
   #     filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s",day.hr,sep="")
   #     ACMfiles <- dir(path="GOES_Data2017",pattern=filestrACM)
   #     print(length(ACMfiles))
   #     for(f in ACMfiles){
   #       day.time <- substr(f,24,34)
   #       print(day.time)
   #       ACM.file <-nc_open(paste("GOES_Data2017/",f,sep=""))
   #       clouds <- ncvar_get(ACM.file,"BCM")[ACM.ind[1],ACM.ind[2]]
   #       if(!is.na(clouds)){
   #         if(clouds == 0){
   #           NDVI.val <- getSpecificNDVI(Ind2,Ind3,day.time)
   #           #print(NDVI.val)
   #           NDVI.day.vals <- c(NDVI.day.vals,NDVI.val)
   #         }
   #         else{
   #           NDVI.day.vals <- c(NDVI.day.vals,NA)
   #         }
   #       }
   #       else{
   #         NDVI.day.vals <- c(NDVI.day.vals,NA)
   #       }
   #     }
   #   }
   #   NDVI.vals <- c(NDVI.vals,mean(NDVI.day.vals,na.rm = TRUE))
   #   NDVI.vars <- c(NDVI.vars,var(NDVI.day.vals,na.rm = TRUE))
   #   days <- c(days,i)
   # }

  output <- rbind(t(days),NDVI.vals,NDVI.vars)
  write.table(output,file=fileName,sep=",",col.names=FALSE,row.names=FALSE)
}


