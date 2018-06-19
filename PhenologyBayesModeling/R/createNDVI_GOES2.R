#!/usr/bin/env Rscript

library("ncdf4")
library(plyr)

createNDVI_GOES2 <- function(lat,long,siteID){
  #load/calcuate GOES NDVI data
  lat.rd <- lat*2*pi/360
  long.rd <- long*2*pi/360

  days <- seq(61,109)
  NDVI.vals <- list()
  for (i in days){
    print(i)
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

    clouds <- ncvar_get(ACM.file,"BCM")[ACM.ind[1],ACM.ind[2]]
    if(clouds == 0){
      NDVI.val <- getSpecificNDVI(Ind2,Ind3,day.time)
      NDVI.vals <- c(NDVI.vals,NDVI.val)
    }
    else{
      NDVI.vals <- c(NDVI.vals,NA)
    }

  }

  fileName <- paste("GOES_NDVI_",siteID,"_kappaDQFSpring2018.csv",sep="")
  output <- rbind(t(days),NDVI.vals)
  write.table(output,file=fileName,sep=",",col.names=FALSE,row.names=FALSE)
}

