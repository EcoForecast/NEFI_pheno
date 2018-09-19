#!/usr/bin/env Rscript

library("ncdf4")
library(plyr)
library("PhenologyBayesModeling")

createNDVI_GOES_diurnal <- function(lat,long,siteID,startDay,endDay){
  #load/calcuate GOES NDVI data
  startDay <- 182
  endDay <- 188
  lat <- 42.5378
  long <- -72.1715
  lat.rd <- lat*2*pi/360
  long.rd <- long*2*pi/360
  
  Ind2 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),2,orbitVersion="OLD")
  Ind3 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),3,orbitVersion="OLD")
  ACM.ind <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion="OLD"),"ACM",orbitVersion="OLD")
  
  i2 <- Ind2[1]
  j2 <- Ind2[2]
  i3 <- Ind3[1]
  j3 <- Ind3[2]
  
  NDVI.vals <- list()
  
  days <- seq(startDay,endDay,1)
  day.time.vals <- list()

  for (i in 1:length(days)){
    filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s2017",days[i],sep="")
    ACM.files <- dir(path="GOES_Data2017",pattern=filestrACM)
    for(j in 1:length(ACM.files)){
      day.time <- substr(ACM.files[j],24,34)
      print(day.time)
      day.time.vals <- c(day.time.vals,day.time)
      ACM.file <-nc_open(paste("GOES_Data2017/",ACM.files[j],sep=""))
      clouds <- ncvar_get(ACM.file,"BCM")[ACM.ind[1],ACM.ind[2]]
      filestrC03 <- paste("OR_ABI-L1b-RadC-M3C03_G16_s",day.time,sep="")
      filestrC02 <- paste("OR_ABI-L1b-RadC-M3C02_G16_s",day.time,sep="")
      R2.file <- nc_open(paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC02),sep=""))
      R3.file <- nc_open(paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC03),sep=""))
      R3.DQF <- ncvar_get(R3.file,"DQF")
      R2.DQF <- ncvar_get(R2.file,"DQF")
      if(R3.DQF[i3,j3]==0 & R2.DQF[i2,j2]==0 & R2.DQF[i2,j2]==0 & R2.DQF[(i2+1),j2]==0 & R2.DQF[i2,(j2+1)]==0 & R2.DQF[(i2+1),(j2+1)]==0 & clouds ==0){
        NDVI.val <- getSpecificNDVI(Ind2,Ind3,day.time)
      }
      else{
        NDVI.val <- NA
      }
      NDVI.vals <- c(NDVI.vals,NDVI.val)
    }
  }
  
  fileName <- paste("GOES_NDVI_Diurnal",siteID,"_kappaDQF.csv",sep="")
  output <- rbind(t(day.time.vals),NDVI.vals)
  write.table(output,file=fileName,sep=",",col.names=FALSE,row.names=FALSE)
}
createNDVI_GOES_diurnal(lat=42.5378, long=-72.1715, siteID="HarvardForest",startDay=182,endDay=188)

