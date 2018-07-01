#!/usr/bin/env Rscript

install.packages("PhenologyBayesModeling",repo=NULL)
library("ncdf4")
library(plyr)
library("PhenologyBayesModeling")

siteData <- read.csv("GOES_Paper_Sites.csv",header=TRUE)

for(i in 1:nrow(siteData)){
  siteName <- as.character(siteData$siteName[i])
  print(siteName)
  URL <- as.character(siteData$URL[i])
  PFT <- as.character(siteData$PFT[i])
  lat <- as.character(siteData$Lat[i])
  long <- as.character(siteData$Long[i])
  TZ <- as.character(siteData$TZ[i])
  if(PFT=="SH"){
    startDay <- 110
    endDay <- 455
  }
  else if(PFT=="DB"){
    startDay <- 152
    endDay <- 546
  }
  GOES_data <- function(siteName,lat,long,startDay,endDay,TZ)
}





siteData <- read.csv("GOES_Paper_Sites.csv",header=FALSE)
startDay <- 110
endDay <- 474

iseq <- c(9,14,17)
#iseq <- c(1,2,3)
#iseq <- c(4,5,6)
#iseq <- c(8,10,12,13)
for(i in iseq){
  siteName <- as.character(siteData[i,1])
  print(siteName)
  lat <- as.numeric(siteData[i,2])
  long <- as.numeric(siteData[i,3])
  #print(c(lat,long))
  createNDVI_GOES(lat=lat,long=long,siteID=siteName)
}
