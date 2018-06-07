#!/usr/bin/env Rscript

library("ncdf4")
library(plyr)
library("PhenologyBayesModeling")

siteData <- read.csv("GOES_Paper_Sites.csv",header=FALSE)
startDay <- 110
endDay <- 424

#iseq <- c(7,15,18,19,20,21)
iseq <- c(20,21)
for(i in iseq){
  siteName <- as.character(siteData[i,1])
  print(siteName)
  lat <- as.numeric(siteData[i,2])
  long <- as.numeric(siteData[i,3])
  #print(c(lat,long))
  createNDVI_GOES(lat=lat,long=long,siteID=siteName)
}
