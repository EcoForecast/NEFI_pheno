#!/usr/bin/env Rscript

install.packages("PhenologyBayesModeling",repo=NULL)
library("ncdf4")
library(plyr)
library("PhenologyBayesModeling")

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
