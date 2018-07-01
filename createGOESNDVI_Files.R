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
  GOES_data(siteName,lat,long,startDay,endDay,TZ)
}

