#!/usr/bin/env Rscript 

source("ID_index.R")
source("calNDVI.R")
#install.packages("txtplot")
#library("txtplot")
library("ncdf4")
library(plyr)

calGOESNDVI <- function(lat,long,siteID,days=list(),year=2017){
  #load/calcuate GOES NDVI data

  lat.rd <- lat*2*pi/360
  long.rd <- long*2*pi/360

  Ind2 <- getDataIndex(getABI_Index(lat.rd,long.rd),2)
  Ind3 <- getDataIndex(getABI_Index(lat.rd,long.rd),3)
  NDVI.vals <- list()

  #days <- c(seq(182,207),209,seq(211,214),seq(216,220),seq(222,231),seq(233,264),seq(266,269),seq(271,295),seq(297,301),seq(303,314),seq(316,324),seq(328,333),348,seq(350,365))
  #days <- c(seq(182,207),209,seq(211,214),seq(216,220),seq(222,231),seq(233,264),seq(266,269),seq(271,295),seq(297,301),seq(303,324),seq(328,333),seq(348,365))
  #days <- c(seq(60,87),seq(89,93),seq(95,99),seq(101,103),seq(105,126),132,seq(134,136),seq(139,143),seq(146,154),seq(156,157),seq(159,161),seq(163,181))

  for (i in days){
    if(i<100){
    day.time <- paste(as.character(year),"0",i,"165",sep="")
    }
    else{
    day.time <- paste(as.character(year),i,"165",sep="")  
    }
    print(i)
    NDVI.val <- getSpecificNDVI(Ind2,Ind3,day.time)
    NDVI.vals <- c(NDVI.vals,NDVI.val)

  }

  fileName <- paste("Test_GOES_NDVI_",siteID,as.character(year),".csv",sep="")
  output <- rbind(t(days),NDVI.vals)
  write.table(output,file=fileName,sep=",",col.names=FALSE,row.names=FALSE)
}
#HarvFor.lat <- 42.5378 #Used for testing
#HarvFor.long <- -72.1715

#mainGraphing(HarvFor.lat,HarvFor.long)
siteData <- read.csv("GOES_Paper_Sites.csv",header=FALSE)

for (i in seq(1,nrow(siteData))){
  siteName <- siteData[i,1]
  lat <- siteData[i,2]
  long <-siteData[i,3]
  print(siteName)
  calGOESNDVI(lat,long,siteName)
}






