#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
library("ncdf4")
library(plyr)
library("PhenologyBayesModeling")
library(doParallel)


#siteName <- "russellSage"
siteName <- "HarvardForest"
dayFiles <- dir(path="dailyNDVI_GOES",pattern=siteName)
TZ <- 6
days <- numeric()
NDVI.means <- numeric()
NDVI.precs <- numeric()
counts <- numeric()
for(i in 2:length(dayFiles)){
  print(dayFiles[i])
  dat <- read.csv(paste("dailyNDVI_GOES/",dayFiles[i],sep=""),header=FALSE)
  hrs <- as.numeric(substr(as.character(dat[1,]),8,9))
  mid.dat <- cbind(dat[,(hrs==(TZ+10))],dat[,(hrs==(TZ+11))],dat[,(hrs==(TZ+12))],dat[,(hrs==(TZ+13))])
  #dy <- as.numeric(substr(dayFiles[i],26,28))
  dy <- as.numeric(substr(dayFiles[i],30,32))
  print(dy)
  if(dy<182){
    dy <- as.numeric(dy)+365
  }
  days <- c(days,dy)
  NDVI.means <- c(NDVI.means,mean(as.numeric(mid.dat[2,]),na.rm=TRUE))
  NDVI.precs <- c(NDVI.precs,1/(var(as.numeric(mid.dat[2,]),na.rm=TRUE)))
  counts <- c(counts,sum(!is.na(mid.dat[2,])))
}

startDate <- "2017-07-01"
endDate <- "2018-06-30"
final.dat <- rbind(days,NDVI.means,NDVI.precs,counts)
outFileName <- paste("GOES_NDVI_",siteName,"_",startDate,"_",endDate,"_Avg.csv",sep="")
write.table(final.dat,file=outFileName,row.names=FALSE,col.names = FALSE,sep=",")
