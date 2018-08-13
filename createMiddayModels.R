#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
library("ncdf4")
library(plyr)
library("PhenologyBayesModeling")
library("rjags")
library("runjags")

startDay <- 182
endDay <- 181+365
siteData <- read.csv("GOES_Paper_Sites.csv",header=TRUE)
iseq <- c(9,11)
i=11
for(i in iseq){
  siteName <- as.character(siteData[i,1])
  print(siteName)
  TZ <- as.numeric(siteData[i,6])
  lat <- as.numeric(siteData[i,2])
  long <- as.numeric(siteData[i,3])
  j.model <- createBayesModel.DB_Avg(siteName=siteName,startDay = startDay,endDay=endDay,lat=lat,long=long,TZ=TZ)
  var.Burn <- runMCMC_Model(j.model = j.model, variableNames = c("TranS","bS","TranF","bF","d","c","k","prec"))
  outFileName <- paste(siteName,"_Midday_varBurn.RData",sep="")
  save(var.Burn,file=outFileName)
}