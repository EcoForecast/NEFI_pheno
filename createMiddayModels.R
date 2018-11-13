#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
library("ncdf4")
library(plyr)
library("PhenologyBayesModeling")
library("rjags")
library("runjags")
library(doParallel)

#detect cores.
n.cores <- 4

#register the cores.
registerDoParallel(cores=n.cores)

#startDay <- 182
#endDay <- 181+365
siteData <- read.csv("GOES_Paper_Sites.csv",header=TRUE)
#iseq <- c(18)
i <- 12
#iseq <- c(seq(1,6),seq(8,11),seq(15,20))
iseq <- c(7,12,13,14)
output <- 
foreach(i = iseq) %dopar% {
  siteName <- as.character(siteData[i,1])
  print(siteName)
  TZ <- as.numeric(siteData[i,6])
  lat <- as.numeric(siteData[i,2])
  long <- as.numeric(siteData[i,3])
  PFT <- as.character(siteData[i,5])
  if(PFT=="DB"){
    startDay <- 182
    endDay <- 546
    startDate <- as.Date(startDay,origin="2016-12-31")
    endDate <- as.Date(endDay,origin="2016-12-31")
    xseq <- seq(startDay,endDay,1)
  }
  else if(PFT=="SH"){
    startDay <- 110
    endDay <- 455
    startDate <- as.Date(startDay,origin="2016-12-31")
    endDate <- as.Date(endDay,origin="2016-12-31")
    xseq <- seq(startDay,endDay,1)
  }
  outFileName <- paste(siteName,"_Midday2_varBurn.RData",sep="")
  print(outFileName)
  if(!file.exists(outFileName)){
    if(PFT=="DB"){
    j.model <- createBayesModel.DB_Avg(siteName=siteName,startDay = startDay,endDay=endDay,lat=lat,long=long,TZ=TZ)
    var.Burn <- runMCMC_Model(j.model = j.model, variableNames = c("TranS","bS","TranF","bF","d","c","k","prec"),baseNum = 20000,iterSize = 5000)
    }
    else if(PFT=="SH"){
      j.model <- createBayesModel.SH_Avg(siteName=siteName,startDay = startDay,endDay=endDay,lat=lat,long=long,TZ=TZ,dataSource = "GOES.NDVI")
      var.Burn <- runMCMC_Model(j.model = j.model, variableNames = c("Tran","b","c","d","k","r","prec"),baseNum = 80000,iterSize = 20000,maxGBR = 100)
    }
    if(typeof(var.Burn)!=typeof(FALSE)){
      save(var.Burn,file=outFileName)
    }
  }
}
