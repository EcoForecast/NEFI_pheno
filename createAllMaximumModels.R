#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
library("PhenologyBayesModeling")
library("rjags")
library("runjags")
library("ncdf4")
library(plyr)
library(doParallel)

siteData <- read.csv("GOES_Paper_Sites.csv",header=TRUE)

DB.vars <- c("TranF","bF","TranS","bS","c","d","prec","k")
SH.vars <- c("Tran","b","c","d","k","r","prec")

#detect cores.
#n.cores <- detectCores()
n.cores <- 6


#register the cores.
registerDoParallel(cores=n.cores)
i=2
output <- 
  foreach(i=1:nrow(siteData)) %dopar% {
    siteName <- as.character(siteData$siteName[i])
    print(siteName)
    URL <- as.character(siteData$URL[i])
    PFT <- as.character(siteData$PFT[i])
    print(PFT)
    lat <- as.character(siteData$Lat[i])
    long <- as.character(siteData$Long[i])
    TZ <- as.character(siteData$TZ[i])
    
    if(PFT=="DB"){
      startDay <- 182
      endDay <- 546
      xseq <- seq(startDay,endDay,1)
      fileName <- paste(siteName,"_",startDay,"_",endDay,"_GOES_max_varBurn.RData",sep="")
      if(!file.exists(fileName)){
        j.model.GOES <- createBayesModel.DB(dataSource="GOES.NDVI",siteName=siteName,startDay = startDay,endDay = endDay,maxValue=TRUE)
        GOES.md.out <- runMCMC_Model(j.model=j.model.GOES,variableNames = DB.vars,maxGBR = 30,iterSize = 50000,baseNum = 100000)
        save(GOES.md.out,file=fileName)
      }
    }
    else if(PFT=="SH"){
      startDay <- 110
      endDay <-90+365
      xseq <- seq(startDay,endDay,1)
      SH.vars <- c("Tran","b","c","d","k","r","prec")
      fileName <- paste(siteName,"_GOES_max_varBurn.RData",sep="")
      if(!file.exists(fileName)){
        j.model.GOES <- createBayesModel.SH(dataSource="GOES.NDVI",siteName=siteName,startDay = startDay,endDay = endDay,maxValue=TRUE)
        GOES.md.out <- runMCMC_Model(j.model=j.model.GOES,variableNames = SH.vars)
        save(GOES.md.out,file=fileName)
      }
    }
  }

