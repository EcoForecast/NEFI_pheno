#!/usr/bin/env Rscript
#Sys.setenv(LIBCURL_BUILD="winssl")
#install.packages("https://github.com/jeroen/curl/archive/master.tar.gz", repos = NULL)

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
install.packages("MODISTools",repo="https://cloud.r-project.org/")
#install.packages("curl",repo="https://cloud.r-project.org/")
library("PhenologyBayesModeling")
library("rjags")
library("runjags")
library("MODISTools")
#library("curl")
library("ncdf4")
library(plyr)

siteData <- read.csv("GOES_Paper_Sites.csv",header=TRUE)
#startDay <- 152
#endDay <- 544
#startDay <- 110
#endDay <- 455
#xseq <- seq(startDay,endDay,1)

iseq <- seq(17,20)
DB.vars <- c("TranF","bF","TranS","bS","c","d","prec","k")
SH.vars <- c("Tran","b","c","d","k","r","prec")

for(i in iseq){
  siteName <- as.character(siteData$siteName[i])
  print(siteName)
  URL <- as.character(siteData$URL[i])
  PFT <- as.character(siteData$PFT[i])
  lat <- as.character(siteData$Lat[i])
  long <- as.character(siteData$Long[i])
                      
  if(PFT=="DB"){
    startDay <- 152
    endDay <- 546
    xseq <- seq(startDay,endDay,1)
    fileName <- paste(siteName,"_PC_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.PC <- createBayesModel.DB(dataSource="PC.GCC",siteName=siteName,URL=URL,startDay = startDay,endDay = endDay)
      PC.md.out <- runMCMC_Model(j.model=j.model.PC,variableNames = DB.vars)
      save(PC.md.out,file=fileName)
    }
    fileName <- paste(siteName,"_MODIS_NDVI_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.MODIS <- createBayesModel.DB(dataSource="MODIS.NDVI",siteName=siteName,lat=lat,long=long,startDay = startDay,endDay = endDay)
      MODIS.N.md.out <- runMCMC_Model(j.model=j.model.MODIS,variableNames = DB.vars)
      save(MODIS.N.md.out,file=fileName)
    }
    fileName <- paste(siteName,"_MODIS_EVI_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.MODIS <- createBayesModel.DB(dataSource="MODIS.EVI",siteName=siteName,lat=lat,long=long,startDay = startDay,endDay = endDay)
      MODIS.E.md.out <- runMCMC_Model(j.model=j.model.MODIS,variableNames = DB.vars)
      save(MODIS.E.md.out,file=fileName)
    }
    fileName <- paste(siteName,"_GOES_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.GOES <- createBayesModel.DB(dataSource="GOES.NDVI",siteName=siteName,startDay = startDay,endDay = endDay,lat=lat, long=long)
      GOES.md.out <- runMCMC_Model(j.model=j.model.GOES,variableNames = DB.vars)
      save(GOES.md.out,file=fileName)
    }
  }
  else if(PFT=="SH"){
    startDay <- 110
    endDay <- 455
    xseq <- seq(startDay,endDay,1)
    fileName <- paste(siteName,"_PC_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.PC <- createBayesModel.SH(dataSource="PC.GCC",siteName=siteName,URL=URL,startDay = startDay,endDay = endDay)
      PC.md.out <- runMCMC_Model(j.model=j.model.PC,variableNames = SH.vars)
      save(PC.md.out,file=fileName)
    }
    fileName <- paste(siteName,"_MODIS_NDVI_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.MODIS <- createBayesModel.SH(dataSource="MODIS.NDVI",siteName=siteName,lat=lat,long=long,startDay = startDay,endDay = endDay)
      MODIS.N.md.out <- runMCMC_Model(j.model=j.model.MODIS,variableNames = SH.vars)
      save(MODIS.N.md.out,file=fileName)
    }
    fileName <- paste(siteName,"_MODIS_EVI_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.MODIS <- createBayesModel.SH(dataSource="MODIS.EVI",siteName=siteName,lat=lat,long=long,startDay = startDay,endDay = endDay)
      MODIS.E.md.out <- runMCMC_Model(j.model=j.model.MODIS,variableNames = SH.vars)
      save(MODIS.E.md.out,file=fileName)
    }
    fileName <- paste(siteName,"_GOES_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.GOES <- createBayesModel.SH(dataSource="GOES.NDVI",siteName=siteName,startDay = startDay,endDay = endDay,lat=lat,long=long)
      GOES.md.out <- runMCMC_Model(j.model=j.model.GOES,variableNames = SH.vars)
      save(GOES.md.out,file=fileName)
    }
  }
}
  
#graphMCMC_Outputs(outputFileName = "BayesFits_SH3.pdf",siteFileName = "GOES_Paper_Sites.csv",iseq=c(7,18,19),startDay=110,endDay=424)
#graphMCMC_Outputs_withData(outputFileName = "BayesFits_luckyHills.pdf",siteFileName = "GOES_Paper_Sites.csv",iseq=c(7),startDay=110,endDay=474)
#createOutputSummary(siteFileName = "GOES_Paper_Sites.csv",iseq=c(7,15),startDay=110,endDay=474)

#createOutputSummary(siteFileName = "GOES_Paper_Sites.csv",iseq=c(9,14,17),startDay=110,endDay=474)

#graphMCMC_Outputs_withData(outputFileName = "BayesFits_springHarvard.pdf",siteFileName = "GOES_Paper_Sites.csv",iseq=c(1),startDay=startDay,endDay=endDay)
