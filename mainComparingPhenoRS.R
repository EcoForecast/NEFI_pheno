#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
library("PhenologyBayesModeling")
library("rjags")
library("runjags")

siteData <- read.csv("GOES_Paper_Sites.csv",header=FALSE)
startDay <- 110
endDay <- 424
xseq <- seq(startDay,endDay,1)

iseq <- seq(1,19)

for(i in iseq){
  siteName <- as.character(siteData[i,1])
  print(siteName)
  URL <- as.character(siteData[i,4])
  PFT <- as.character(siteData[i,5])
  if(PFT=="DB"){
    DB.vars <- c("a","b","c","d","prec")
    fileName <- paste(siteName,"_PC_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.PC <- createBayesModel.DB(dataSource="PC.GCC",siteName=siteName,URL=URL)
      PC.md.out <- runMCMC_Model(j.model=j.model.PC,variableNames = DB.vars)
      save(var.burn,file=fileName)
    }
    fileName <- paste(siteName,"_MODIS_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.MODIS <- createBayesModel.DB(dataSource="MODIS.NDVI",siteName=siteName)
      MODIS.md.out <- runMCMC_Model(j.model=j.model.MODIS,variableNames = DB.vars)
      save(var.burn,file=fileName)
    }
    
    fileName <- paste(siteName,"_GOES_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.GOES <- createBayesModel.DB(dataSource="GOES.NDVI",siteName=siteName)
      GOES.md.out <- runMCMC_Model(j.model=j.model.GOES,variableNames = DB.vars)
      save(var.burn,file=fileName)
    }
  
  }
  else if(PFT=="SH"){
    SH.vars <- c("a","b","c","d","k","r","prec")
    fileName <- paste(siteName,"_PC_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.PC <- createBayesModel.SH(dataSource="PC.GCC",siteName=siteName,URL=URL)
      PC.md.out <- runMCMC_Model(j.model=j.model.PC,variableNames = SH.vars)
      save(var.burn,file=fileName)
    }
    fileName <- paste(siteName,"_MODIS_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.MODIS <- createBayesModel.SH(dataSource="MODIS.NDVI",siteName=siteName)
      MODIS.md.out <- runMCMC_Model(j.model=j.model.MODIS,variableNames = SH.vars)
      save(var.burn,file=fileName)
    }
    
    fileName <- paste(siteName,"_GOES_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.GOES <- createBayesModel.SH(dataSource="GOES.NDVI",siteName=siteName)
      GOES.md.out <- runMCMC_Model(j.model=j.model.GOES,variableNames = SH.vars)
      save(var.burn,file=fileName)
    }
  }
  #graphMCMC_Outputs(outputFileName = "SH_PhenoFit.pdf",siteFileName = "GOES_Paper_Sites.csv",iseq=7,startDay = startDay,endDay = endDay)
    

}
  
#graphMCMC_Outputs(outputFileName = "BayesFits_SH3.pdf",siteFileName = "GOES_Paper_Sites.csv",iseq=c(7,18,19),startDay=110,endDay=424)
graphMCMC_Outputs_withData(outputFileName = "BayesFits_SH2.pdf",siteFileName = "GOES_Paper_Sites.csv",iseq=c(7,18,19),startDay=110,endDay=424)



