#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
#module load R/3.4.2
#module load jags
#install.packages("coda",repo="http://cloud.r-project.org/",lib = '/projectnb/dietzelab/kiwheel/Rlibrary')#, configure.args = "--with-jags-lib=/share/pkg/jags/4.0.0/install/lib")
library("coda")
library("PhenologyBayesModeling")
#install.packages("rjags",repo="http://cloud.r-project.org/", configure.args = "--with-jags-include=/share/pkg/jags/4.0.0/install/lib",lib = '/projectnb/dietzelab/kiwheel/Rlibrary')
#install.packages("runjags",repo="http://cloud.r-project.org/", configure.args = "--with-jags-include=/share/pkg/jags/4.0.0/install/lib",lib = '/projectnb/dietzelab/kiwheel/Rlibrary')
#install.packages("rjags",repo="http://cloud.r-project.org/",lib = '/projectnb/dietzelab/kiwheel/Rlibrary')
library("rjags",lib = '/projectnb/dietzelab/kiwheel/Rlibrary')#,configure.args = "--with-jags-include-lib=/share/pkg/jags/4.0.0/install/lib")
library("runjags",lib = '/projectnb/dietzelab/kiwheel/Rlibrary')#,configure.args = "--with-jags-include-lib=/share/pkg/jags/4.0.0/install/lib")

siteData <- read.csv("GOES_Paper_Sites.csv",header=FALSE)
startDay <- 110
endDay <- 424
xseq <- seq(startDay,endDay,1)

iseq <- c(7,15,18,19)
#iseq <- c(19,18)
for(i in iseq){
  siteName <- as.character(siteData[i,1])
  print(siteName)
  URL <- as.character(siteData[i,4])
  PFT <- as.character(siteData[i,5])
  if(PFT=="SH"){
    SH.vars <- c("a","b","c","d","k","r","prec")
    fileName <- paste(siteName,"_PC_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.PC <- createBayesModel.SH(dataSource="PC.GCC",siteName=siteName,URL=URL)
      PC.md.out <- runMCMC_Model(j.model=j.model.PC,variableNames = SH.vars)
      save(PC.md.out,file=fileName)
    }
    print("PC Done")
    fileName <- paste(siteName,"_MODIS_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.MODIS <- createBayesModel.SH(dataSource="MODIS.NDVI",siteName=siteName)
      MODIS.md.out <- runMCMC_Model(j.model=j.model.MODIS,variableNames = SH.vars)
      save(MODIS.md.out,file=fileName)
    }
    print("MODIS Done")
    fileName <- paste(siteName,"_GOES_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      j.model.GOES <- createBayesModel.SH(dataSource="GOES.NDVI",siteName=siteName)
      GOES.md.out <- runMCMC_Model(j.model=j.model.GOES,variableNames = SH.vars)
      save(GOES.md.out,file=fileName)
    }
    print("GOES Done")
  }
}

