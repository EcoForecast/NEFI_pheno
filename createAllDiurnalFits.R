#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
install.packages("MODISTools",repo="https://cloud.r-project.org/")
install.packages("doParallel",repo="https://cloud.r-project.org/")
library("PhenologyBayesModeling")
library("rjags")
library("runjags")
library("MODISTools")
library("doParallel")

#detect cores.
n.cores <- detectCores()
n.cores <- 2

#register the cores.
registerDoParallel(cores=n.cores)

siteName <- "russellSage"
#diurnal.files <- dir(path="dailyNDVI_GOES",pattern=paste("GOES_Diurnal_",siteName,sep=""))
iseq <- c(186,191,198,230,248,250,252,285)
#iseq <- c(186,191,198,230)
#iseq <-c(186)
output <- foreach(i = iseq) %dopar% {
#for(i in iseq){
#i <- iseq[4]
  fileName <- paste("dailyNDVI_GOES/","GOES_Diurnal_",siteName,"_2017",i,".csv",sep="")
  print(fileName)
  dat <- read.csv(fileName,header=FALSE)
  data.v <- list()
  print(dim(dat))
  data.v$x <- as.numeric(dat[3,])
  data.v$y <- as.numeric(dat[2,])
  print("j.model")
  j.model <- createBayesModel.Diurnal(siteName=siteName,data.v)
  
  var.burn <- runMCMC_Model(j.model = j.model,variableNames=c("TranL","bL","TranR","bR","c","k","prec","p.cloud"))
  counter <- 1
  while(counter < 5){
    if(typeof(var.burn)==typeof(FALSE)){
      j.model <- createBayesModel.Diurnal(siteName=siteName,data)
      var.burn <- runMCMC_Model(j.model = j.model,variableNames=c("c","prec","p.cloud"))
    }
    counter <- counter + 1
  }
  if(typeof(var.burn)!=typeof(FALSE)){
    outFileName <- paste(siteName,"_",as.character(i),"_varBurn.RData",sep="")
    save(var.burn,file=outFileName)
  }
}
