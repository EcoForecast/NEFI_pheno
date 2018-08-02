#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
#install.packages("MODISTools",repo="https://cloud.r-project.org/")
#install.packages("doParallel",repo="https://cloud.r-project.org/")
library("PhenologyBayesModeling")
library("rjags")
library("runjags")
#library("MODISTools")
library("doParallel")

#detect cores.
n.cores <- detectCores()
n.cores <- 2

#register the cores.
registerDoParallel(cores=n.cores)

siteName <- "russellSage"
#diurnal.files <- dir(path="dailyNDVI_GOES",pattern=paste("GOES_Diurnal_",siteName,sep=""))
#iseq <- c(186,191,198,230,248,250,252,285)
iseq <- c(186,191,198,230)
#i=191

output <- foreach(i = iseq) %dopar% {
#for(i in iseq){
#i <- iseq[4]
  fileName <- paste("dailyNDVI_GOES/","GOES_Diurnal_",siteName,"_2017",i,".csv",sep="")
  print(fileName)
  dat <- read.csv(fileName,header=FALSE)
  data <- list()
  print(dim(dat))
  data$x <- as.numeric(dat[3,])
  data$y <- as.numeric(dat[2,])
  #plot(data$x,data$y)
  j.model <- createBayesModel.Diurnal(siteName=siteName,data)
  
  var.burn <- runMCMC_Model(j.model = j.model,variableNames=c("a","c","k","prec"),iterSize = 50000)#,baseNum = 1000000,iterSize = 70000)
  counter <- 1
  while(counter < 5){
    if(typeof(var.burn)==typeof(FALSE)){
      j.model <- createBayesModel.Diurnal(siteName=siteName,data)
      var.burn <- runMCMC_Model(j.model = j.model,variableNames=c("c","prec"))
    }
    counter <- counter + 1
  }
  if(typeof(var.burn)!=typeof(FALSE)){
    outFileName <- paste(siteName,substr(diurnal.files[i],28,34),"_varBurn.RData",sep="")
    save(var.burn,file=outFileName)
  }
}
