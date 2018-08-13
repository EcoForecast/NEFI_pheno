install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
library("ncdf4")
library(plyr)
library("PhenologyBayesModeling")
library(doParallel)
library("rjags")
library("runjags")


siteName <- "russellSage"
diurnalFits <- dir(path="diurnalFits",pattern=siteName)
c.vals <- numeric()
prec.vals <- numeric()
days <- numeric()
counts <- numeric()
outDataFile <- paste(siteName,"_diurnalFitData.RData",sep="")
if(!file.exists(outDataFile)){
  for(i in 1:length(diurnalFits)){
    print(diurnalFits[i])
    load(paste("diurnalFits/",diurnalFits[i],sep=""))
    out.mat <- as.matrix(var.burn)
    c <- mean(out.mat[,2])
    prec <- as.numeric(quantile(out.mat[,2],0.975))-as.numeric(quantile(out.mat[,2],0.025))
    c.vals <- c(c.vals,c)
    prec.vals <- c(prec.vals,prec)
    dy <- strsplit(diurnalFits[i],"_")[[1]][2]
    dayDataFile <- intersect(dir(path="dailyNDVI_GOES",pattern=paste(dy,".csv",sep="")),dir(path="dailyNDVI_GOES",pattern=siteName))
    dayData <- read.csv(paste("dailyNDVI_GOES/",dayDataFile,sep=""),header=FALSE)
    counts <- c(counts,length(dayData[2,][!is.na(dayData[2,])]))
    days <- c(days,dy)
  }
  data <- list()
  for(i in 1:length(days)){
    if(days[i]<182){
      days[i] <- as.numeric(days[i]) + 365
    }
  }
  data$x <- as.numeric(days)
  data$y <- as.numeric(c.vals)
  data$obs.prec <- as.numeric(prec.vals)
  data$n <- length(dy)
  data$size <- as.numeric(counts)
  print(dim(data$x))
  print(dim(data$y))
  print(data$x)
  save(data,file=outDataFile)
  print("Done with creating Data")
}
load(outDataFile)
j.model <- createBayesModel.DB_Overall(data=data)
var.burn <- runMCMC_Model(j.model=j.model,variableNames = c("TranS","bS","TranF","bF","d","c","k","prec"))
save(var.burn,file=paste(siteName,"_overall_varBurn.RData"),sep="")


