#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
#install.packages("MODISTools",repo="https://cloud.r-project.org/")
#install.packages("doParallel",repo="https://cloud.r-project.org/")
library("PhenologyBayesModeling")
library("rjags")
library("runjags")
siteName <- "russellSage"
fileName <- "GOES_Diurnal_russellSage_2017186.csv"
dat <- read.csv(fileName,header=FALSE)
data <- list()
print(dim(dat))
data$x <- as.numeric(dat[3,])
data$y <- as.numeric(dat[2,])
#plot(data$x,data$y)
outFileName <- paste(siteName,"_",as.character(i),"_varBurn_ALLParamaters.RData",sep="")
j.model <- createBayesModel.Diurnal(siteName=siteName,data)
var.burn <- runMCMC_Model(j.model = j.model,variableNames=c("a","c","k","prec","alp","bet","p.cloud"),iterSize = 50000)#,baseNum = 1000000,iterSize = 70000)
save(var.burn,file=outFileName)

