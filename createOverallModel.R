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
outDataFile <- paste(siteName,"diurnalFitData.RData",sep="")
for(i in 1:length(diurnalFits)){
  print(diurnalFits[i])
  load(paste("diurnalFits/",diurnalFits[i],sep=""))
  out.mat <- as.matrix(var.burn)
  c <- out.mat[,2]
  prec <- out.mat[,4]
  c.vals <- c(c.vals,c)
  prec.vals <- c(prec.vals,prec)
  dy <- strsplit(diurnalFits[i],"_")[[1]][2]
  days <- c(days,dy)
}
data <- list()
data$x <- as.numeric(dy)
data$y <- as.numeric(c.vals)
data$obs.prec <- as.numeric(prec.vals)

j.model <- createBayesModel.DB_Overall(data=data)
save(data,file=outDataFile)
var.burn <- runMCMC_Model(j.model=j.model,variableNames = c("TranS","bS","TranF","bF","d","c","k","prec"))
save(var.burn,file=paste(siteName,"_overall_varBurn.RData"))


