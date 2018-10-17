#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
library("ncdf4")
library(plyr)
library("PhenologyBayesModeling")
library("rjags")
library("runjags")

##' Create the credible interval envelope for plotting
##' 
##' @param x time range
##' @param ylo the bottom credible interval values
##' @param yhi the top credible interval values
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

startDay <- 182
endDay <- 181+365
siteData <- read.csv("GOES_Paper_Sites.csv",header=TRUE)
iseq <- c(4,5,10,16,18)
#i=8
for(i in iseq){
  siteName <- as.character(siteData[i,1])
  print(siteName)
  TZ <- as.numeric(siteData[i,6])
  lat <- as.numeric(siteData[i,2])
  long <- as.numeric(siteData[i,3])
  j.model <- createBayesModel.DB_Avg(siteName=siteName,startDay = startDay,endDay=endDay,lat=lat,long=long,TZ=TZ)
  var.Burn <- runMCMC_Model(j.model = j.model, variableNames = c("TranS","bS","TranF","bF","d","c","k","prec"))
  outFileName <- paste(siteName,"_Midday_varBurn.RData",sep="")
  save(var.Burn,file=outFileName)
}
