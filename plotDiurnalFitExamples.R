#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
#install.packages("MODISTools",repo="https://cloud.r-project.org/")
#install.packages("doParallel",repo="https://cloud.r-project.org/")
library("PhenologyBayesModeling")
library("rjags")
library("runjags")

siteName <- "russellSage"
xseq <- seq(0,25,0.1)

outputFileName <- paste(siteName,"_diurnalExamples2.pdf",sep="")
pdf(file=outputFileName,width=10,height=5)
par(mfrow=c(3,3))
day.seq <- c("186","201","245","251","278","030","012","013","290")
#dy="030"
for(dy in day.seq){
  #fileName <- dir(path="dailyNDVI_GOES",pattern=paste(dy,"_varBurn.RData",sep=""))
  fileName <- intersect(dir(pattern=paste(dy,"_varBurn2.RData",sep="")),dir(pattern=siteName))
  print(fileName)
  load(fileName)
  if(as.numeric(dy)<182){
    yr <- "2018"
  }
  else{
    yr <- "2017"
  }
  dat <- read.csv(paste("dailyNDVI_GOES/GOES_Diurnal_",siteName,"_",yr,dy,".csv",sep=""),header=FALSE)
  out.mat <- as.matrix(var.burn)
  a <- out.mat[,1]
  c <- out.mat[,2]
  k <- out.mat[,3]
  ycred <- matrix(0,nrow=10000,ncol=length(xseq))
  for(g in 1:10000){
    Ey <- diurnalExp(a=a[g],c=c[g],k=k[g],xseq=xseq)
    ycred[g,] <- Ey
  }
  ci <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  date <- as.Date(as.numeric(dy),origin=as.Date(paste(as.character(as.numeric(yr)-1),"-12-31",sep="")))
  plot(x=list(),y=list(),main=date,ylim=c(0,1),xlim=c(0,25),ylab="NDVI",xlab="Hour",cex=2.5)
  ciEnvelope(xseq,ci[1,],ci[3,],col="lightBlue")
  lines(xseq,ci[2,],col="black")
  points(as.numeric(dat[3,]),as.numeric(dat[2,]),pch=".")
  abline(v=12,col="red")
}
dev.off()
