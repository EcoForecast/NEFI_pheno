#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
#install.packages("MODISTools",repo="https://cloud.r-project.org/")
#install.packages("doParallel",repo="https://cloud.r-project.org/")
library("PhenologyBayesModeling")
library("rjags")
library("runjags")

library("ecoforecastR")


diurnalExp <- function(a,c,k,xseq){
  k <- round(k,digits=1)
  #print(k)
  bk <- which(round(xseq,digits=1)==k)
  #print(bk)
  left <- -a*exp(-1*(xseq[1:bk]-k))+c
  right.xseq <- xseq[(bk+1):length(xseq)]
  right <- -a*exp((right.xseq-k))+c
  #print(length(c(left,right)))
  return(c(left,right))
}

siteName <- "russellSage"
xseq <- seq(0,25,0.1)

outputFileName <- paste(siteName,"_diurnalExamples2.pdf",sep="")
pdf(file=outputFileName,width=8,height=10)
par(mfrow=c(4,4),mai=c(0.5,0.5,0.5,0.5))
day.seq <- c("186","189","201","212","231","245","251","278","290","012","013","030","074","091","167","168")
#day.seq <- c("186","201","245","251","278","030","012","013","290")
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
  polygon(x=c(10,14,14,10),y=c(-1,-1,1.2,1.2),col="lightgray",border=NA)
  ciEnvelope(xseq,ci[1,],ci[3,],col="lightBlue")
  lines(xseq,ci[2,],col="black")
  points(as.numeric(dat[3,]),as.numeric(dat[2,]),pch=".")
  abline(v=12,col="red")
 
}
dev.off()
