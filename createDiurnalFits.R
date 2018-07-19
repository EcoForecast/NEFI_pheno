#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
install.packages("MODISTools",repo="https://cloud.r-project.org/")
#install.packages("curl",repo="https://cloud.r-project.org/")
library("PhenologyBayesModeling")
library("rjags")
library("runjags")
library("MODISTools")

diurnalLogistic <- function(TranL,bL,TranR,bR,c,d,k,xseq){
  bk <- which(xseq==round(k,digits=0))
  left <- pheno.logistic(Tran=TranL,b=bL,c=c,d=d,xseq[1:bk])
  right.xseq <- xseq[(bk+1):length(xseq)]
  right <- pheno.logistic(Tran=TranR,b=bR,c=c,d=d,right.xseq)
  return(c(left,right))
}
xseq <- seq(10,25.2,0.2)

DB_model_DL <- "
model{
##priors
TranL ~ dnorm(mean.TranL,p.Tran) ##S for spring
bL ~ dnorm(mean.bL,p.b)
TranR ~ dnorm(mean.TranR,p.Tran)  ##F for fall/autumn
bR ~ dnorm(mean.bR,p.b)
d ~ dnorm(mean.d,p.d)
c ~ dnorm(mean.c,p.c)
k ~ dnorm(mean.k,p.k)
prec ~ dgamma(s1,s2)

for(i in 1:n){
muL[i] <- c/(1+exp(bL*(x[i]-TranL)))+d ##process model for fall
muR[i] <- c/(1+exp(bR*(x[i]-TranR)))+d ##process model for Spring
mu[i] <- ifelse(x[i]>k,muR[i],muL[i])   #change point process model

y[i]  ~ dnorm(mu[i],prec)		## data model
}
}
"
nchain = 5
inits <- list()

for(i in 1:nchain){
  inits[[i]] <- list(TranL=rnorm(1,11.95,0.1),bL=rnorm(1,-1.5,0.3),TranR=rnorm(1,24,0.1),bR=rnorm(1,1.8,0.2),c=rnorm(1,0.48,0.5),d=rnorm(1,0.3,0.1),k=rnorm(1,17.5,1))
}

dayData <- read.csv("sampleDiurnalDays.csv",header=TRUE)
pdf(file="DiurnalBayesFits.pdf",width=20,height=30)

i <- 2
iseq <- c(2,3)
for(i in iseq){
  siteName <- as.character(dayData[i,]$Site)
  print(siteName)
  yr <- as.character(dayData[i,]$Year)
  mth <- as.character(dayData[i,]$Month)
  dy <- as.character(dayData[i,]$Day)
  #TZ <- as.numeric(dayData[i,]$TZ)
  #DOY <- as.numeric(dayData[i,]$DOY)
  #quality <- as.character(dayData[i,]$Quality)
  InFileName <- paste("GOES_Diurnal_",siteName,"_",yr,mth,dy,".csv",sep="")
  print(InFileName)
  fileDat <- read.csv(InFileName,header=FALSE)
  print(dim(fileDat))
  print(fileDat[3,])
  data$x <- as.numeric(fileDat[3,])
  data$y <- as.numeric(fileDat[2,])
  outFileName <- paste("DiurnalFit_",siteName,"_",yr,mth,dy,"varBurn.RData",sep="")
  if(!file.exists(outFileName)){
  data$mean.c <- 0.48
  data$mean.d <- 0.3
  data$p.c <- 1/(0.5**2)
  data$p.d <- 1/(0.5**2)
  
  data$s1 <- 0.001
  data$s2 <- 0.00001
  data$p.Tran <- 1/(1**2)
  data$p.b <- 1/(1**2)
  data$mean.TranL <- 11.95
  data$mean.bL <- -1.5
  data$mean.TranR <- 24
  data$mean.bR <- 1.8
  data$mean.k <- 17.5
  data$p.k <- 1/(1**2)
  data$n <- length(data$x)
  j.model   <- jags.model(file = textConnection(DB_model_DL),
                          data = data,
                          inits=inits,
                          n.chains = nchain)
  md.out <- runMCMC_Model(j.model,variableNames=c("TranL","bL","TranR","bR","c","d","k"))
  }
  # load(outFileName)
  # out.mat <- as.matrix(md.out)
  # TranL <- out.mat[,1]
  # TranR <- out.mat[,2]
  # bL <- out.mat[,3]
  # bR <- out.mat[,4]
  # c <- out.mat[,5]
  # d <- out.mat[,6]
  # k <- out.mat[,7]
  # 
  # ycred <- matrix(0,nrow=10000,ncol=length(xseq))
  # for(g in 1:10000){
  #   Ey <- diurnalLogistic(TranL=TranL[g],bL=bL[g],TranR=TranR[g],bR=bR[g],c=c[g],d=d[g],k=k[g],xseq=xseq)
  #   ycred[g,] <- Ey
  # }
  # CI <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  # plot(data$x,data$y,ylim=c(0,1))
  # lines(xseq,CI[1,],col="red",lty=2)
  # lines(xseq,CI[3,],col="red",lty=2)
  # lines(xseq,CI[2,],col="red")
  
}
dev.off()
