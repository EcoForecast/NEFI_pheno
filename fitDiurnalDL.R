#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
#install.packages("MODISTools",repo="https://cloud.r-project.org/")
#install.packages("curl",repo="https://cloud.r-project.org/")
library("PhenologyBayesModeling")
library("rjags")
library("runjags")
library("MODISTools")



#dat <- read.csv("GOES_NDVI_missouriozarks_20170606.csv",header=FALSE)
dat <- read.csv("GOES_Diurnal_HarvardForest_2017July2.csv",header=FALSE)
data <- list()
data$x <- as.numeric(substr(dat[1,],8,9))+as.numeric(substr(dat[1,],10,11))/60
for(i in 1:length(data$x)){
  if(data$x[i]<5){
    data$x[i] <- data$x[i]+24
  }
}
data$y <- as.numeric(dat[2,])
plot(data$x,data$y)
#xseq <- seq(10,17.5)
#lines(x <- xseq,y <- pheno.logistic(Tran=11.95,b=-1.5,d=0.3,c=0.48,xseq=xseq),col="red")
#xseq <- xseq <- seq(17.5,25.2,0.1)
#lines(x <- xseq,y <- pheno.logistic(Tran=24,b=1.8,d=0.3,c=0.48,xseq=xseq),col="red")
nchain = 5
inits <- list()
#,d=rnorm(1,0.3,0.1)
for(i in 1:nchain){
  inits[[i]] <- list(TranL=rnorm(1,11.95,0.1),bL=rnorm(1,-1.5,0.3),TranR=rnorm(1,24,0.1),bR=rnorm(1,1.8,0.2),c=rnorm(1,0.8,0.05),k=rnorm(1,17.5,1))
}
#data$mean.c <- 0.48
#data$mean.d <- 0.3
#data$p.c <- 1/(0.5**2)
#data$p.d <- 1/(0.5**2)
data$alpha.c <- 37#11
data$beta.c <- 10#3

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

DB_model_DL <- "
model{
##priors
TranL ~ dnorm(mean.TranL,p.Tran) ##S for spring
bL ~ dnorm(mean.bL,p.b)
TranR ~ dnorm(mean.TranR,p.Tran)  ##F for fall/autumn
bR ~ dnorm(mean.bR,p.b)
#d ~ dnorm(mean.d,p.d)
#c ~ dnorm(mean.c,p.c)
c ~ dbeta(alpha.c,beta.c)
k ~ dnorm(mean.k,p.k)
prec ~ dgamma(s1,s2)

for(i in 1:n){
muL[i] <- c/(1+exp(bL*(x[i]-TranL))) ##process model for left
muR[i] <- c/(1+exp(bR*(x[i]-TranR))) ##process model for right
mu[i] <- ifelse(x[i]>k,muR[i],muL[i])   #change point process model

y[i]  ~ dnorm(mu[i],prec)		## data model
}
}
"
j.model   <- jags.model(file = textConnection(DB_model_DL),
                        data = data,
                        inits=inits,
                        n.chains = nchain)
md.out <- runMCMC_Model(j.model,variableNames=c("TranL","bL","TranR","bR","c","k"))
print(summary(md.out))
save(md.out,file="HarvardForest_2017July2_DL_varBurn.RData")
out.mat <- as.matrix(md.out)
save(out.mat,file="HarvardForest_2017July2_DL_outMat.RData")
