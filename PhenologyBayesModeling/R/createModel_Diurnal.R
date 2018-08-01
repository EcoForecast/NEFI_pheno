library("rjags")
library("runjags")
library("MODISTools")
#library("doParallel")

createBayesModel.Diurnal <- function(siteName,dat){
  print("entered model")
  nchain <-  5
  init.vls <- list()
  print("initiated inits")

  for(i in 1:5){
    init.vls[[i]] <- list(TranL=rnorm(1,11.95,0.1),bL=rnorm(1,-1.5,0.3),TranR=rnorm(1,24,0.1),bR=rnorm(1,1.8,0.2),c=rnorm(1,0.8,0.05),k=rnorm(1,17.5,1))
    print(i)
  }
  "Finished inits"
  data$mean.c <- 0.48
  data$p.c <- 1/(0.5**2)

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
  print("finished defining data")
  DB_model_MM <- "
  model{
  ##priors
  TranL ~ dnorm(mean.TranL,p.Tran) ##S for spring
  bL ~ dnorm(mean.bL,p.b)
  TranR ~ dnorm(mean.TranR,p.Tran)  ##F for fall/autumn
  bR ~ dnorm(mean.bR,p.b)
  c ~ dnorm(mean.c,p.c)
  k ~ dnorm(mean.k,p.k)
  prec ~ dgamma(s1,s2)
  alp ~ dunif(1,100)
  bet ~ dunif(1,100)
  p.cloud ~ dunif(0,1)


  for(i in 1:n){
  muL[i] <- c/(1+exp(bL*(x[i]-TranL))) ##process model for left
  muR[i] <- c/(1+exp(bR*(x[i]-TranR))) ##process model for right
  f[i] <- ifelse(x[i]>k,muR[i],muL[i])   #change point process model

  y[i] ~ dnorm(mu[i],prec)   ##data model
  is.cloudy[i] ~ dbern(p.cloud)
  trans[i] ~ dbeta(alp,bet)
  mu[i] <- is.cloudy[i] * trans[i]*f[i] + (1-is.cloudy[i]) * f[i]

  }
  }
  "

  j.model   <- jags.model(file = textConnection(DB_model_MM),
                          data = data,
                          inits=init.vls,
                          n.chains=nchain)
  return(j.model)

}
