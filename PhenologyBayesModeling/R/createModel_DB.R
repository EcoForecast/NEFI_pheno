#!/usr/bin/env Rscript

library("rjags")
library("runjags")

##' Create a Bayes Model for a deciduous broadleaf site
##'
##' @param dataSource data source (GOES.NDVI, MODIS.NDVI, PC.GCC)
##' @param siteName Site Name
##' @param URL PhenoCam network URL
createBayesModel.DB <- function(dataSource,siteName="",URL="",niter=100000,startDay=164,endDay=425) {
  nchain = 5
  inits <- list()
  if(dataSource=="PC.GCC"){
    fileName <- paste(siteName,"_PC_Data.RData",sep="")
    load(fileName)
    data <- PC.data
    for(i in 1:nchain){
      inits[[i]] <- list(a=-30,b=rnorm(1,0.10,0.015),c=rnorm(1,0.05,0.01),d=rnorm(1,0.33,0.03))
    }
    data$beta.c <- 5
    data$alpha.c <- 3
    data$alpha.d <- 4
    data$beta.d <- 5
  }
  else if(dataSource == "MODIS.NDVI"){
    data = MODIS_data(siteName=siteName)
    for(i in 1:(nchain)){
      inits[[i]] <- list(a=rnorm(1,-30,3),b=rnorm(1,0.11,0.05),c=0.2,d=0.7)
    }
    data$beta.c <- 5
    data$alpha.c <- 3
    data$alpha.d <- 6
    data$beta.d <- 4
  }
  else if(dataSource=="GOES.NDVI"){
    data = GOES_data(siteName,startDay = 110,endDay = 424)
    for(i in 1:(nchain)){
      inits[[i]] <- list(a=rnorm(1,-29.9,0.3),b=rnorm(1,0.25,0.1),c=rnorm(1,0.4,0.1),d=rnorm(1,0.4,0.1))
    }
    data$beta.c <- 5
    data$alpha.c <- 3
    data$alpha.d <- 4
    data$beta.d <- 5
  }
  data$s1 <- 0.001
  data$s2 <- 0.00001
  data$p.a <- 1
  data$p.b <- .001
  data$mean.a <- -30
  data$mean.b <- 0.11

  DB_model <- "
  model{
  ##priors
  a ~ dnorm(mean.a,p.a)
  b ~ dnorm(mean.b,p.b)
  d ~ dbeta(alpha.d,beta.d)
  c ~ dbeta(alpha.c,beta.c)
  prec ~ dgamma(s1,s2)

  for(i in 1:n){
  mu[i] <- c/(1 + exp(a+b*x[i]))+d   	## process model
  y[i]  ~ dnorm(mu[i],prec)		## data model (will need to change to beta eventually)
  }
  }
  "
  j.model   <- jags.model(file = textConnection(DB_model),
                          data = data,
                          inits=inits,
                          n.chains = nchain)

  return(j.model)
}
