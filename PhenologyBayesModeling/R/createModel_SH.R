library("rjags")
library("runjags")

##' Create a Bayes Model for a shrubland site
##'
##' @param dataSource data source (GOES.NDVI, MODIS.NDVI, PC.GCC)
##' @param siteName Site Name
##' @param URL PhenoCam network URL
createBayesModel.SH <- function(dataSource,siteName="",URL="") {
  nchain = 10
  inits <- list()
  if(dataSource=="PC.GCC"){
    fileName <- paste(siteName,"_PC_Data.RData",sep="")
    load(fileName)
    data <- PC.data
    #data = PC_data(URL=URL,startDay = 110,endDay = 424)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,-30,3),b=rnorm(1,0.11,0.05),c=rnorm(1,0.25,0.05),d=rnorm(1,0.15,0.05))
    }
    data$mean.d <- 0.3
    data$mean.c <- 0.1
    #print(data$y)
  }
  else if(dataSource == "MODIS.NDVI"){
    data = MODIS_data(siteName=siteName)
    for(i in 1:(nchain)){
      inits[[i]] <- list(a=rnorm(1,-30,3),b=rnorm(1,0.11,0.05),c=rnorm(1,0.25,0.05),d=rnorm(1,0.15,0.05))
    }
    data$mean.c <- 0.25
    data$mean.d <- 0.15
  }
  else if(dataSource=="GOES.NDVI"){
    data = GOES_data(siteName,startDay = 110,endDay = 424)
    for(i in 1:(nchain)){
      inits[[i]] <- list(a=rnorm(1,30,1),b=rnorm(1,-0.14,0.01),c=rnorm(1,0.25,0.02),d=rnorm(1,0.15,0.001),r=rnorm(1,-0.02,0.002),k=rnorm(1,240,5))
    }
    data$mean.c <- 0.25
    data$mean.d <- 0.15
  }

  data$mean.a <- 30
  data$p.a <- 1/(3**2)#10
  data$mean.b <- -0.14
  data$p.b <- 1/(0.01**2)
  data$p.c <- 1/(0.05**2)#10000
  data$p.d <- 1/(0.05**2)#10000#1/(0.01**2)
  data$mean.r <- -0.02
  data$p.r <- 1/(0.005**2)
  data$s1 <- 0.001
  data$s2 <- 0.00001
  data$mean.k <- 240
  data$p.k <- 1/(50**2)
  print(data$mean.k)

  SH_model <- "
  model{
  ##priors
  a ~ dnorm(mean.a,p.a)
  b ~ dnorm(mean.b,p.b)
  c ~ dnorm(mean.c,p.c)
  d ~ dnorm(mean.d,p.d)
  #d2 ~ dnorm(mean.d,p.d)
  r ~ dnorm(mean.r,p.r)
  prec ~ dgamma(s1,s2)
  k ~ dnorm(mean.k,p.k)

  for(i in 1:n){
  mu1[i] <- c/(1 + exp(a+b*x[i]))+d   	## process model for green up (logistic) (prebreak)
  mu2[i] <- c*exp(r*(x[i]-k))+d  ##process model for green down (exponential) (postbreak)
  mu[i] <- ifelse(x[i]>k,mu2[i],mu1[i])   #process model
  y[i]  ~ dnorm(mu[i],prec)		## data model (will need to change to beta eventually)
  }
  }
  "

  j.model   <- jags.model(file = textConnection(SH_model),
                          data = data,
                          inits=inits,
                          n.chains = nchain)
  return(j.model)
}
