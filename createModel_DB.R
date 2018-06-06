library(rjags)
library(coda)
library("MODISTools")
library("numDeriv")

##' Download Phenocam data
##' 
##' @param URL  web address where data is located
download.phenocam <- function(URL) {
  ## check that we've been passed a URL
  if (length(URL) == 1 & is.character(URL) & substr(URL,1,4)=="http") {
    
    ## read data
    dat <- read.csv(URL,skip = 22)
    
    ## convert date
    dat$date <- as.Date(as.character(dat$date))
    
    return(dat)
  } else {
    print(paste("download.phenocam: Input URL not provided correctly",URL))
  }
}



##' Create a Bayes Model for a deciduous broadleaf site
##' 
##' @param Lat  latitude of desired site in decimals
##' @param Long longitude of desired site in decimals
##' @param data.source data source (GOES.NDVI, MODIS.NDVI, PC.GCC)
##' @param site.name Site Name
##' @param URL PhenoCam network URL
createBayesModel.DB <- function(Lat=0, Long=0, data.source,site.name="",URL="",niter=100000) {
  nchain = 5
  inits <- list()
  if(data.source=="PC.GCC"){
    data = PC_data(URL)
    print(data$x)
    for(i in 1:nchain){
      inits[[i]] <- list(a=-30,b=rnorm(1,0.10,0.015),c=rnorm(1,0.05,0.01),d=rnorm(1,0.33,0.03))
    }
  }
  else if(data.source == "MODIS.NDVI"){
    data = MODIS_data(Lat,Long,data.source,site.name=site.name)
    for(i in 1:(nchain)){
      inits[[i]] <- list(a=rnorm(1,-30,3),b=rnorm(1,0.11,0.05),c=0.2,d=0.7)
    }
  }
  else if(data.source=="GOES.NDVI"){
    data = GOES_data(site.name)
    for(i in 1:(nchain)){
      inits[[i]] <- list(a=rnorm(1,-29.9,0.3),b=rnorm(1,0.25,0.1),c=rnorm(1,0.4,0.1),d=rnorm(1,0.4,0.1))
    }
  }
  data$s1 <- 0.001
  data$s2 <- 0.00001
  data$v.a <- 1
  data$v.b <- .001
  data$mean.a <- -30
  data$mean.b <- 0.11
  
  DB_model <- "
  model{
  ##priors
  a ~ dnorm(mean.a,v.a)
  b ~ dnorm(mean.b,v.b)
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
  var.out   <- coda.samples (model = j.model,
                             variable.names = c("a","b","c","d","prec"),
                             n.iter = niter)
  output <- list()
  output$var.out <- var.out
  output$x <- data$x
  output$y <- data$y
  return(output)
}

##' For GOES NDVI data, construct the data object for input into MCMC
##' 
##' @param site.name Site Name
GOES_data <- function(site.name) {
  ##Data
  NDVI.fileName1 <- paste("GOES_NDVI_",site.name,"2017_kappaDQF2.csv",sep="")
  GOES1 <- read.csv(NDVI.fileName1,header=FALSE) 
  NDVI.fileName2 <- paste("GOES_NDVI_",site.name,"2018_kappaDQF.csv",sep="")
  GOES2 <- read.csv(NDVI.fileName2,header=FALSE)
  GOES2[1,] <- GOES2[1,]+365
  GOES <- cbind(GOES1,GOES2)
  
  GOES_Days <- as.numeric(GOES[1,])
  GOES_NDVI <- as.numeric(GOES[2,])
  
  sep.val <- min(which(GOES_Days>164)) 
  y <- GOES_NDVI[sep.val:length(GOES_Days)]
  x <- GOES_Days[sep.val:length(GOES_Days)]
  
  data <- list(x=x,y=y,n=length(y))
  ##Specify Priors
  data$alpha.c <- 4
  data$beta.c <- 3
  data$alpha.d <- 2
  data$beta.d <- 3
  
  return(data)
}

##' For PhenoCam data, construct the data object for input into MCMC
##' 
##' @param URL PhenoCam network URL
PC_data <- function(URL) {
  ##Data
  PC.data <- subset(download.phenocam(URL),year%in%c(2017,2018))
  PC.data <- PC.data[1:425,]
  PC.time = as.Date(PC.data$date)
  y <- PC.data$gcc_mean[164:425]
  x <- lubridate::yday(PC.time[164:425])
  for(i in 1:length(x)){
    if(x[i]<100){
      x[i] <- x[i]+365
    }
  }
  data <- list(x=x,y=y,n=length(y))
  ##Specify Priors
  data$beta.c <- 5
  data$alpha.c <- 1
  data$alpha.d <- 3
  data$beta.d <- 7

  return(data)
}

##' For MODIS EVI data, construct the data object for input into MCMC
##' 
##' @param Lat  latitude of desired site in decimals
##' @param Long longitude of desired site in decimals
##' @param data.source data source (GOES.NDVI, MODIS.NDVI, MODIS.EVI, PC.GCC)
MODIS_data <- function(Lat,Long,data.source,site.name) {
  ##Data
  #site.name <- "HarvardForest"
  fileName <- paste(site.name,"_MODIS_NDVI2.csv",sep="")

  MODIS = read.csv(fileName,header=FALSE)
  y <- MODIS[,7]
  x <- as.integer(MODIS[,5])
  for(i in 1:length(x)){
    if(x[i]<100){
      x[i] <- as.numeric(x[i]) + 365
    }
  }
  
  data <- list(x=x,y=y,n=length(y))
  ##Specify Priors
  data$alpha.c <- 1
  data$beta.c <- 5
  data$alpha.d <- 3.5
  data$beta.d <- 5
  
  return(data)
}

siteData <- read.csv("GE509_Project_Sites.csv",header=FALSE)

iseq <- c(1,3,4,7)
for (i in iseq){
  i <- 4
  siteName <- as.character(siteData[i,1])
  Lat <- as.character(siteData[i,2])
  Long <-as.character(siteData[i,3])
  URL <- as.character(siteData[i,4])
  
  #GOES
  out.GOES <- createBayesModel.DB(data.source="GOES.NDVI",site.name =  siteName,niter = 200000)


  #plot(out.GOES$var.out)
  gelman.diag(out.GOES$var.out)
  GBR <- gelman.plot(out.GOES$var.out)
  burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]
  burnin
  if(length(burnin) == 0) burnin = 1
  var.burn <- window(((out.GOES$var.out)),start=burnin)
  effectiveSize(var.burn)
  fileName <- paste(siteName,"_GOES_varBurn.RData",sep="")
  save(var.burn,file=fileName)

  
  #MODIS:
    out.MODIS <- createBayesModel.DB(Lat = Lat, Long=Long,data.source = "MODIS.NDVI",site.name=siteName,niter = 10000000)
  #plot(out.MODIS$var.out)
  
  gelman.diag(out.MODIS$var.out)
  GBR <- gelman.plot(out.MODIS$var.out)
  burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]
  burnin
  if(length(burnin) == 0) burnin = 1
  var.burn <- window(((out.MODIS$var.out)),start=burnin)
  effectiveSize(var.burn)
  fileName <- paste(siteName,"_MODIS_varBurn.RData",sep="")
  save(var.burn,file=fileName)
  
  #PC:
  out.PC <- createBayesModel.DB(data.source="PC.GCC",URL=URL,niter=300000)
  #plot(out.PC$var.out)
  
  gelman.diag(out.PC$var.out)
  GBR <- gelman.plot(out.PC$var.out)
  burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]
  burnin
  
  if(length(burnin) == 0) burnin = 1
  var.burn <- window(((out.PC$var.out)),start=burnin)
  effectiveSize(var.burn)
  fileName <- paste(siteName,"_PC_varBurn.RData",sep="")
  save(var.burn,file=fileName)
  
}
