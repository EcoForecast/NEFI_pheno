source("downloadPhenoCam.R")
library(rjags)
library(coda)

##' Create a Bayes Model for a deciduous broadleaf site
##' 
##' @param Lat  latitude of desired site in decimals
##' @param Long longitude of desired site in decimals
##' @param data.source data source (GOES.NDVI, MODIS.NDVI, MODIS.EVI, PC.GCC)
##' @param URL PhenoCam network URL
##' @param season spring or autumn
createBayesModel.DB <- function(Lat,Long,data.source,URL,season) {
  nchain = 3
  inits <- list()
  if(data.source=="PC.GCC"){
    ##Data
    PC.data <- subset(download.phenocam(harvURL),year==2017)
    PC.time = as.Date(PC.data$date)
    if(season=="spring"){
      y <- PC.data$gcc_mean[1:182]
      x <- lubridate::yday(PC.time[1:182])
    }
    if(season=="autumn"){
      y <- PC.data$gcc_mean[182:365]
      x <- lubridate::yday(PC.time[182:365])
    }
    
    data <- list(x=x,y=y,n=length(y))
    ##Specify Priors
    data$beta.c <- 7.5
    data$alpha.c <- 2
    data$alpha.d <- 3
    data$beta.d <- 7
    data$s1 <- 0.5
    data$s2 <- 0.2
    data$v.a <- 3
    data$v.b <- 0.01
    if(season=="spring"){
      data$mean.a <- 30
      data$mean.b <- -0.11
      for(i in 1:nchain){
        inits[[i]] <- list(s1 = 0.6, s2 = 0.3, v.a = 3.2, v.b = 0.012,
                           mean.a = 32, mean.b = -0.13)
        # inits[[i]] <- list(alpha.c = rnorm(1,2.5,0.3),beta.c = rnorm(1,7.5,0.5),alpha.d = 
        #                      rnorm(1,3,0.2), beta.d = rnorm(1,7,0.5), s1 = rnorm(1,0.5,0.01),
        #                    s2 = rnorm(1,0.2,0.01), v.a = rnorm(1,3,0.3), v.b = rnorm(1,0.01,0.001),
        #                    mean.a = rnorm(1,30,5), mean.b = rnorm(1,-0.11,0.02))
      }
    }
    if(season=="autumn"){
      data$mean.a <- -30
      data$mean.b <- 0.11
    }
  }
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
                          n.chains = nchain)
  var.out   <- coda.samples (model = j.model,
                              variable.names = c("a","b","c","d","prec"),
                              n.iter = 1000)
  return(var.out)

}

harvLat <- 42.5378
harvLong <- -72.1715
harvURL <- "http://phenocam.sr.unh.edu/data/archive/harvard/ROI/harvard_DB_0001_1day.csv"
var.out <- createBayesModel.DB(harvLat,harvLong,"PC.GCC",harvURL,"spring")

var.mat <- as.matrix(var.out)
plot(var.out)

gelman.diag(var.out)
GBR <- gelman.plot(var.out)

burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]

if(length(burnin) == 0) burnin = 1
var.burn <- window(var.out,start=burnin)
gelman.diag(var.burn)
plot(var.burn)
summary(var.burn)
