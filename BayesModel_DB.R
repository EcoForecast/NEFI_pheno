source("downloadPhenoCam.R")
source("PC_data.R")
source("MODIS_data.R")
source("GOES_data.R")
library(rjags)
library(coda)

##' Create a Bayes Model for a deciduous broadleaf site
##' 
##' @param Lat  latitude of desired site in decimals
##' @param Long longitude of desired site in decimals
##' @param data.source data source (GOES.NDVI, MODIS.NDVI, MODIS.EVI, PC.GCC)
##' @param site.name Site Name
##' @param URL PhenoCam network URL
##' @param season spring or autumn
##' @param download Boolean to indicate whether you need to download the MODIS data (true) or not (false)
createBayesModel.DB <- function(Lat, Long, data.source,site.name,URL,season,download ) {
  nchain = 3
  c(Lat,Long,site.name,URL,download) #to get rid of unused arguments error
  if(data.source=="PC.GCC"){
    data = PC_data(URL,season)
  }
  else if(data.source=="MODIS.EVI" || data.source == "MODIS.NDVI"){
    data = MODIS_data(Lat,Long,data.source,season,download)
  }
  else if(data.source=="GOES_NDVI"){
    data = GOES_data(site.name,season)
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

   # j.model   <- jags.model(file = textConnection(DB_model),
   #                         data = data,
   #                         n.chains = nchain) #cannot figure out how to include the inits
   # var.out   <- coda.samples (model = j.model,
   #                             variable.names = c("a","b","c","d","prec"),
   #                             n.iter = 1000)
  return(data)

}

harvLat <- 42.5378
harvLong <- -72.1715
harvURL <- "http://phenocam.sr.unh.edu/data/archive/harvard/ROI/harvard_DB_0001_1day.csv"
var.out <- createBayesModel.DB(harvLat,harvLong,"PC.GCC","HarvardForest",harvURL,"spring",FALSE)
#var.out <- createBayesModel.DB("PC.GCC","spring",Lat=harvLat,long=harvLong,site.name="HarvardForest",URL=harvURL,download=FALSE)
var.out2 <- createBayesModel.DB(harvLat,harvLong,"PC.GCC",harvURL,"autumn",FALSE)
var.out3 <- createBayesModel.DB(harvLat,harvLong,"MODIS.EVI",harvURL,"spring",FALSE)
var.out4 <- createBayesModel.DB(harvLat,harvLong,"GOES_NDVI","HarvardForest", harvURL,"spring",FALSE)

var.mat <- as.matrix(var.out)
plot(var.out2)

gelman.diag(var.out)
GBR <- gelman.plot(var.out)

burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]

if(length(burnin) == 0) burnin = 1
var.burn <- window(var.out,start=burnin)
gelman.diag(var.burn)
plot(var.burn)
summary(var.burn)
