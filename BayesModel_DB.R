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
createBayesModel.DB <- function(Lat=0, Long=0, data.source,site.name="",URL="",season,download=FALSE ) {
  nchain = 3
  #c(Lat,Long,site.name,URL,download) #to get rid of unused arguments error
  if(data.source=="PC.GCC"){
    data = PC_data(URL,season)
  }
  else if(data.source=="MODIS.EVI" || data.source == "MODIS.NDVI"){
    data = MODIS_data(Lat,Long,data.source,season,download)
  }
  else if(data.source=="GOES_NDVI"){
    data = GOES_data(site.name,season)
  }
  #print(data)
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
    #print(data$x)
   inits <- list()
   for(i in 1:nchain){
     inits[[i]] <- list(a=30,b=rnorm(1,-0.225,0.3),c=rnorm(1,0.4,0.2),d=rnorm(1,0.4,0.2))
   }
   j.model   <- jags.model(file = textConnection(DB_model),
                           data = data,
                           inits=inits,
                           n.chains = nchain) #cannot figure out how to include the inits
   var.out   <- coda.samples (model = j.model,
                               variable.names = c("a","b","c","d","prec"),
                               n.iter = 10000)
  output <- list()
  output$var.out <- var.out
  output$x <- data$x
  output$y <- data$y
  return(output)

}

pheno.logistic <- function(a,b,c,d,xseq){
  print(xseq)
  return(c/(1 + exp(a+b*xseq))+d)
}

harvLat <- 42.5378
harvLong <- -72.1715
harvURL <- "http://phenocam.sr.unh.edu/data/archive/harvard/ROI/harvard_DB_0001_1day.csv"
var.out <- createBayesModel.DB(harvLat,harvLong,"PC.GCC","HarvardForest",harvURL,"spring",FALSE)
#var.out <- createBayesModel.DB("PC.GCC","spring",Lat=harvLat,long=harvLong,site.name="HarvardForest",URL=harvURL,download=FALSE)
var.out.PC <- createBayesModel.DB(data.source =  "PC.GCC",URL = harvURL,season ="spring")
var.out.MODIS2 <- createBayesModel.DB(Lat = harvLat,Long = harvLong,data.source ="MODIS.NDVI",season = "spring",download = FALSE)
var.out.GOES2 <- createBayesModel.DB(data.source="GOES_NDVI",site.name =  "HarvardForest",season ="autumn")

var.PC.sum <- summary(var.out.PC$var.out)
var.PC.a <- var.PC.sum$statistics[1,1]
var.PC.b <- var.PC.sum$statistics[2,1]
var.PC.c <- var.PC.sum$statistics[3,1]
var.PC.d <- var.PC.sum$statistics[4,1]

var.MODIS.sum <- summary(var.out.MODIS2$var.out)
plot((var.out.MODIS2$var.out))
gelman.diag(var.out.MODIS2$var.out)
GBR <- gelman.plot(var.out.MODIS2$var.out)
burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]
burnin
if(length(burnin) == 0) burnin = 1
var.burn <- window(((var.out.MODIS2$var.out)),start=burnin)
plot(var.burn)
summary(var.burn)

var.MODIS.sum <- summary(var.burn)
var.MODIS.sum
var.MODIS.a <- var.MODIS.sum$statistics[1,1]
var.MODIS.b <- var.MODIS.sum$statistics[2,1]
var.MODIS.c <- var.MODIS.sum$statistics[3,1]
var.MODIS.d <- var.MODIS.sum$statistics[4,1]


x.PC <- seq(1,185,1)
var.PC.pred <- pheno.logistic(var.PC.a,var.PC.b,var.PC.c,var.PC.d,x.PC)
plot(x.PC,var.PC.pred)

x.MODIS <- var.out.MODIS2$x
x.MODIS <- seq(1,5,0.005)
var.MODIS.pred <- pheno.logistic(var.MODIS.a,var.MODIS.b,var.MODIS.c,var.MODIS.d,x.MODIS)
var.MODIS.pred <- pheno.logistic(var.MODIS.a,-0.2,var.MODIS.c,var.MODIS.d,x.MODIS)
plot(x.MODIS,var.MODIS.pred)
lines(x.MODIS,var.MODIS.pred)

plot(x=list(),y=list(),xlim=c(0,200),ylim=c(0.5,1))
lines(x.MODIS,var.MODIS.pred)
points(var.out.MODIS2$x,var.out.MODIS2$y)


lines(x.MODIS,var.MODIS.pred)

var.GOES.sum <- summary(var.out.GOES2$var.out)
var.GOES.sum
var.GOES.a <- var.GOES.sum$statistics[1,1]
var.GOES.b <- var.GOES.sum$statistics[2,1]
var.GOES.c <- var.GOES.sum$statistics[3,1]
var.GOES.d <- var.GOES.sum$statistics[4,1]
x.GOES <- var.out.GOES2$x

var.GOES.pred <- pheno.logistic(var.GOES.a,var.GOES.b,var.GOES.c,var.GOES.d,xseq=x.GOES)
plot(x.GOES,var.GOES.pred)

summary(var.out.MODIS)
summary(var.out.GOES)


var.mat.PC <- as.matrix(var.out.PC)


var.mat.MODIS <- as.matrix(var.out.MODIS)
var.mat.GOES <- as.matrix(var.out.GOES)
hist(var.mat.PC[,1],nclass=20,main="A")
hist(var.mat.MODIS[,1],add=T,col="blue",nclass=20)
hist(var.mat.GOES[,1],add=T,col="red",nclass=20)

hist(var.mat.PC[,2],nclass=20,main="B",xlim=c(-30,0))
hist(var.mat.MODIS[,2],add=T,col="blue",nclass=20)
hist(var.mat.GOES[,2],add=T,col="red",nclass=20)

plot(var.out4)

gelman.diag(var.out4)
GBR <- gelman.plot(var.out)

burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]

if(length(burnin) == 0) burnin = 1
var.burn <- window(var.out,start=burnin)
gelman.diag(var.burn)
plot(var.burn)
summary(var.burn)
