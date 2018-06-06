source("GOES_data.R")
source("MODIS_data.R")
source("PC_data.R")
library("rjags")
library("runjags")

iseq <- c(8,16,19,20,21)

siteData <- read.csv("GOES_Paper_Sites.csv",header=FALSE)
xpred <- seq(110,424,1)

for (i in iseq){
  i <- 7
  siteName <- as.character(siteData[i,1])
  print(siteName)
  #Lat <- as.character(siteData[i,2])
  #Long <-as.character(siteData[i,3])
  URL <- as.character(siteData[i,4])
}


##' Create a Bayes Model for a shrubland site
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
    #URL <- "https://phenocam.sr.unh.edu/data/archive/luckyhills/ROI/luckyhills_SH_0002_1day.csv"
    data = PC_data(URL=URL,startDay = 110,endDay = 424)
    print(data$x)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,-30,3),b=rnorm(1,0.11,0.05),c=rnorm(1,0.25,0.05),d=rnorm(1,0.15,0.05))
    }
    data$mean.d <- 0.3
    data$mean.c <- 0.1
  }
  else if(data.source == "MODIS.NDVI"){
    site.name <- "jernort"
    Lat <- 31.7438
    Long <- -110.0522
    data = MODIS_data(site.name=siteName)
    for(i in 1:(nchain)){
      inits[[i]] <- list(a=rnorm(1,-30,3),b=rnorm(1,0.11,0.05),c=rnorm(1,0.25,0.05),d=rnorm(1,0.15,0.05))
    }
  }
  else if(data.source=="GOES.NDVI"){
    site.name <- "luckyHills"
    data = GOES_data(site.name,startDay = 110,endDay = 424)
    #data = GOES_data(site.name,startDay = 240,endDay = 424)
    for(i in 1:(nchain)){
      inits[[i]] <- list(a=rnorm(1,30,1),b=rnorm(1,-0.14,0.01),c=rnorm(1,0.25,0.02),d=rnorm(1,0.15,0.001),r=rnorm(1,-0.02,0.002),k=rnorm(1,240,5))
      #inits[[i]] <- list(a=rnorm(1,30,1),b=rnorm(1,-0.14,0.01),c=rnorm(1,0.25,0.02),d=rnorm(1,0.15,0.02))
      #inits[[i]] <- list(c=rnorm(1,0.25,0.02),d=rnorm(1,0.15,0.02),r=rnorm(1,-0.02,0.002))
    }
    data$mean.a <- 30
    data$p.a <- 1/(3**2)#10
    data$mean.b <- -0.14
    data$p.b <- 1/(0.01**2)
    #data$mean.c <- 0.25
    data$p.c <- 1/(0.05**2)#10000
    #data$mean.d <- 0.15
    data$p.d <- 1/(0.05**2)#10000#1/(0.01**2)
    #data$alpha.c <- 4
    #data$beta.c <- 7
    #data$alpha.d <- 2
    #data$beta.d <- 6
    #data$alpha.a2 <- 3
    #data$beta.a2 <- 7
    data$mean.r <- -0.02
    data$p.r <- 1/(0.005**2)
    data$s1 <- 0.001
    data$s2 <- 0.00001
    data$mean.k <- 240
    data$p.k <- 1/(50**2)
  }
  
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
  
  #mu2[i] <- (c-(d2-d))*exp(r*(x[i]-k))+d2  ##process model for green down (exponential) (postbreak)
  
  SH_model1 <- "
  model{
  ##priors
  a ~ dnorm(mean.a,p.a)
  b ~ dnorm(mean.b,p.b)
  c ~ dnorm(mean.c,p.c)
  d ~ dnorm(mean.d,p.d)
  prec ~ dgamma(s1,s2)

  
  for(i in 1:n){
  mu[i] <- c/(1 + exp(a+b*x[i]))+d   	## process model for green up (logistic) (prebreak)
  y[i]  ~ dnorm(mu[i],prec)		## data model (will need to change to beta eventually)
  }
}
"
SH_model2 <- "
  model{
##priors

c ~ dnorm(mean.c,p.c)
d ~ dnorm(mean.d,p.d)
r ~ dnorm(mean.r,p.r)
prec ~ dgamma(s1,s2)

for(i in 1:n){
mu[i] <- c*exp(r*(x[i]-240))+d  ##process model for green down (exponential) (postbreak)
y[i]  ~ dnorm(mu[i],prec)		## data model (will need to change to beta eventually)
}
}
"
  
  j.model   <- jags.model(file = textConnection(SH_model),
                          data = data,
                          inits=inits,
                          n.chains = nchain)
  var.out   <- coda.samples (model = j.model,
                             variable.names = c("a","b","c","d","k","r","prec"),
                             n.iter = 1000)
  numb <- 1
  continue <- TRUE
  while(continue){
    print(numb)
    new.out   <- coda.samples (model = j.model,
                             variable.names = c("a","b","c","d","k","r","prec"),
                             n.iter = 1000)
    var.out <- combine.mcmc(mcmc.objects=list(var.out,new.out),collapse.chains = FALSE)
    GBR.vals <- gelman.diag(var.out)
    continue <- FALSE
    for(i in 1:nrow(GBR.vals$psrf)){
      for(j in 1:ncol(GBR.vals$psrf)){
        if(GBR.vals$psrf[i,j]>1.04){
          continue = TRUE
        }
      }
    }
    if(!continue){
      GBR <- gelman.plot(var.out)
      burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]
      if(length(burnin) == 0) burnin = 1
      var.burn <- window(var.out,start=burnin)
      var.burn <- var.out
      effsize <- effectiveSize(var.burn)
      for(i in 1:length(effsize)){
        if(effsize[i]<5000){
          continue = TRUE
        }
      }
      
    }
  numb <- numb+1
  
  }
  # var.out   <- coda.samples (model = j.model,
  #                            variable.names = c("c","d","r","prec"),
  #                            n.iter = 100000)
  

  return(var.out)
}
#var.out1 <- var.out
#var.out <- var.out2
test <- gelman.diag(var.out)
#plot(var.out)

GBR <- gelman.plot(var.out)
burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]
burnin
#var.burn <- var.out
if(length(burnin) == 0) burnin = 1
var.burn <- window(var.out,start=burnin)
#gelman.diag(var.burn)
effectiveSize(var.burn)
#plot(var.burn)
burn.sum <- summary(var.burn)
#burn.sum$statistics
plot(data$x,data$y,ylim=c(0.2,0.4))

a <- burn.sum$statistics[1,1]
#a2 <- burn.sum$statistics[2,1]
b <- burn.sum$statistics[2,1]
c <- burn.sum$statistics[3,1]
d <- burn.sum$statistics[4,1]
k <- burn.sum$statistics[5,1]
r <- burn.sum$statistics[7,1]

#c <- burn.sum$statistics[1,1]
#d <- burn.sum$statistics[2,1]
#r <- burn.sum$statistics[4,1]

#a <- burn.sum$statistics[1,1]
#a2 <- burn.sum$statistics[2,1]
#b <- burn.sum$statistics[2,1]
#c <- burn.sum$statistics[3,1]
#c2 <- burn.sum$statistics[4,1]
#d <- burn.sum$statistics[4,1]
#d2 <- burn.sum$statistics[5,1]
#k <- burn.sum$statistics[6,1]
#r <- burn.sum$statistics[8,1]


xseq <- seq(110,424)

#xseq <- seq(240,424)
#lines(xseq,pheno.logistic(a,b,c,d,xseq))
lines(xseq,shrublandYvals(a=a,b=b,c=c,d=d,k=k,r=r,xseq=xseq),col="red")
#lines(xseq,shrublandYvals(c=c,d=d,r=r,xseq=xseq))
#lines(xseq,shrublandYvals(a=32,b=-0.16,c=0.25,d=0.15,r=-0.02,k=150,xseq=xseq)) 
#abline(h=0.15)


pheno.logistic <- function(a,b,c,d,xseq){
  return(c/(1 + exp(a+b*xseq))+d)
}
shrublandYvals <- function(a,b,c,d,k,r,xseq){
  bk <- which(xseq==round(k,digits=0))
  if(k<110){
    bk <- 1
  }
  greenup <- pheno.logistic(a=a,b=b,c=c,d=d,xseq[1:bk])
  gd.xseq <- xseq[(bk+1):length(xseq)]-k
  greendown <- c*exp(r*gd.xseq)+d
  return(c(greenup,greendown))
  #return(greendown)
}


fileName <- paste(siteName,"_MODIS_varBurn.RData",sep="")
save(var.burn,file=fileName)
