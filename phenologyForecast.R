
library("ecoforecastR")
library("rjags")
library("runjags")

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

##' Create the credible interval envelope for plotting
##' 
##' @param x time range
##' @param ylo the bottom credible interval values
##' @param yhi the top credible interval values
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

##' Executes a phenology forecast based on PhenoCam data
##' 
##' @param siteName The site name to be printed on the graph
##' @param URL The PhenoCam URL
##' @param forecastLength The number of days in the future you want to forecast
phenologyForecast <- function(siteName,URL,forecastLength){
  ###Download PhenoCam data and format 
  phenoData <- download.phenocam(URL)
  p <- phenoData$gcc_mean
  x <-  as.Date(phenoData$date)
  
  startDate <- x[1]
  endDate <- x[length(x)]
  #startDate <- as.Date("2008-04-04")
  #endDate <- as.Date("2018-08-27")
  
  ###Pad the x (time) and p (GCC) with future days and GCC values of NA to stimulate a forecast
  # p <- c(p,rep(x=NA,times=forecastLength)) #Padded with NA's to forecast for one month into the future
  # 
  metric <- "NDVI"
  MODISfileName <- paste(siteName,"_",metric,"_MOD13Q1_",startDate,"_",endDate,".csv",sep="")
  print(MODISfileName)
  if(!file.exists(MODISfileName)){
    print("Downloading MODIS File")
    directory="/Users/Kathryn/Documents/PhD_Research/NEFI_pheno/PhenologyForecast"
    mt_subset(product = "MOD13Q1",lat=lat,lon=long,band=paste("250m_16_days_",metric,sep=""),start=startDate,end=endDate,site_name = paste(siteName,"_",metric,sep=""),out_dir = directory,internal=FALSE)
  }
  print("MODIS File Downloaded")
  dat <- read.csv(MODISfileName,header=TRUE,skip=15)
  MODIS.x <- as.Date(dat$calendar_date)
  MODIS.y <- as.numeric(dat$data)/10000
  m <- rep(NA,length(p))
  for(i in 1:length(MODIS.x)){
    m[which(x==MODIS.x[i])] <- MODIS.y[i]
  }
  GOES.data <- read.csv("GOES_NDVI_HarvardForest_2017-07-01_2018-06-30_noon.csv",header=FALSE)
  GOES.x <- as.Date(rep(NA,ncol(GOES.data)))
  for(d in 1:ncol(GOES.data)){
    print(as.numeric(GOES.data[1,d]))
    if(GOES.data[1,d]<182){
      GOES.data[1,d] <- GOES.data[1,d]+365
    }
    GOES.x[d] <- as.Date(GOES.data[1,d],origin="2016-12-31")
    print(GOES.x[d])
  }
  GOES.y <- GOES.data[2,]
  g <- rep(NA,length(p))
  for(i in 1:length(GOES.x)){
    g[which(x==GOES.x[i])] <- GOES.y[i]
  }
  
  #timeForecast <- c(time,seq.Date(from=time[length(time)],by="day",length.out=forecastLength))
  
  ###Construct arguments to input into the JAGS model
  data <- list()
  data$p <- p
  data$n <- length(p)
  data$x_ic <- p[1]
  data$tau_ic <- 1/(phenoData$g_std[1]**2)
  data$a_obs <- 0.5
  data$r_obs <- 0.2
  data$a_add <- 0.5
  data$r_add <- 0.2
  data$m <- m
  data$s1 <- 0.5
  data$s2 <- 0.2
  data$g <- g
  
  #data <- list(p=y,n=length(y),x_ic=y[1],tau_ic=1/(phenoData$g_std[1]**2),a_obs=0.5,r_obs=0.2,a_add=0.5,r_add=0.2)
  nchain = 3 #The number of chains in the MCMC run
  init <- list() #list of initial values
  for(i in 1:nchain){
    pheno.samp = sample(p,length(p),replace=TRUE)
    init[[i]] <- list(p.proc=1/var(diff(pheno.samp)),p.PC=5/var(pheno.samp))
  }
  
  ###The very basic random walk that will be used as the process model in this example. In the future, this will become a lot more complicated. 
  ###The very basic random walk that will be used as the process model in this example. In the future, this will become a lot more complicated. 
  RandomWalk = "
  model{
  
  #### Data Model
  for(i in 1:n){
  p[i] ~ dnorm(x[i],p.PC)
  }
  
  #### Data Model: MODIS NDVI
  for(i in 1:n){
  m[i] ~ dnorm(x[i],p.MN)
  }
  
  #### Data Model: GOES
  for(i in 1:n){
  g[i] ~ dnorm(x[i],p.G)
  }
  
  #### Process Model
  for(i in 2:n){
  x[i]~dnorm(x[i-1],p.proc)
  }
  
  #### Priors
  x[1] ~ dnorm(x_ic,tau_ic)
  p.PC ~ dgamma(a_obs,r_obs)
  p.MN ~ dgamma(s1,s2) #obervation error on Modis NDVI
  p.G ~ dgamma(s1,s2)
  p.proc ~ dgamma(a_add,r_add) 
  }
  "
  
  
  
  ###Create the JAGS model using the basic RandomWalk Model
  maxIter <- 10**9 #The maximum number of iterations to wait for convergence (This number could change)
  j.model   <- jags.model (file = textConnection(RandomWalk),
                           data = data,
                           inits = init,
                           n.chains = nchain)
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("p.proc","p.PC","p.G","x"),
                              n.iter = 1000)
  ###Check for Model Convergence and keep adding to MCMC chains if it hasn't converged and/or effective sample size is not large enough
  numb <- 1000 #The current number of iterations at this step
  continue <- TRUE #Flag that signals that the coda.samples should be rerun to produce more iterations because the model hasn't converged yet/doesnt have a large enough sample size
  GBR.bad <- TRUE #Flag that signals that it hasn't converged yet
  burnin <- 0
  while(continue & numb<maxIter){
    print(numb) #Just done to keep track of the number of iterations that have been performed
    new.out   <- coda.samples (model = j.model,
                               variable.names = c("p.proc","p.PC","p.G","x"),
                               n.iter = 1000)
    numb <- numb + 1000
    jags.out <- combine.mcmc(mcmc.objects=list(jags.out,new.out),collapse.chains = FALSE)
    continue <- FALSE
    if(GBR.bad){
      out = list(params=NULL,predict=NULL) #Split output into parameters and state variables
      mfit = as.matrix(jags.out,chains=TRUE)
      pred.cols = grep("x[",colnames(mfit),fixed=TRUE)
      chain.col = which(colnames(mfit)=="CHAIN")
      out$params = ecoforecastR::mat2mcmc.list(mfit[,-pred.cols])
      GBR.vals <- gelman.diag(out$params)
      for(i in 1:nrow(GBR.vals$psrf)){ #Checking to see if any of the parameters haven't converged yet
        for(j in 1:ncol(GBR.vals$psrf)){
          if(!is.nan(GBR.vals$psrf[i,j])){
            if(GBR.vals$psrf[i,j]>1.04){
              continue <-  TRUE
            }
          }
        }
      }
    }
    if(!continue){
      if(burnin==0){ #If the while loop has to be rerun because the effective size is too small, you don't need to calculate burnin again
        GBR <- gelman.plot(out$params)
        burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]
        if(length(burnin) == 0) burnin = 1
      }
      var.burn <- window(jags.out,start=burnin)
      out.burn = list(params=NULL,predict=NULL)
      mfit = as.matrix(var.burn,chains=TRUE)
      pred.cols = grep("x[",colnames(mfit),fixed=TRUE)
      chain.col = which(colnames(mfit)=="CHAIN")
      out.burn$params = ecoforecastR::mat2mcmc.list(mfit[,-pred.cols])
      effsize <- effectiveSize(out.burn$params)
      print(effsize)
      for(i in 1:length(effsize)){
        if(effsize[i]<5000){
          continue = TRUE
        }
      }
    }
  }
  if(!continue){
    out.burn$predict = ecoforecastR::mat2mcmc.list(mfit[,c(chain.col,pred.cols)])
    
    ###Visualize Output
    ci <- apply(as.matrix(out.burn$predict),2,quantile,c(0.025,0.5,0.975)) #Computes the 95% credible interval (CI)
    return(ci)
    #plot(x,ci[2,],type='n',xlab="Time",ylab="Mean GCC",main=siteName,cex.lab=1.5,cex.main=2,ylim=c(0.2,0.7))
    #ciEnvelope(timeForecast,ci[1,],ci[3,],col="lightBlue")
    #points(time,phenoData$gcc_mean,pch="+",cex=0.5)
    #abline(v=time[length(time)],col="red")
  }
  else{
    print("Model Did Not Converge")
  }
}

#siteData <- read.csv("phenologyForecastSites.csv",header=TRUE)

ci <- phenologyForecast(siteName=as.character(siteData[1,1]),URL=as.character(siteData[1,4]),forecastLength = 30)
URL <- as.character(siteData[1,4])
phenoData <- download.phenocam(URL)
p <- phenoData$gcc_mean
x <-  as.Date(phenoData$date)
plot(x,p)
ciEnvelope(x,ci[1,],ci[3,],col="lightblue")
