###Script to generate a state space model forecast of PhenoCam data

##Load Required Libraries
#library(devtools)
#install_github("EcoForecast/ecoforecastR")
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
  y <- phenoData$gcc_mean
  time <-  as.Date(phenoData$date)
  
  ###Pad the x (time) and y (GCC) with future days and GCC values of NA to stimulate a forecast
  y <- c(y,rep(x=NA,times=forecastLength)) #Padded with NA's to forecast for one month into the future
  timeForecast <- c(time,seq.Date(from=time[length(time)],by="day",length.out=forecastLength))
  
  ###Construct arguments to input into the JAGS model
  data <- list(y=y,n=length(y),x_ic=y[1],tau_ic=1/(phenoData$g_std[1]**2),a_obs=0.5,r_obs=0.2,a_add=0.5,r_add=0.2)
  nchain = 3 #The number of chains in the MCMC run
  init <- list() #list of initial values
  for(i in 1:nchain){
    pheno.samp = sample(y,length(y),replace=TRUE)
    init[[i]] <- list(tau_add=1/var(diff(pheno.samp)),tau_obs=5/var(pheno.samp))
  }
  
  ###The very basic random walk that will be used as the process model in this example. In the future, this will become a lot more complicated. 
  RandomWalk = "
  model{
  
  #### Data Model
  for(i in 1:n){
  y[i] ~ dnorm(x[i],tau_obs)
  }
  
  #### Process Model
  for(i in 2:n){
  x[i]~dnorm(x[i-1],tau_add)
  }
  
  #### Priors
  x[1] ~ dnorm(x_ic,tau_ic)
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_add ~ dgamma(a_add,r_add) 
  }
  "
  
  ###Create the JAGS model using the basic RandomWalk Model
  maxIter <- 10**9 #The maximum number of iterations to wait for convergence (This number could change)
  j.model   <- jags.model (file = textConnection(RandomWalk),
                           data = data,
                           inits = init,
                           n.chains = nchain)
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("tau_add","tau_obs","x"),
                              n.iter = 1000)
  ###Check for Model Convergence and keep adding to MCMC chains if it hasn't converged and/or effective sample size is not large enough
  numb <- 1000 #The current number of iterations at this step
  continue <- TRUE #Flag that signals that the coda.samples should be rerun to produce more iterations because the model hasn't converged yet/doesnt have a large enough sample size
  GBR.bad <- TRUE #Flag that signals that it hasn't converged yet
  burnin <- 0
  while(continue & numb<maxIter){
    print(numb) #Just done to keep track of the number of iterations that have been performed
    new.out   <- coda.samples (model = j.model,
                               variable.names = c("tau_add","tau_obs","x"),
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
    
    plot(timeForecast,ci[2,],type='n',xlab="Time",ylab="Mean GCC",main=siteName,cex.lab=1.5,cex.main=2,ylim=c(0.2,0.7))
    ciEnvelope(timeForecast,ci[1,],ci[3,],col="lightBlue")
    points(time,phenoData$gcc_mean,pch="+",cex=0.5)
    abline(v=time[length(time)],col="red")
  }
  else{
    print("Model Did Not Converge")
  }
}

siteData <- read.csv("phenologyForecastSites.csv",header=TRUE)

phenologyForecast(siteName=as.character(siteData[1,1]),URL=as.character(siteData[1,4]),forecastLength = 30)
