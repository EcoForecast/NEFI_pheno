source("downloadPhenoCam.R")

##' For PhenoCam data, construct the data object for input into MCMC
##' 
##' @param URL PhenoCam network URL
##' @param season spring or autumn
PC_data <- function(URL,season) {
  ##Data
  PC.data <- subset(download.phenocam(harvURL),year==2017)
  PC.time = as.Date(PC.data$date)
  if(season=="spring"){
    y <- PC.data$gcc_mean[1:182]
    x <- lubridate::yday(PC.time[1:182])
  }
  else if(season=="autumn"){
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
  }
  else if(season=="autumn"){
    data$mean.a <- -30
    data$mean.b <- 0.11
  }
  return(data)
}