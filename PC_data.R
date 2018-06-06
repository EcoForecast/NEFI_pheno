source("downloadPhenoCam.R")

##' For PhenoCam data, construct the data object for input into MCMC
##' 
##' @param URL PhenoCam network URL
PC_data <- function(URL,startDay,endDay) {
  ##Data
  PC.data <- subset(download.phenocam(URL),year%in%c(2017,2018))
  PC.data <- PC.data[1:endDay,]
  PC.time = as.Date(PC.data$date)
  y <- PC.data$gcc_mean[startDay:endDay]
  x <- lubridate::yday(PC.time[startDay:endDay])
  
  if(startDay<100){ #Don't want to add to the first days of 2017
    for(i in 100:length(x)){
      if(x[i]<100){
        x[i] <- x[i]+365
      }
    }
  }
  else{
  for(i in 1:length(x)){
    if(x[i]<100){
      x[i] <- x[i]+365
    }
  }
  }
  data <- list(x=x,y=y,n=length(y))

  return(data)
}


