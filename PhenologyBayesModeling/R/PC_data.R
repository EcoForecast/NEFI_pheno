##' For PhenoCam data, construct the data object for input into MCMC
##'
##' @param URL PhenoCam network URL
PC_data <- function(siteName,URL,startDay,endDay) {
  ##Data
  startDate <- as.Date(startDay,origin="2017-01-01")
  endDate <- as.Date(endDay,origin="2017-01-01")
  fileName <- paste(siteName,"_",startDate,"_",endDate,".RData",sep="")
  if(!file.exists(fileName)){
    directory=getwd()
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
    save(data,file=fileName)
  }
  load(fileName)
  return(data)
}

