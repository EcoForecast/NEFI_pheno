##' For PhenoCam data, construct the data object for input into MCMC
##'
##' @param URL PhenoCam network URL
##' @param siteName The site name
##' @param startDay The start day counted as the day number after 2016-12-31
##' @param endDay The end day counted as the day number after 2016-12-31
PC_data <- function(siteName,URL,startDay,endDay) {
  ##Data
  startDate <- as.Date(startDay,origin="2016-12-31")
  endDate <- as.Date(endDay,origin="2016-12-31")
  fileName <- paste(siteName,"_",startDate,"_",endDate,"PC.RData",sep="")
  if(!file.exists(fileName)){
    PC.data <- subset(download.phenocam(URL),year%in%c(2017,2018))
    PC.data <- PC.data[1:endDay,]
    PC.time = as.Date(PC.data$date)
    y <- PC.data$midday_gcc[startDay:endDay]
    x <- lubridate::yday(PC.time[startDay:endDay])
    bk <- which(xseq==366)
    for(i in bk:length(x)){
      x[i] <- x[i]+365
    }
    data <- list(x=x,y=y,n=length(y))
    save(data,file=fileName)
  }
  load(fileName)
  return(data)
}

