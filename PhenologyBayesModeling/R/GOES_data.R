##' For GOES NDVI data, construct the data object for input into MCMC
##'
##' @param siteName Site Name
##' @param lat Latitude
##' @param long Longitude
##' @param startDay The start day counted as the day number after 2016-12-31
##' @param endDay The end day counted as the day number after 2016-12-31
##' @param TZ The timezone off of UTC
GOES_data <- function(siteName,lat,long,startDay,endDay,TZ) {
  startDate <- as.Date(startDay,origin="2016-12-31")
  endDate <- as.Date(endDay,origin="2016-12-31")
  fileName <- paste("GOES_NDVI_",siteName,"_",startDate,"_",endDate,"_noon.csv",sep="")
  if(!file.exists(GOES.fileName)){
    createNDVI_GOES(lat=lat,long=long,startDay=startDay,endDay=endDay,fileName=fileName,TZ=TZ)
  }
  GOES <- read.csv(GOES.fileName,header=FALSE)

  GOES_Days <- as.numeric(GOES[1,])
  GOES_NDVI <- as.numeric(GOES[2,])

  for(i in 1:length(GOES_Days)){
    if(GOES_Days[i]<100){
      GOES_Days[i] <- as.numeric(GOES_Days[i]) + 365
    }
  }

  days <- which(GOES_Days>startDay&GOES_Days<endDay)

  y <- GOES_NDVI[days]
  x <- GOES_Days[days]

  data <- list(x=x,y=y,n=length(y))
  return(data)
}

