##' For GOES NDVI data, construct the data object for input into MCMC
##'
##' @param siteName Site Name
GOES_data <- function(siteName,startDay,endDay) {
  ##Data
  #NDVI.fileName1 <- paste("GOES_NDVI_",siteName,"2017_kappaDQF.csv",sep="")
  #GOES1 <- read.csv(NDVI.fileName1,header=FALSE)
  #NDVI.fileName2 <- paste("GOES_NDVI_",siteName,"2018_kappaDQF.csv",sep="")
  #GOES2 <- read.csv(NDVI.fileName2,header=FALSE)
  #GOES2[1,] <- GOES2[1,]+365
  #GOES <- cbind(GOES1,GOES2)
  GOES.fileName <- paste("GOES_NDVI_",siteName,"_kappaDQF.csv",sep="")
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

