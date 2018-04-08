##' For MODIS EVI data, construct the data object for input into MCMC
##' 
##' @param Lat  latitude of desired site in decimals
##' @param Long longitude of desired site in decimals
##' @param data.source data source (GOES.NDVI, MODIS.NDVI, MODIS.EVI, PC.GCC)
##' @param season spring or autumn
##' @param download Boolean to indicate whether you need to download the data (true) or not (false)
MODIS_data <- function(Lat,Long,data.source,season,download) {
  ##Data
  library("MODISTools")
  
  if(download){
    if(data.source == "MODIS.EVI"){
      MODISSubsets(data.frame(lat=Lat,long=Long,start.date=2017,end.date=2017),
                   Product="MOD13Q1",Bands="250m_16_days_EVI",Size=c(1,1),StartDate=TRUE) 
    }
    if(data.source == "MODIS.NDVI"){
      MODISSubsets(data.frame(lat=Lat,long=Long,start.date=2017,end.date=2017),
                   Product="MOD13Q1",Bands="250m_16_days_NDVI",Size=c(1,1),StartDate=TRUE) 
    }
  }
  if(data.source == "MODIS.NDVI"){
    filePattern <- paste("NDVI_Lat",substr(as.character(Lat),1,5),sep="") 
  }
  else if(data.source == "MODIS.EVI"){
    filePattern <- paste("EVI_Lat",substr(as.character(Lat),1,5),sep="")
    
  }
  MODIS = read.csv(list.files(pattern=filePattern)[1],header=FALSE,as.is=TRUE,na.string="-3000")
  dat = apply(MODIS[,11:ncol(MODIS)],1,mean,na.rm=TRUE)*0.0001
  MODIS.time = as.Date(substr(MODIS[,10],1,7),format="%Y%j")
  MODIS2 <- read.csv("MODIS_Harv_Test.csv",header=FALSE)

  if(season=="spring"){
    #y <- dat[1:12]
    x <- as.numeric(MODIS2[1,])
    y <- as.numeric(MODIS2[2,])
    #x <- lubridate::yday(MODIS.time[1:12])
  }
  else if(season=="autumn"){
    y <- dat[13:23]
    x <- lubridate::yday(MODIS.time[13:23])
  }
  print(x)
  print(y)
  data <- list(x=x,y=y,n=length(y))
  ##Specify Priors
  data$beta.c <- 5
  data$alpha.c <- 3
  data$alpha.d <- 6
  data$beta.d <- 4
  data$s1 <- 0.5
  data$s2 <- 0.2
  data$v.a <- 3
  data$v.b <- 2
  if(season=="spring"){
    data$mean.a <- 30
    data$mean.b <- -0.4
  }
  else if(season=="autumn"){
    data$mean.a <- -30
    data$mean.b <- 0.11
  }
  return(data)
}