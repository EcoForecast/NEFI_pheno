##' For MODIS EVI data, construct the data object for input into MCMC
##'
##' @param siteName Site Name
##' @param lat Latitude
##' @param long Longitude
##' @param startDay The start day counted as the day number after 2016-12-31
##' @param endDay The end day counted as the day number after 2016-12-31
##' @param metric "NDVI" or "EVI"
MODIS_data <- function(siteName,lat,long,startDay,endDay,metric) {
  startDate <- as.Date(startDay,origin="2016-12-31")
  endDate <- as.Date(endDay,origin="2016-12-31")
  fileName <- paste(siteName,"_",metric,"_MOD13Q1_",startDate,"_",endDate,".csv",sep="")
  if(!file.exists(fileName)){
    print("Downloading MODIS File")
    directory=getwd()
    mt_subset(product = "MOD13Q1",lat=lat,lon=long,band=paste("250m_16_days_",metric,sep=""),start=startDate,end=endDate,site_name = paste(siteName,"_",metric,sep=""),out_dir = directory,internal=FALSE)
  }
  print("MODIS File Downloaded")
  dat <- read.csv(fileName,header=TRUE,skip=15)
  x <- numeric()
  y <- numeric()

  for(i in 1:nrow(dat)){
    y <- c(y,dat$data[i]/10000)
    tmp <- as.Date(dat$calendar_date[i])
    x.val <- as.numeric(format(tmp, "%j"))
    if(substr(tmp,1,4)=="2018"){
      x.val <- x.val + 365
    }
    x <- c(x,x.val)
  }

  data <- list(x=x,y=y,n=length(y))
  return(data)
}
