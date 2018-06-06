##' For MODIS EVI data, construct the data object for input into MCMC
##' 
##' @param Lat  latitude of desired site in decimals
##' @param Long longitude of desired site in decimals
##' @param data.source data source (GOES.NDVI, MODIS.NDVI, MODIS.EVI, PC.GCC)
MODIS_data <- function(site.name) {
  ##Data
  #site.name <- "HarvardForest"
  fileName <- paste(site.name,"_MODIS_NDVI2.csv",sep="")
  
  MODIS = read.csv(fileName,header=FALSE)
  y <- MODIS[,7]
  x <- as.integer(MODIS[,5])
  for(i in 1:length(x)){
    if(x[i]<100){
      x[i] <- as.numeric(x[i]) + 365
    }
  }
  
  data <- list(x=x,y=y,n=length(y))
  
  return(data)
}