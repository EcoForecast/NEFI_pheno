#' General phenology forecast function
#'
#' @param forecastType The type of forecast (randomWalk or logistic)
#' @param forecastLength The length of the forecast into the future in days
#' @param siteName The site name
#' @param URL The URL where the site's phenocam data is located
#' @param lat The latitude of the site in decimals
#' @param long The longitude of the site in decimals
#' @param dataDirectory The file path for the directory to download and store data in
#' @param startDate The start date for the forecast in date form
#' @param endDate The end date for the forecast in date form
#' @import rjags
#' @import runjags
#' @import coda
#' @export
phenologyForecast <- function(forecastType,forecastLength,siteName,URL,lat,long,dataDirectory,startDate,endDate){
  ###Download PhenoCam data and format
  phenoData <- download.phenocam(URL)
  p <- phenoData$gcc_mean
  time <-  as.Date(phenoData$date)

  ###Pad the x (time) and y (GCC) with future days and GCC values of NA to stimulate a forecast
  p <- c(p,rep(x=NA,times=forecastLength)) #Padded with NA's to forecast for one month into the future
  timeForecast <- c(time,seq.Date(from=time[length(time)],by="day",length.out=forecastLength))
  data <- list()
  data$p <- rescaleObs(time=timeForecast,vals=p)

  ##Download and format MODIS NDVI data
  data$mn <- prepareMODIS(startDate=startDate,endDate=endDate,metric="NDVI",timeForecast=timeForecast,dataDirectory=dataDirectory,siteName=siteName)
  data$me <- prepareMODIS(startDate=startDate,endDate=endDate,metric="EVI",timeForecast=timeForecast,dataDirectory=dataDirectory,siteName=siteName)

  data$n <- length(data$p)
  data$x_ic <- 0
  data$tau_ic <- 1/(phenoData$g_std[1]**2)
  nchain=3

  ##Create Models and assign variable names
  if(forecastType=="randomWalk"){
    j.model <- randomWalkPhenoModel(data=data,nchain=nchain)
    variableNames <- c("p.PC","p.MN","p.ME","p.proc","x")
  }
  else if(forecastType=="logistic"){
    j.model <- logisticPhenoModel(data=data,nchain=nchain)
    variableNames <- c("p.proc","p.PC","p.ME","p.MN","x","r")

  }

  ##Run Model until convergence with large enough sample size
  out.burn <- runForecastIter(j.model=j.model,variableNames=variableNames)
  return(out.burn)

}
