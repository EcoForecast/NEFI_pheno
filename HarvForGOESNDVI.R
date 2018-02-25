HarvFor.lat <- 42.5378
HarvFor.long <- -72.1715

#GOES NDVI
GOES_NDVI <- read.csv("HarvForNDVI.csv",header=TRUE)
days <- c(seq(182,207),209,seq(211,214),seq(216,220),seq(222,231),seq(233,264),seq(266,269),seq(271,295),seq(297,301),seq(303,324),seq(328,333),seq(348,365))
PC.time = as.Date(phenoData2017$date)
GOES.time = as.Date(days,origin="2017-01-01")

#MODIS EVI
GetProducts()

MODISSubsets(data.frame(lat=HarvFor.lat,long=HarvFor.long,start.date=2017,end.date=2017),
             Product="MOD13Q1",Bands="250m_16_days_EVI",Size=c(1,1),StartDate=TRUE)

MODIS = read.csv(list.files(pattern=".asc")[1],header=FALSE,as.is=TRUE,na.string="-3000")

EVI = apply(MODIS[,11:ncol(MODIS)],1,mean,na.rm=TRUE)*0.0001
MODIS.time = as.Date(substr(MODIS[,10],1,7),format="%Y%j")
#MODIS.time = substr(MODIS[,10],1,7)

#PhenoCam

download.phenocam <- function(URL) {
  ## check that we've been passed a URL
  if (length(URL) == 1 & is.character(URL) & substr(URL,1,4)=="http") {
    
    ## read data
    dat <- read.csv(URL,skip = 22)
    
    ## convert date
    dat$date <- as.Date(as.character(dat$date))
    
    return(dat)
  } else {
    print(paste("download.phenocam: Input URL not provided correctly",URL))
  }
}

URL = "https://phenocam.sr.unh.edu/data/archive/harvard/ROI/harvard_DB_0001_1day.csv"
phenoData <- download.phenocam(URL)
phenoData2017 <- subset(phenoData,year==2017)
y=phenoData2017$gcc_mean
#PC.time = phenoData2017$date
PC.time = as.Date(phenoData2017$date)

par(mfrow=c(1,1))
par(mfrow=c(3,1))
plot(GOES.time, GOES_NDVI,main="GOES NDVI")
plot(PC.time,y,col="Blue")

plot(MODIS.time,EVI,main="Phenology at Harvard Forest in 2017",xlab="Time",ylab="Value",col="Green")
points(PC.time,y,col="Red")
points(GOES.time, GOES_NDVI,col="Blue")
legend("topleft",c("MODIS EVI","PhenoCam","GOES NDVI"),pch=c("o","o","o"),col=c("Green","Red","Blue"))

