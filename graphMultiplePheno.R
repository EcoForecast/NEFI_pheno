setwd("/Users/Kathryn/Documents/PhD_Research/")

library("MODISTools")

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
pdf(file="PhenoCurves.pdf",width=8,height=8)
siteData <- read.csv("GOES_Paper_Sites.csv",header=FALSE)

for (i in seq(1,nrow(siteData))){
  siteName <- siteData[i,1]
  latVal <- siteData[i,2]
  longVal <-siteData[i,3]
  NDVI.fileName <- paste("GOES_NDVI_",siteName,"2017.csv",sep="")
  GOES <- read.csv(NDVI.fileName,header=FALSE) #TRUE
  GOES_Days <- as.numeric(GOES[1,])
  GOES_NDVI <- as.numeric(GOES[2,])
  GOES.time = as.Date(GOES_Days,origin="2017-01-01")

  #MODIS EVI
  
  #only have to do the following line if you do not already have the data
  #MODISSubsets(data.frame(lat=latVal,long=longVal,start.date=2017,end.date=2017),
             #Product="MOD13Q1",Bands="250m_16_days_EVI",Size=c(1,1),StartDate=TRUE) 
  
  EVIfilePattern <- paste("EVI_Lat",substr(as.character(latVal),1,5),sep="")
  NDVIfilePattern <- paste("NDVI_Lat",substr(as.character(latVal),1,5),sep="")
  MODIS.EVI = read.csv(list.files(pattern=EVIfilePattern)[1],header=FALSE,as.is=TRUE,na.string="-3000")
  MODIS.NDVI = read.csv(list.files(pattern=NDVIfilePattern)[1],header=FALSE,as.is=TRUE,na.string="-3000")
  EVI = apply(MODIS.EVI[,11:ncol(MODIS.EVI)],1,mean,na.rm=TRUE)*0.0001
  NDVI = apply(MODIS.NDVI[,11:ncol(MODIS.NDVI)],1,mean,na.rm=TRUE)*0.0001
  MODIS.time = as.Date(substr(MODIS.EVI[,10],1,7),format="%Y%j")
  
  #PhenoCam
  URL <-  as.character(siteData[i,4])
  phenoData <- download.phenocam(URL)
  phenoData2017 <- subset(phenoData,year==2017)
  print(siteName)
  GCC <- phenoData2017$gcc_mean
  PC.time = as.Date(phenoData2017$date)
  if(i %in% c(6,8,10,12,15)){ #smaller values have a smaller ylim
    plot(MODIS.time,EVI,main=siteName,xlab="Time",ylab="Value",ylim=c(-0.2,0.5),pch=19,cex=1.5) 
  }
  else{
    plot(MODIS.time,EVI,main=siteName,xlab="Time",ylab="Value",ylim=c(-0.2,1),pch=19,cex=1.5)
  }
  points(GOES.time,GOES_NDVI,col="Blue",pch=19,cex=1.5)
  points(PC.time,GCC,col="Red",pch=19,cex=1.5)
  points(MODIS.time,NDVI,col="Magenta",pch=19,cex=1.5)

  legend("topleft",c("MODIS EVI","MODIS NDVI","PhenoCam","GOES NDVI"),pch=c(19,19,19,19),col=c("Black","Magenta","Red","Blue"),cex=c(1.5,1.5,1.5,1.5))
}

dev.off()


