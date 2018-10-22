#' Main function to calculate all NDVI values for a day for a list of sites
#'
#' @param day The desired day of year
#' @param siteData matrix of site data where the sites are in individual rows and the columns are siteName, latitude, longitude, timezone (Note: all timezone values must be the same)
#' @param year The desired year
#' @param TZ The timezone of the sites in siteData (e.g. eastern US sites have a TZ value of 5).
#' @param dataPath The directory where all of the GOES data is located
#' @export
calculateNDVI_GOES_MAIN <- function(day,siteData,year,TZ,dataPath){
  if(year==2017 && day < 321){
    orbitVersion <- "OLD"
  }
  else{
    orbitVersion <- "NEW"
  }

  filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s",year,day,sep="")
  ACM.files <- dir(path=dataPath,pattern=filestrACM)

  if(!dir.exists((paste(dataPath,dir(path=dataPath,pattern=filestrACM),sep="")))){
    if(length(ACM.files>1)){
      day.time.vals <- character()
      times <- character()
      NDVI.vals <- matrix(ncol=nrow(siteData),nrow=0)
      for(j in 1:length(ACM.files)){
        day.time <- substr(ACM.files[j],24,34)
        day.time.vals <- c(day.time.vals,day.time)
        hr <- as.numeric(substr(day.time,8,9))-TZ
        mt <- substr(day.time,10,11)
        times <- c(times,(as.numeric(hr)+as.numeric(mt)/60))
        print((as.numeric(hr)+as.numeric(mt)/60))
        NDVI.vals <- rbind(NDVI.vals,createNDVI_sub(siteData=siteData,orbitVersion=orbitVersion,day.time=day.time,dataPath=dataPath))
      }
      for(i in 1:nrow(siteData)){
        siteName <- as.character(siteData[i,1])
        fileName <- paste("GOES_NDVI_Diurnal",siteName,"_",year,day,".csv",sep="")
        output <- rbind(day.time.vals,NDVI.vals[,i],times)
        write.table(output,file=fileName,sep=",",col.names=FALSE,row.names=FALSE)
      }
    }
    else{
      createEmptyFiles(siteData=siteData,day=day)
    }
  }
  else{
    createEmptyFiles(siteData=siteData,day=day)
  }
}
