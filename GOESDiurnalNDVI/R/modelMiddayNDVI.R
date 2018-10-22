#' Creates the parameter posterior distributions
#'
#' @param siteData matrix of site data where the sites are in individual rows and the columns are siteName, latitude, longitude, timezone
#' @param year The desired year
#' @param day The desired day of year
#' @export
#' @import rjags
#' @import runjags
modelMiddayNDVI <- function(siteData,year,day){
  for(i in 1:nrow(siteData)){
    siteName <- siteData[i,1]
    fileName <- paste("GOES_NDVI_Diurnal",siteName,"_",year,day,".csv",sep="")
    print(fileName)
    dat <- read.csv(fileName,header=FALSE)
    print(dim(dat))
    data <- list()
    data$x <- as.numeric(dat[3,])
    data$y <- as.numeric(dat[2,])
    outFileName <- paste(siteName,"_",year,day,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createDiurnalModel(siteName=siteName,data)
      var.burn <- runMCMC_Model(j.model = j.model,variableNames=c("a","c","k","prec"),iterSize = 50000)
      save(var.burn,file=outFileName)
    }
  }
}
