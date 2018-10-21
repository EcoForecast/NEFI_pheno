#' Calculate credible interval on midday NDVI value
#'
#' @param siteName The site name used for file naming
#' @param year The desired year
#' @param day The desired day of year
#' @export
#' @import rjags
#' @import runjags
calculateMiddayCI <- function(siteName,year,day){
  fileName <- paste(siteName,"_",year,day,"_varBurn.RData",sep="")
  load(fileName)
  dat <- read.csv(paste("GOES_NDVI_Diurnal",siteName,"_",year,day,".csv",sep=""))
  out.mat <- as.matrix(var.burn)
  a <- out.mat[,1]
  c <- out.mat[,2]
  k <- out.mat[,3]
  ycred <- matrix(0,nrow=10000,ncol=length(xseq))
  for(g in 1:10000){
    Ey <- diurnalExp(a=a[g],c=c[g],k=k[g],xseq=xseq)
    ycred[g,] <- Ey
  }
  ci <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  return(ci)
}

