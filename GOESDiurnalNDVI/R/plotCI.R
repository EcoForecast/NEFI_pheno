#' Plots the fitted curve with credible interval for a day
#'
#' @param siteName The site name used for file naming
#' @param year The desired year
#' @param day The desired day of year
plotCI <- function(siteName,year,day){
  ci <- calculateMiddayCI(siteName=siteName,year=year,day=day)
  date <- as.Date(as.numeric(dy),origin=as.Date(paste(as.character(as.numeric(yr)-1),"-12-31",sep="")))
  plot(x=list(),y=list(),main=date,ylim=c(0,1),xlim=c(0,24),ylab="",xlab="",cex=2.5)
  polygon(x=c(10,14,14,10),y=c(-1,-1,1.2,1.2),col="lightgray",border=NA)
  ciEnvelope(xseq,ci[1,],ci[3,],col="lightBlue")
  lines(xseq,ci[2,],col="black")
  points(as.numeric(dat[3,]),as.numeric(dat[2,]),pch=".")
  abline(v=12,col="red")
}
