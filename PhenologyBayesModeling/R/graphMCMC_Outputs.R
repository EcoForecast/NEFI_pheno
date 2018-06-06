graphMCMC_Outputs <- function(outputFileName,siteFileName,iseq,startDay,endDay){
  pdf(file=outputFileName,width=8,height=8)
  siteData <- read.csv(siteFileName,header=FALSE)
  xseq <- seq(startDay,endDay,1)

  for (i in iseq){
    siteName <- as.character(siteData[i,1])
    URL <- as.character(siteData[i,4])
    PFT <- as.character(siteData[i,5])

    #GOES
    data.GOES = GOES_data(siteName=siteName,startDay = startDay,endDay = endDay)
    inputFileName <- paste(siteName,"_GOES_varBurn.RData",sep="")
    load(inputFileName)

    var.mat<-as.matrix(var.burn)
    ci.GOES <- createCI(PFT=PFT,var.mat=var.mat,xseq=xseq)

    #MODIS:
    data.MODIS = MODIS_data(siteName=siteName)
    inputFileName <- paste(siteName,"_MODIS_varBurn.RData",sep="")
    load(inputFileName)

    var.mat<-as.matrix(var.burn)
    ci.MODIS <- createCI(PFT=PFT,var.mat=var.mat,xseq=xseq)

    #PC:
    data.PC = PC_data(URL=URL,startDay=startDay,endDay=endDay)
    inputFileName <- paste(siteName,"_PC_varBurn.RData",sep="")
    load(inputFileName)

    var.mat<-as.matrix(var.burn)
    ci.PC <- createCI(PFT=PFT,var.mat=var.mat,xseq=xseq)

    par(mfrow=c(1,1))
    plot(x=list(),y=list(),xlim=c(164,420),ylim=c(-0.2,1.2),ylab="Value",xlab="Day of Year",main=siteName,cex.axis=2,cex.lab=2,cex.main=2)
    lines(xseq,ci.GOES[2,],col="black",lwd=2)
    lines(xseq,ci.GOES[1,],col="black", lty = 2,lwd=2)
    lines(xseq,ci.GOES[3,],col="black", lty = 2,lwd=2)
    #abline(v=ci.GOES.k[2],col="black")
    #points(data.GOES$x,rescale(var.GOES.c,var.GOES.d,data.GOES$y),col="Black",pch=20)

    lines(xseq,ci.PC[2,],col="cyan",lwd=2)
    lines(xseq,ci.PC[1,],col="cyan", lty = 2,lwd=2)
    lines(xseq,ci.PC[3,],col="cyan", lty = 2,lwd=2)
    #abline(v=ci.PC.k[2],col="cyan")
    #points(data.PC$x,rescale(var.PC.c,var.PC.d,data.PC$y),col="cyan",pch=20)

    lines(xseq,ci.MODIS[2,],col="red",lwd=2)
    lines(xseq,ci.MODIS[1,],col="red", lty = 2,lwd=2)
    lines(xseq,ci.MODIS[3,],col="red", lty = 2,lwd=2)
    #abline(v=ci.MODIS.k[2],col="red")
    #points(data.MODIS$x,rescale(var.MODIS.c,var.MODIS.d,data.MODIS$y),col="red",pch=20)

  }
  dev.off()
}
