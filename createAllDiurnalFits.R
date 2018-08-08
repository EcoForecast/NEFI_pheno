#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
#install.packages("MODISTools",repo="https://cloud.r-project.org/")
#install.packages("doParallel",repo="https://cloud.r-project.org/")
library("PhenologyBayesModeling")
library("rjags")
library("runjags")
#library("MODISTools")
library("doParallel")

#detect cores.
n.cores <- detectCores()
n.cores <- 4

#register the cores.
registerDoParallel(cores=n.cores)

siteName <- "howland"
#diurnal.files <- dir(path="dailyNDVI_GOES",pattern=paste("GOES_Diurnal_",siteName,sep=""))
#iseq <- c(186,191,198,230,248,250,252,285)
#iseq <- c(seq(186,193),seq(195,201),206,207,211,217,230,231,seq(233,236),seq(244,254),258,259,seq(277,287),seq(297,299),seq(301,304),seq(313,315))
#iseq <- c(seq(182,321,1),seq(348,364,1))
#russellSage: #iseq <- c(seq(182,193),seq(195,203),seq(206,207),seq(211,213),seq(215,217),224,seq(227,231),seq(233,236),seq(244,250),seq(251,254),seq(258,260),seq(262,268),seq(271,274),seq(277,291),seq(296,299),seq(301,304),seq(306,309),seq(313,315),seq(318,320),321,355,363)
iseq <- c(seq(182,193),seq(195,203),seq(206,208),211,213,seq(215,217),224,230,231,seq(233,236),seq(244,254),seq(256,260),seq(262,258),seq(271,274),seq(277,291),seq(296,299),seq(301,304),seq(313,315),318,355,363)

#i=191

output <- foreach(i = iseq) %dopar% {
#for(i in iseq){
#i <- iseq[4]
  fileName <- paste("dailyNDVI_GOES/","GOES_Diurnal_",siteName,"_2017",i,".csv",sep="")
  print(fileName)
  dat <- read.csv(fileName,header=FALSE)
  data <- list()
  print(dim(dat))
  data$x <- as.numeric(dat[3,])
  data$y <- as.numeric(dat[2,])
  #plot(data$x,data$y)
  outFileName <- paste(siteName,"_",as.character(i),"_varBurn2.RData",sep="")
  if(!file.exists(outFileName)){
    j.model <- createBayesModel.Diurnal(siteName=siteName,data)
  
    var.burn <- runMCMC_Model(j.model = j.model,variableNames=c("a","c","k","prec"),iterSize = 50000)#,baseNum = 1000000,iterSize = 70000)
    counter <- 1
    while(counter < 5){
      if(typeof(var.burn)==typeof(FALSE)){
        j.model <- createBayesModel.Diurnal(siteName=siteName,data)
        var.burn <- runMCMC_Model(j.model = j.model,variableNames=c("a","c","k","prec"))
      }
      counter <- counter + 1
    }
    save(var.burn,file=outFileName)
  }
}
