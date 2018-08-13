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

siteName <- "HarvardForest"

#i=191
iseq <- c("001","005","007","010","026","029","031","034","045","048","049","050","051","052","055","057","058","059","060","065","076","077","078","079","085","090","091","092","095","099","111","112","113","114","118","121","125","128","129","130","131","133","134","141","143","144","145","149","150","151","154","156","157","159","160","162","163","165","166","167","168","170","171","172","173","176","177","180","181")
output <- foreach(i = iseq) %dopar% {
  #for(i in iseq){
  #i <- iseq[4]
  fileName <- paste("dailyNDVI_GOES/","GOES_Diurnal_",siteName,"_2018",i,".csv",sep="")
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
        var.burn <- runMCMC_Model(j.model = j.model,variableNames=c("a","c","k","prec"),maxIter = 1000000)
      }
      counter <- counter + 1
    }
    save(var.burn,file=outFileName)
  }
}
