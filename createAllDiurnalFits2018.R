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
n.cores <- 6

#register the cores.
registerDoParallel(cores=n.cores)

siteName <- "Shenandoah"
#diurnal.files <- dir(path="dailyNDVI_GOES",pattern=paste("GOES_Diurnal_",siteName,sep=""))
#iseq <- c(186,191,198,230,248,250,252,285)
#iseq <- c(seq(186,193),seq(195,201),206,207,211,217,230,231,seq(233,236),seq(244,254),258,259,seq(277,287),seq(297,299),seq(301,304),seq(313,315))
#russellSage: #iseq <- c("001","013","015","017","018","022","023","024","025","028","029","030","031","032","033","035","036","039","046","053","057","058","061","062","065","066","067","068","071","072","073","074","078","080","081","082","085","086","089","090","091","093","094","095","100","101","102","105","106","107","109","110","114","118","119","120","121","121","125","126","127","152","153","156","157","158","159","160","161","161","164","166","167","168","169","172","173","174","175","176","177","178","179","180","181")
#i=191
#iseq <- c("001","013","015","018","022","023","024","025","028","029","030","031","032","033","035","036","039","046","057","058","051","052","055","066","067","068","071","072","073","074","078","080","081","082","089","090","091","093","094","095","100","101","102","105","106","107","109","110","114","117","118","119","120","121","122","125","126","127","128","129","130","131","132","133",as.character(seq(134,139)),"141","143","145",as.character(seq(147,153)),"157","158","159","160","161","163","164","166","164","166","167","167","168","169","172","173","174","176","177","178","179","180","181")
iseq <- as.character(seq(1,181,1))
for(i in 1:length(iseq)){
  if(as.numeric(iseq[i])<10){
    iseq[i] <- paste("00",iseq[i],sep="")
  }
  else if(as.numeric(iseq[i])<100){
    iseq[i] <- paste("0",iseq[i],sep="")
  }
}

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
