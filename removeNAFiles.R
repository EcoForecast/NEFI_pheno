files <- dir("dailyNDVI_GOES",pattern="GOES_Diurnal_dukehw")
for(i in 1:length(files)){
  dat <- read.csv(paste("dailyNDVI_GOES/",files[i],sep=""),header=FALSE)
  if(length(na.omit(as.numeric(dat[2,])))==0){
    print(files[i])
    file.remove(paste("dailyNDVI_GOES/",files[i],sep=""))
  }
}
