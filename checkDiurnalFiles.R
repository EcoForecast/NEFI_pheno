siteName <- "howland"
diurnal.files <- dir(pattern=paste("GOES_NDVI_Diurnal",siteName,"_",sep=""))

NDVI.files <- dir(pattern="GOES_NDVI_DiurnalrussellSage_")
#length(NDVI.files)
dat <- read.csv(NDVI.files[1],header=FALSE)
for(i in 2:length(NDVI.files)){
  print(NDVI.files[i])
  dat <- cbind(dat,read.csv(NDVI.files[i],header=FALSE))
}
diurnal.files <- dir(pattern=paste("GOES_NDVI_Diurnal",siteName,"_",sep=""))
days <- numeric()
for(i in 1:length(diurnal.files)){
  start <- as.numeric(strsplit(diurnal.files[i],"_")[[1]][4])
  end <- as.numeric(strsplit(diurnal.files[i],"_")[[1]][5])
  dys <- seq(start,end,1)
  days <- c(days,dys)
}
#sort(days)
all.days <- c(seq(1,321,1),seq(347,365,1))
missingDays <- numeric()
for(j in 1:length(all.days)){
  if((!all.days[j] %in% days)){
    missingDays <- c(missingDays,all.days[j])
  }
}
missingDays
length(missingDays)
