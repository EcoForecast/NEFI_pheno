#!/usr/bin/env Rscript

#install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
library("ncdf4")
library(plyr)
library("PhenologyBayesModeling")
library(doParallel)

#detect cores.
#n.cores <- 2

#register the cores.
#registerDoParallel(cores=n.cores)

##Can only do for same TZ

createNDVI_sub <- function(siteData,orbitVersion,day.time,year){
  ##Will need to return a vector of the NDVI values
  
  ##Create File Paths
  ACM.path <- paste("GOES_Data2017/","OR_ABI-L2-ACMC-M3_G16_s",day.time,sep="")
  filestrC03 <- paste("OR_ABI-L1b-RadC-M3C03_G16_s",day.time,sep="")
  filestrC02 <- paste("OR_ABI-L1b-RadC-M3C02_G16_s",day.time,sep="")
  filePathC02 <- paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC02),sep="")
  filePathC03 <- paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC03),sep="")
  NDVI <- list()
  
  ##Open Files
  print(paste("filePathC02:",filePathC02))
  print(paste("filePathC03:",filePathC03))
  if(nchar(filePathC02)>20 && nchar(filePathC03)>20){
    if(file.exists(filepathC02) && file.exists(filepathC03) && !dir.exists(filepathC02) && !dir.exists(filepathC03)){
    ACM.file <-nc_open(ACM.path)
    R2.file <- nc_open(filePathC02)
    R3.file <- nc_open(filePathC03)
    
    ##Extract Data
    R3 <- ncvar_get(R3.file,"Rad")
    R2 <- ncvar_get(R2.file,"Rad") #the full R2 dataset
    R3.kappa0 <- ncvar_get(R3.file,"kappa0")
    R2.kappa0 <- ncvar_get(R2.file,"kappa0")
    R3.DQF <- ncvar_get(R3.file,"DQF") #Data Quality Flags
    R2.DQF <- ncvar_get(R2.file,"DQF")
    R3 <- R3 * R3.kappa0 #done to covert radiance to reflectance
    R2 <- R2 * R2.kappa0
    clouds <- ncvar_get(ACM.file,"BCM")
    clouds.DQF <- ncvar_get(ACM.file,"DQF")
    
    for(i in 1:nrow(siteData)){
      ##General Site Data
      siteName <- as.character(siteData[i,1])
      lat <- as.numeric(siteData[i,2])
      long <- as.numeric(siteData[i,3])
      TZ <- as.numeric(siteData[i,6])
      
      ##Determine index values
      lat.rd <- lat*2*pi/360
      long.rd <- long*2*pi/360
      
      Ind2 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion=orbitVersion),2,orbitVersion=orbitVersion)
      Ind3 <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion=orbitVersion),3,orbitVersion=orbitVersion)
      ACM.ind <- getDataIndex(getABI_Index(lat.rd,long.rd,orbitVersion=orbitVersion),"ACM",orbitVersion=orbitVersion)
      
      i2 <- Ind2[1]
      j2 <- Ind2[2]
      i3 <- Ind3[1]
      j3 <- Ind3[2]

      if(!is.na(R3.DQF[i3,j3]) && !is.na(R2.DQF[i2,j2]) && !is.na(R2.DQF[i2,j2]) && !is.na(R2.DQF[(i2+1),j2]) && !is.na(R2.DQF[i2,(j2+1)]) && !is.na(R2.DQF[(i2+1),(j2+1)]) && !is.na(clouds[ACM.ind[1],ACM.ind[2]]) && !is.na(clouds.DQF[ACM.ind[1],ACM.ind[2]])){
        if(R3.DQF[i3,j3]==0 && R2.DQF[i2,j2]==0 && R2.DQF[i2,j2]==0 && R2.DQF[(i2+1),j2]==0 && R2.DQF[i2,(j2+1)]==0 && R2.DQF[(i2+1),(j2+1)]==0 && clouds[ACM.ind[1],ACM.ind[2]] == 0 && clouds.DQF[ACM.ind[1],ACM.ind[2]] == 0){
          R3.val <- R3[i3,j3]
          R2.val <- mean(R2[i2,j2],R2[(i2+1),j2],R2[i2,(j2+1)],R2[(i2+1),(j2+1)])
          output <- calNDVI(R2.val,R3.val)
        }
        else{
          output <- NA
        }
      }
      else{
        output <- NA
      }
      print(paste("NDVI:",output))
      NDVI <- c(NDVI,output)
    }
    }
    else{
      return(rep(NA,nrow(siteData)))
    }
  }
  else{
    return(rep(NA,nrow(siteData)))
  }
}

createEmptyFiles <- function(siteData,day,year){
  for(i in 1:length(siteData)){
    siteName <- as.character(siteData[i,1])
    fileName <- paste("GOES_NDVI_Leftover",siteName,"_",day,"_",day,"_kappaDQF.csv",sep="")
    write.table(NA,file=fileName,sep=",",col.names=FALSE,row.names=FALSE)
  }
}

checkFileExists <- function(siteName,day){
  ##Returns True if the file exists
  dat <- read.csv(file=paste(siteName,"_missing_NDVI_Days.csv",sep=""),header=FALSE)
  for(i in 1:nrow(dat)){
    if(as.numeric(dat[i,1])==as.numeric(day)){
      return(FALSE)
    }
  }
  return(TRUE)
}

createMissingFilesList <- function(siteName){
  # diurnal.files <- dir(pattern=paste("GOES_NDVI_LeftoverTEST",siteName,sep=""))
  # days <- numeric()
  # for(i in 1:length(diurnal.files)){
  #   #print(diurnal.files[i])
  #   start <- as.numeric(strsplit(diurnal.files[i],"_")[[1]][4])
  #   end <- as.numeric(strsplit(diurnal.files[i],"_")[[1]][5])
  #   #print(start)
  #   #print(end)
  #   dys <- seq(start,end,1)
  #   days <- c(days,dys)
  # }
  # #sort(days)
  all.days <- c(seq(1,320,1),seq(348,365,1))
  #all.days <- c(seq(182,320,1),seq(348,365,1))
  #all.days <- seq(1,181,1)
  # missingDays <- numeric()
  # for(j in 1:length(all.days)){
  #   if((!all.days[j] %in% days)){
  #     missingDays <- c(missingDays,all.days[j])
  #   }
  # }
  # #missingDays
  # #print(length(missingDays))
  # if(length(missingDays)==0){
  #   missingDays <- c(missingDays,0)
  # }
  missingDays <- all.days
  write.table(missingDays,file=paste(siteName,"_missing_NDVI_Days.csv",sep=""),sep=",",col.names=FALSE,row.names=FALSE)
}


createNDVI_GOES_LeftoverMAIN <- function(day,siteData,orbitVersion,year,TZ){
  print("Inside Main")
  ##hours for leftover calculations
  hrs <- as.character(c(seq((4+TZ),(9+TZ),1),seq((15+TZ),(17+TZ),1))) 
  
  ##
  filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s",year,day,sep="")
  ACM.files <- character()
  for(q in 1:length(hrs)){
    if(hrs[q]<10){
      hrs[q] <- paste("0",hrs[q],sep="")
    }
    #print(c("hrs[q]",hrs[q]))
    newFiles <- intersect(dir(path="GOES_Data2017",pattern=filestrACM),dir(path="GOES_Data2017",pattern=paste("s",year,day,hrs[q],sep="")))
    ACM.files <- c(ACM.files,newFiles)
  }
  print(ACM.files)
  
  #if(!dir.exists((paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrACM),sep="")))){
    if(length(ACM.files>1)){
      NDVI.vals <- numeric()
      day.time.vals <- character()
      for(j in 1:length(ACM.files)){
        ##Open Files
        day.time <- substr(ACM.files[j],24,34)
        day.time.vals <- c(day.time.vals,day.time)
        NDVI <- createNDVI_sub(siteData=siteData,orbitVersion=orbitVersion,day.time=day.time) 
        NDVI.vals <- cbind(NDVI.vals,NDVI)
      }
      for(i in 1:nrow(siteData)){
        siteName <- as.character(siteData[i,1])
        fileName <- paste("GOES_NDVI_LeftoverTEST",siteName,"_",day,"_",day,"_kappaDQF.csv",sep="")
        output <- rbind(day.time.vals,NDVI.vals[i])
        write.table(output,file=fileName,sep=",",col.names=FALSE,row.names=FALSE)
      }
    }
    else{
      createEmptyFiles(siteData=siteData,day=day)
    }
  # }
  # else{
  #   createEmptyFiles(siteData=siteData,day=day)
  # }
}

#########

##For TZ == 5 
#siteData <- read.csv("GOES_Paper_Sites.csv",header=TRUE)[c(2,3,4,5,8,16,17,19,20),]
siteData <- read.csv("GOES_Paper_Sites.csv",header=TRUE)[c(2,3,4),]
for(s in 1:nrow(siteData)){
  siteName <- as.character(siteData[s,1])
  #print(siteName)
  createMissingFilesList(siteName)
}
print("Done creating missing day files")
#all.days <- c(seq(1,320,1),seq(348,365,1))
all.days <- c(seq(182,320,1),seq(348,365,1))
year <- 2017
#foreach (d = 1:length(all.days)) %dopar% {
for(d in 1:length(all.days)){
  print(paste("Starting Day:",all.days[d],sep=" "))
  iseq <- numeric()
  for(s in 1:nrow(siteData)){
    siteName <- as.character(siteData[s,1])
    #print(siteName)
    if(!checkFileExists(siteName = siteName,day=all.days[d])){
      iseq <- c(iseq,s)
    }
  }
  print(iseq)
  if(all.days[d]<321 && all.days[d]>181){
    orbitVersion <- "OLD"
  }
  else{
    orbitversion <- "NEW"
  }
  createNDVI_GOES_LeftoverMAIN(day=all.days[d],siteData=siteData[iseq,],orbitVersion = orbitVersion,year = year,TZ = 5)
}

