#!/usr/bin/env Rscript

library("ncdf4")
library(plyr)

calNDVI <- function(R2,R3){
  return((R3-R2)/(R3+R2))
}

createNDVI <- function(day.time){
  #Function to create a file with NDVI values for a specific day/time
  #outputs a csv file
  #day.time needs to be in the format "20171821658"
  #will need to change this to account for the files being in a different folder
  filestrC02 <- paste("OR_ABI-L1b-RadC-M3C02_G16_s",day.time,sep="")
  filestrC03 <- paste("OR_ABI-L1b-RadC-M3C03_G16_s",day.time,sep="")
  R3 <- ncvar_get(nc_open(dir(pattern=filestrC03)),"Rad")
  R2 <- ncvar_get(nc_open(dir(pattern=filestrC02)),"Rad") #the full R2 dataset
  NDVI.vals <- matrix(ncol=3000,nrow=5000)
  for(i in seq(1,nrow(R2),2)){
    for(j in seq(1,ncol(R2),2)){
      R2.val <- mean(R2[i,j],R2[(i+1),j],R2[i,(j+1)],R2[(i+1),(j+1)])
      R3.val <- R3[(i/2),(j/2)]
      NDVI.vals[(i/2),(j/2)] <- calNDVI(R2.val,R3.val)
    }
    if(i%%500==1){
      print(i) #done to keep track of progress
    }
  }
  file.output.name <- paste("GOES16_NDVI_",day.time,".csv",sep="")
  write.table(NDVI.vals,file.output.name,row.names=FALSE,sep=",")
  return(NDVI.vals)
}

getSpecificNDVI <- function(ind2,ind3,day.time){
  #Function to calculate the NDVI when you have a known pixel
  #print("Entered getSpecificNDVI")
  filestrC02 <- paste("OR_ABI-L1b-RadC-M3C02_G16_s",day.time,sep="")
  filestrC03 <- paste("OR_ABI-L1b-RadC-M3C03_G16_s",day.time,sep="")
  filepathC02 <- paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC02),sep="")
  filepathC03 <- paste("GOES_Data2017/",dir(path="GOES_Data2017",pattern=filestrC03),sep="")
  if(file.exists(filepathC02) && file.exists(filepathC03) && !dir.exists(filepathC02) && !dir.exists(filepathC03)){
    R2.file <-nc_open(filepathC02)
    R3.file <-nc_open(filepathC02)

    R3 <- ncvar_get(R3.file,"Rad")
    R2 <- ncvar_get(R2.file,"Rad") #the full R2 dataset
    R3.kappa0 <- ncvar_get(R3.file,"kappa0")
    R2.kappa0 <- ncvar_get(R2.file,"kappa0")
    R3.DQF <- ncvar_get(R3.file,"DQF") #Data Quality Flags
    R2.DQF <- ncvar_get(R2.file,"DQF")
    R3 <- R3 * R3.kappa0 #done to covert radiance to reflectance
    R2 <- R2 * R2.kappa0

    i2 <- ind2[1]
    j2 <- ind2[2]
    i3 <- ind3[1]
    j3 <- ind3[2]

    if(!is.na(R3.DQF[i3,j3]) && !is.na(R2.DQF[i2,j2]) && !is.na(R2.DQF[i2,j2]) && !is.na(R2.DQF[(i2+1),j2]) && !is.na(R2.DQF[i2,(j2+1)]) && !is.na(R2.DQF[(i2+1),(j2+1)])){
      if(R3.DQF[i3,j3]==0 && R2.DQF[i2,j2]==0 && R2.DQF[i2,j2]==0 && R2.DQF[(i2+1),j2]==0 && R2.DQF[i2,(j2+1)]==0 && R2.DQF[(i2+1),(j2+1)]==0){
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
  }
  else{
    output <- NA
  }
  return(output)
}
