#!/usr/bin/env Rscript

#hr.seq <- c(seq(1,4,1),seq(8,24,1))
hr.seq <- c("00","01","02","03","04",as.character(seq(8,23,1)))
startDay <- 182
endDay <- 181

day.time <- list()
no.ACM <- list()
no.C02 <- list()
no.C03 <- list()

days1 <- seq(startDay,365,1)

days <- days1
for(dy in 1:length(days)){
  #print(days)
  for(hr in 1:length(hr.seq)){
    day.hr <- paste(days[dy],hr.seq[hr])
    print(day.hr)
    filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s2017",days[dy],hr.seq[hr],sep="")
    ACM.files <- dir(path="GOES_Data2017",pattern=filestrACM)
    no.ACM <- c(no.ACM,length(ACM.files))
    filestrC03 <- paste("OR_ABI-L1b-RadC-M3C03_G16_s2017",days[dy],hr.seq[hr],sep="")
    C03.files <- dir(path="GOES_Data2017",pattern=filestrC03)
    no.C03 <- c(no.C03,length(C03.files))
    filestrC02 <- paste("OR_ABI-L1b-RadC-M3C02_G16_s",days[dy],hr.seq[hr],sep="")
    C02.files <- dir(path="GOES_Data2017",pattern=filestrC02)
    no.C02 <- c(no.C02,length(C02.files))
    day.time <- c(day.time,day.hr)
  }
}
days2 <- seq(1,endDay,1)

days <- days2
for(dy in 1:length(days)){
  #print(days)
  for(hr in 1:length(hr.seq)){
    day.hr <- paste(days[dy],hr.seq[hr])
    print(day.hr)
    filestrACM <- paste("OR_ABI-L2-ACMC-M3_G16_s2018",days[dy],hr.seq[hr],sep="")
    ACM.files <- dir(path="GOES_Data2017",pattern=filestrACM)
    no.ACM <- c(no.ACM,length(ACM.files))
    filestrC03 <- paste("OR_ABI-L1b-RadC-M3C03_G16_s2018",days[dy],hr.seq[hr],sep="")
    C03.files <- dir(path="GOES_Data2017",pattern=filestrC03)
    no.C03 <- c(no.C03,length(C03.files))
    filestrC02 <- paste("OR_ABI-L1b-RadC-M3C02_G16_s2018",days[dy],hr.seq[hr],sep="")
    C02.files <- dir(path="GOES_Data2017",pattern=filestrC02)
    no.C02 <- c(no.C02,length(C02.files))
    day.time <- c(day.time,day.hr)
  }
}

write.table(rbind(day.time,no.ACM,no.C02,no.C03),"presentFiles.csv",row.names = FALSE,col.names = FALSE,sep=",")

