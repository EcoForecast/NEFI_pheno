#!/usr/bin/env Rscript

siteName <- "russellSage"
fitFiles <- intersect(dir(pattern="varBurn2.RData"),dir(pattern=siteName))

days <- numeric()
for(i in 1:length(fitFiles)){
  days <- c(days,as.character(strsplit(fitFiles[i],"_")[[1]][2]))
}
iseq <- as.character(seq(1,181,1))
for(i in 1:length(iseq)){
  if(as.numeric(iseq[i])<10){
    iseq[i] <- paste("00",iseq[i],sep="")
  }
  else if(as.numeric(iseq[i])<100){
    iseq[i] <- paste("0",iseq[i],sep="")
  }
}

all.days <- c(seq(182,321,1),seq(348,364,1),iseq)
missingDays <- numeric()
for(j in 1:length(all.days)){
  if((!all.days[j] %in% days)){
    missingDays <- c(missingDays,all.days[j])
  }
}
missingDays
length(missingDays)
length(all.days)
