#!/usr/bin/env Rscript

siteName <- "russellSage"
fitFiles <- intersect(dir(pattern="varBurn2.RData"),dir(pattern=siteName))

days <- numeric()
for(i in 1:length(fitFiles)){
  days <- c(days,as.character(strsplit(fitFiles[i],"_")[[1]][2]))
}

all.days <- c(seq(182,321,1),seq(348,364,1))
missingDays <- numeric()
for(j in 1:length(all.days)){
  if((!all.days[j] %in% days)){
    missingDays <- c(missingDays,all.days[j])
  }
}
missingDays
length(missingDays)
length(all.days)
