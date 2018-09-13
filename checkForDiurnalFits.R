#!/usr/bin/env Rscript

siteName <- "russellSage"
fitFiles <- dir(path="dailyNDVI_GOES",pattern="varBurn2.RData")
days <- numeric()
for(i in 1:length(fitFiles)){
  days <- c(days,substr(fitFiles[i],13,15))
}

#all.days <- c(seq(182,193),seq(195,203),seq(206,207),seq(211,213),seq(215,217),224,seq(227,231),seq(233,236),seq(244,250),seq(251,254),seq(258,260),seq(262,268),seq(271,274),seq(277,291),seq(296,299),seq(301,304),seq(306,309),seq(313,315),seq(318,320),321,355,363)
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
