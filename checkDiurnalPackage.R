#!/usr/bin/env Rscript

install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/GOESDiurnalNDVI",repo=NULL)
#install.packages("/Users/Kathryn/Documents/PhD_Research/NEFI_pheno/GOESDiurnalNDVI",repos=NULL,type=source)
#install.packages("/Library/Frameworks/R.framework/Versions/3.4/Resources/library/GOESDiurnalNDVI")
library("GOESDiurnalNDVI")
print("Checking calNDVI()")
print(calNDVI(0.3,0.6))
print("Testing calculateNDVI MAIN")

siteData <- read.csv("GOES_Paper_Sites.csv",header=TRUE)
siteData <- siteData[9,c(1,2,3,6)]

calculateNDVI_GOES_MAIN(day=101,siteData=siteData,TZ=5,dataPath="GOES_Data2017",year=2018)
#303
#print("Testing modelMiddayNDVI")
#modelMiddayNDVI(siteData=siteData,year=2017,day=303)

