library(rjags)
library(coda)
library("MODISTools")
library("numDeriv")


##' Download Phenocam data
##' 
##' @param URL  web address where data is located
download.phenocam <- function(URL) {
  ## check that we've been passed a URL
  if (length(URL) == 1 & is.character(URL) & substr(URL,1,4)=="http") {
    
    ## read data
    dat <- read.csv(URL,skip = 22)
    
    ## convert date
    dat$date <- as.Date(as.character(dat$date))
    
    return(dat)
  } else {
    print(paste("download.phenocam: Input URL not provided correctly",URL))
  }
}

##' For GOES NDVI data, construct the data object for input into MCMC
##' 
##' @param site.name Site Name
GOES_data <- function(site.name) {
  ##Data
  NDVI.fileName1 <- paste("GOES_NDVI_",site.name,"2017_kappaDQF2.csv",sep="")
  GOES1 <- read.csv(NDVI.fileName1,header=FALSE) 
  NDVI.fileName2 <- paste("GOES_NDVI_",site.name,"2018_kappaDQF.csv",sep="")
  GOES2 <- read.csv(NDVI.fileName2,header=FALSE)
  GOES2[1,] <- GOES2[1,]+365
  GOES <- cbind(GOES1,GOES2)
  
  GOES_Days <- as.numeric(GOES[1,])
  GOES_NDVI <- as.numeric(GOES[2,])
  
  sep.val <- min(which(GOES_Days>182)) 
  y <- GOES_NDVI[sep.val:length(GOES_Days)]
  x <- GOES_Days[sep.val:length(GOES_Days)]
  
  data <- list(x=x,y=y,n=length(y))
  ##Specify Priors
  data$alpha.c <- 4
  data$beta.c <- 3
  data$alpha.d <- 2
  data$beta.d <- 3
  
  return(data)
}

##' For PhenoCam data, construct the data object for input into MCMC
##' 
##' @param URL PhenoCam network URL
PC_data <- function(URL) {
  ##Data
  PC.data <- subset(download.phenocam(URL),year%in%c(2017,2018))
  PC.data <- PC.data[1:425,]
  PC.time = as.Date(PC.data$date)
  y <- PC.data$gcc_mean[164:425]
  x <- lubridate::yday(PC.time[164:425])
  for(i in 1:length(x)){
    if(x[i]<100){
      x[i] <- x[i]+365
    }
  }
  data <- list(x=x,y=y,n=length(y))
  ##Specify Priors
  data$beta.c <- 5
  data$alpha.c <- 1
  data$alpha.d <- 3
  data$beta.d <- 7
  return(data)
}

##' For MODIS EVI data, construct the data object for input into MCMC
##' 
##' @param Lat  latitude of desired site in decimals
##' @param Long longitude of desired site in decimals
##' @param data.source data source (GOES.NDVI, MODIS.NDVI, MODIS.EVI, PC.GCC)
MODIS_data <- function(Lat,Long,data.source,site.name) {
  fileName <- paste(site.name,"_MODIS_NDVI2.csv",sep="")

  MODIS = read.csv(fileName,header=FALSE)
  y <- MODIS[,7]
  x <- as.integer(MODIS[,5])
  for(i in 1:length(x)){
    if(x[i]<100){
      x[i] <- as.numeric(x[i]) + 365
    }
  }
  
  data <- list(x=x,y=y,n=length(y))
  ##Specify Priors
  data$alpha.c <- 1
  data$beta.c <- 5
  data$alpha.d <- 3.5
  data$beta.d <- 5
  
  return(data)
}

pheno.logistic <- function(a,b,c,d,xseq){
  return(c/(1 + exp(a+b*xseq))+d)
}

rescale <- function(c,d,yseq){
  return((yseq-c-d)/c+1)
}

########################################################################

pdf(file="DB_Bayes_Fits2.pdf",width=8,height=8)

results.table <- data.frame(matrix(nrow=1,ncol=6))
colnames(results.table) <- c("site","Source","a+-SE","b+-SE","c+-SE","d+-SE")

iseq <- c(1,2,3,7,8,9)
trans.dates <- matrix(nrow=10,ncol=9)
colnames(trans.dates) <- c("GOES 0.025","GOES 0.5","GOES 0.975","MODIS 0.025","MODIS 0.5","MODIS 0.975","PC 0.025","PC 0.5","PC 0.975")

siteData <- read.csv("GE509_Project_Sites.csv",header=FALSE)
xpred <- seq(164,425,1)

for (i in iseq){
  siteName <- as.character(siteData[i,1])
  siteName <- site.name
  Lat <- as.character(siteData[i,2])
  Long <-as.character(siteData[i,3])
  URL <- as.character(siteData[i,4])
  
  #GOES
  data.GOES = GOES_data(site.name,startDay = 110,endDay = 424)
  inputFileName <- paste(siteName,"_GOES_varBurn.RData",sep="")
  load(inputFileName)
  burn.sum <- summary(var.burn)
  
  var.GOES.a <- burn.sum$statistics[1,1]
  var.GOES.b <- burn.sum$statistics[2,1]
  var.GOES.c <- burn.sum$statistics[3,1]
  var.GOES.d <- burn.sum$statistics[4,1]
  
  a_stat <-paste(as.character(round(burn.sum$statistics[1,1],digits=4)),"+-",as.character(round(burn.sum$statistics[1,2],digits=4)))
  b_stat <-paste(as.character(round(burn.sum$statistics[2,1],digits=4)),"+-",as.character(round(burn.sum$statistics[2,2],digits=4)))
  c_stat <-paste(as.character(round(burn.sum$statistics[3,1],digits=4)),"+-",as.character(round(burn.sum$statistics[3,2],digits=4)))
  d_stat <-paste(as.character(round(burn.sum$statistics[4,1],digits=4)),"+-",as.character(round(burn.sum$statistics[4,2],digits=4)))
  results.table <- rbind(results.table,c(siteName,"GOES",a_stat,b_stat,c_stat,d_stat))
  
  var.GOES.pred <- pheno.logistic(var.GOES.a,var.GOES.b,var.GOES.c,var.GOES.d,xseq=xpred)
  var.mat<-as.matrix(var.burn)
  
  a<-var.mat[,1]
  b<-var.mat[,2]
  c <- var.mat[,3]
  d <- var.mat[,4]

  ycred <- matrix(0,nrow=10000,ncol=length(xpred))
  half.vals <- rep(NA,10000)
  
  for(g in 1:10000){
    Ey <- (c[g]/(1 + exp(a[g]+b[g]*xpred))+d[g])
    ycred[g,] <- Ey
    half.vals[g] <- -a[g]/b[g]
  }
  ci <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  ci.GOES <- ci
  trans.dates[i,1:3] <- quantile(half.vals,c(0.025,0.5, 0.975), na.rm= TRUE)

  #MODIS:
  data.MODIS = MODIS_data(Lat,Long,data.source,site.name=siteName)
  inputFileName <- paste(siteName,"_MODIS_varBurn.RData",sep="")
  load(inputFileName)

  burn.sum <- summary(var.burn)
  
  var.MODIS.a <- burn.sum$statistics[1,1]
  var.MODIS.b <- burn.sum$statistics[2,1]
  var.MODIS.c <- burn.sum$statistics[3,1]
  var.MODIS.d <- burn.sum$statistics[4,1]
  
  a_stat <-paste(as.character(round(burn.sum$statistics[1,1],digits=4)),"+-",as.character(round(burn.sum$statistics[1,2],digits=4)))
  b_stat <-paste(as.character(round(burn.sum$statistics[2,1],digits=4)),"+-",as.character(round(burn.sum$statistics[2,2],digits=4)))
  c_stat <-paste(as.character(round(burn.sum$statistics[3,1],digits=4)),"+-",as.character(round(burn.sum$statistics[3,2],digits=4)))
  d_stat <-paste(as.character(round(burn.sum$statistics[4,1],digits=4)),"+-",as.character(round(burn.sum$statistics[4,2],digits=4)))
  results.table <- rbind(results.table,c(siteName,"MODIS",a_stat,b_stat,c_stat,d_stat))
  
  var.MODIS.pred <- pheno.logistic(var.MODIS.a,var.MODIS.b,var.MODIS.c,var.MODIS.d,xseq=xpred)
  
  var.mat<-as.matrix(var.burn)
  
  a<-var.mat[,1]
  b<-var.mat[,2]
  c <- var.mat[,3]
  d <- var.mat[,4]
  ycred <- matrix(0,nrow=10000,ncol=length(xpred))
  half.vals <- rep(NA,10000)
  
  for(g in 1:10000){
    Ey <- (c[g]/(1 + exp(a[g]+b[g]*xpred))+d[g])
    ycred[g,] <- Ey
    half.vals[g] <- -a[g]/b[g]
  }
  ci.MODIS <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  trans.dates[i,4:6] <- quantile(half.vals,c(0.025,0.5, 0.975), na.rm= TRUE)
  
  #PC:
  data.PC = PC_data(URL)
  inputFileName <- paste(siteName,"_PC_varBurn.RData",sep="")
  load(inputFileName)
  burn.sum <- summary(var.burn)
  
  var.PC.a <- burn.sum$statistics[1,1]
  var.PC.b <- burn.sum$statistics[2,1]
  var.PC.c <- burn.sum$statistics[3,1]
  var.PC.d <- burn.sum$statistics[4,1]
  
  a_stat <-paste(as.character(round(burn.sum$statistics[1,1],digits=4)),"+-",as.character(round(burn.sum$statistics[1,2],digits=4)))
  b_stat <-paste(as.character(round(burn.sum$statistics[2,1],digits=4)),"+-",as.character(round(burn.sum$statistics[2,2],digits=4)))
  c_stat <-paste(as.character(round(burn.sum$statistics[3,1],digits=4)),"+-",as.character(round(burn.sum$statistics[3,2],digits=4)))
  d_stat <-paste(as.character(round(burn.sum$statistics[4,1],digits=4)),"+-",as.character(round(burn.sum$statistics[4,2],digits=4)))
  results.table <- rbind(results.table,c(siteName,"PC",a_stat,b_stat,c_stat,d_stat))
  
  var.PC.pred <- pheno.logistic(var.PC.a,var.PC.b,var.PC.c,var.PC.d,xseq=xpred)
  var.mat<-as.matrix(var.burn)
  
  a<-var.mat[,1]
  b<-var.mat[,2]
  c <- var.mat[,3]
  d <- var.mat[,4]

  ycred <- matrix(0,nrow=10000,ncol=length(xpred))
  half.vals <- rep(NA,10000)
  
  for(g in 1:10000){
    Ey <- (c[g]/(1 + exp(a[g]+b[g]*xpred))+d[g])
    ycred[g,] <- Ey
    half.vals[g] <- -a[g]/b[g]
  }
  ci.PC <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  trans.dates[i,7:9] <- quantile(half.vals,probs=c(0.025,0.5, 0.975), na.rm= TRUE)
  par(mfrow=c(1,1))
  plot(x=list(),y=list(),xlim=c(164,420),ylim=c(-0.2,1.2),ylab="Value",xlab="Day of Year",main=siteName,cex.axis=2,cex.lab=2,cex.main=2)
  lines(xpred,rescale(var.GOES.c,var.GOES.d,var.GOES.pred),col="black",lwd=2)
  lines(xpred,rescale(var.GOES.c,var.GOES.d,ci.GOES[1,]), col="black", lty = 2,lwd=2)
  lines(xpred,rescale(var.GOES.c,var.GOES.d,ci.GOES[3,]),col="black",lty=2,lwd=2)
  abline(v=trans.dates[i,2],col="black")
  #points(data.GOES$x,rescale(var.GOES.c,var.GOES.d,data.GOES$y),col="Black",pch=20)
  lines(xpred,rescale(var.PC.c,var.PC.d,var.PC.pred),col="cyan",lwd=2)
  lines(xpred,rescale(var.PC.c,var.PC.d,ci.PC[1,]), col="cyan", lty = 2,lwd=2)
  lines(xpred,rescale(var.PC.c,var.PC.d,ci.PC[3,]),col="cyan",lty=2,lwd=2)
  abline(v=trans.dates[i,8],col="cyan")
  #points(data.PC$x,rescale(var.PC.c,var.PC.d,data.PC$y),col="cyan",pch=20)
  lines(xpred,rescale(var.MODIS.c,var.MODIS.d,var.MODIS.pred),col="red",lwd=2)
  lines(xpred,rescale(var.MODIS.c,var.MODIS.d,ci.MODIS[1,]), col="red", lty = 2,lwd=2)
  lines(xpred,rescale(var.MODIS.c,var.MODIS.d,ci.MODIS[3,]),col="red",lty=2,lwd=2)
  abline(v=trans.dates[i,5],col="red")
  #points(data.MODIS$x,rescale(var.MODIS.c,var.MODIS.d,data.MODIS$y),col="red",pch=20)
  
}
par(mfrow=c(1,3),pty="s")
plot(trans.dates[,2],trans.dates[,8],ylab="PhenoCam Date",xlab="GOES Date",xlim=c(250,350),ylim=c(250,350),asp=1,pch=20,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
abline(a=0,b=1,col="black")
for(i in 1:nrow(trans.dates)){
  lines(x=c(trans.dates[i,2],trans.dates[i,2]),y=c(trans.dates[i,7],trans.dates[i,9]),col="red")
  lines(y=c(trans.dates[i,8],trans.dates[i,8]),x=c(trans.dates[i,1],trans.dates[i,3]),col="red")
}

plot(trans.dates[,2],trans.dates[,5],ylab="MODIS Date",xlab="GOES Date",xlim=c(250,350),ylim=c(250,350),asp=1,pch=20,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
abline(a=0,b=1,col="black")
for(i in 1:nrow(trans.dates)){
  lines(x=c(trans.dates[i,2],trans.dates[i,2]),y=c(trans.dates[i,4],trans.dates[i,6]),col="red")
  lines(y=c(trans.dates[i,5],trans.dates[i,5]),x=c(trans.dates[i,1],trans.dates[i,3]),col="red")
}

plot(trans.dates[,8],trans.dates[,5],ylab="MODIS Date",xlab="PhenoCam Date",xlim=c(250,350),ylim=c(250,350),asp=1,pch=20,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
abline(a=0,b=1,col="black")
for(i in 1:nrow(trans.dates)){
  lines(x=c(trans.dates[i,8],trans.dates[i,8]),y=c(trans.dates[i,4],trans.dates[i,6]),col="red")
  lines(y=c(trans.dates[i,5],trans.dates[i,5]),x=c(trans.dates[i,7],trans.dates[i,9]),col="red")
}
write.csv(results.table[2:19,],file="DB_BayesFits_results.csv")
results.table[2:19,]

dev.off()


AAAA_results <- d(matrix(nrow=18,ncol=10))
AAAA_results[,1:2] <- results.table[2:19,1:2]
results.table2 <- results.table[2:19,]
for(i in 1:18){
  AAAA_results[i,3] <- as.numeric(substr(results.table2[i,3],1,8))
  AAAA_results[i,4] <- as.numeric(substr(results.table2[i,3],13,20))
  
  if(i==2){
    AAAA_results[i,5] <- as.numeric(substr(results.table2[i,4],1,3))
    AAAA_results[i,6] <- as.numeric(substr(results.table2[2,4],8,13))
  }
  else{
  AAAA_results[i,5] <- as.numeric(substr(results.table2[i,4],1,6))
  AAAA_results[i,6] <- as.numeric(substr(results.table2[i,4],11,16))
  }
  AAAA_results[i,7] <- as.numeric(substr(results.table2[i,5],1,6))
  AAAA_results[i,8] <- as.numeric(substr(results.table2[i,4],11,16))
  
  AAAA_results[i,9] <- as.numeric(substr(results.table2[i,6],1,6))
  AAAA_results[i,10] <- as.numeric(substr(results.table2[i,4],11,16))
}
colnames(AAAA_results) <- c("Site","DataSource","A","A_SE","B","B_SE","C","C_SE","D","D_SE")

dodge <- position_dodge(width = 0.9)
p <- ggplot(data=AAAA_results[,1:4],aes(x=Site,y=A),fill=DataSource)
p <- p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(AAAA_results$A_SE, position = dodge, width = 0.25) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank())

limits <- aes(ymax = (AAAA_results[,3] + AAAA_results[,4]),
              ymin = (AAAA_results[,3] - AAAA_results[,4]))

p <- ggplot(data = AAAA_results[,1:4], aes(x = Site, y = A,
                               fill = factor(DataSource)))

p <- p + geom_bar(stat = "identity",
             position = position_dodge(0.9))


p <- p + geom_errorbar(limits, position = position_dodge(0.9),width = 0.25)
p <-  p +  labs(x = "Site", y = "Value") + scale_fill_discrete(name = "Data Source")
p
#######
limits <- aes(ymax = (AAAA_results[,5] + as.numeric(AAAA_results[,6])),
              ymin = (AAAA_results[,5] - as.numeric(AAAA_results[,6])))

p <- ggplot(data = AAAA_results[,1:6], aes(x = Site, y = B,
                                           fill = factor(DataSource)))

p <- p + geom_bar(stat = "identity",
                  position = position_dodge(0.9))


p <- p + geom_errorbar(limits, position = position_dodge(0.9),width = 0.25)
p <-  p +  labs(x = "Site", y = "Value") + scale_fill_discrete(name = "Data Source")
p

#######
limits <- aes(ymax = (AAAA_results[,7] + as.numeric(AAAA_results[,8])),
              ymin = (AAAA_results[,7] - as.numeric(AAAA_results[,8])))

p <- ggplot(data = AAAA_results[,1:8], aes(x = Site, y = C,
                                           fill = factor(DataSource)))

p <- p + geom_bar(stat = "identity",
                  position = position_dodge(0.9))


p <- p + geom_errorbar(limits, position = position_dodge(0.9),width = 0.25)
p <-  p +  labs(x = "Site", y = "Value") + scale_fill_discrete(name = "Data Source")
p

#######
limits <- aes(ymax = (AAAA_results[,9] + as.numeric(AAAA_results[,10])),
              ymin = (AAAA_results[,9] - as.numeric(AAAA_results[,10])))

p <- ggplot(data = AAAA_results[,1:10], aes(x = Site, y = D,
                                           fill = factor(DataSource)))

p <- p + geom_bar(stat = "identity",
                  position = position_dodge(0.9))


p <- p + geom_errorbar(limits, position = position_dodge(0.9),width = 0.25)
p <-  p +  labs(x = "Site", y = "Value") + scale_fill_discrete(name = "Data Source")
p


