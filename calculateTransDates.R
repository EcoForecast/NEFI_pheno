siteData <- read.csv("GE509_Project_Sites.csv",header=FALSE)
noSites <- 4
trans.dates <- matrix(nrow=noSites,ncol=9)
colnames(trans.dates) <- c("GOES 0.025","GOES 0.5","GOES 0.975","PC 0.025","PC 0.5","PC 0.975","MODIS 0.025","MODIS 0.5","MODIS 0.975")
nrows <- 1
iseq <- c(1,3,4,7)
for (q in 1:length(iseq)){
  print(q)
  i <- iseq[q]
  siteName <- as.character(siteData[i,1])

  #GOES
  inputFileName <- paste(siteName,"_GOES_varBurn.RData",sep="")
  load(inputFileName)
  burn.sum <- summary(var.burn)
  
  var.mat<-as.matrix(var.burn)
  
  a<-var.mat[,1]
  b<-var.mat[,2]
  c <- var.mat[,3]
  d <- var.mat[,4]
  half.vals <- rep(NA,10000)
  
  for(g in 1:10000){
    half.vals[g] <- -a[g]/b[g]
  }
  ci <- quantile(half.vals,c(0.025,0.5, 0.975), na.rm= TRUE)
  trans.dates[q,1:3] <- ci
  
  #PhenoCam
  inputFileName <- paste(siteName,"_PC_varBurn.RData",sep="")
  load(inputFileName)
  burn.sum <- summary(var.burn)
  
  var.mat<-as.matrix(var.burn)
  
  a<-var.mat[,1]
  b<-var.mat[,2]
  c <- var.mat[,3]
  d <- var.mat[,4]
  half.vals <- rep(NA,10000)
  
  for(g in 1:10000){
    half.vals[g] <- -a[g]/b[g]
  }
  ci <- quantile(half.vals,c(0.025,0.5, 0.975), na.rm= TRUE)
  trans.dates[q,4:6] <- ci
  #MODIS
  inputFileName <- paste(siteName,"_MODIS_varBurn.RData",sep="")
  load(inputFileName)
  burn.sum <- summary(var.burn)
  
  var.mat<-as.matrix(var.burn)
  
  a<-var.mat[,1]
  b<-var.mat[,2]
  c <- var.mat[,3]
  d <- var.mat[,4]
  half.vals <- rep(NA,10000)
  
  for(g in 1:10000){
    half.vals[g] <- -a[g]/b[g]
  }
  ci <- quantile(half.vals,probs=c(0.025,0.5, 0.975), na.rm= TRUE)
  trans.dates[q,7:9] <- ci
}

plot(trans.dates[,2],trans.dates[,5],ylab="PhenoCam Date",xlab="GOES Date",main="Date of 50% Leaf Off",xlim=c(240,320),ylim=c(240,320))
abline(a=0,b=1,col="red")
for(i in 1:4){
  lines(x=c(trans.dates[i,2],trans.dates[i,2]),y=c(trans.dates[i,4],trans.dates[i,6]))
  lines(y=c(trans.dates[i,5],trans.dates[i,5]),x=c(trans.dates[i,1],trans.dates[i,3]))
}

plot(trans.dates[,2],trans.dates[,8],ylab="MODIS Date",xlab="GOES Date",main="Date of 50% Leaf Off",xlim=c(240,320),ylim=c(240,320))
abline(a=0,b=1,col="red")
for(i in 1:4){
  lines(x=c(trans.dates[i,2],trans.dates[i,2]),y=c(trans.dates[i,7],trans.dates[i,9]))
  lines(y=c(trans.dates[i,8],trans.dates[i,8]),x=c(trans.dates[i,1],trans.dates[i,3]))
}

plot(trans.dates[,5],trans.dates[,8],ylab="MODIS Date",xlab="PhenoCam Date",main="Date of 50% Leaf Off",xlim=c(240,320),ylim=c(240,320))
abline(a=0,b=1,col="red")
for(i in 1:4){
  lines(x=c(trans.dates[i,5],trans.dates[i,5]),y=c(trans.dates[i,7],trans.dates[i,9]))
  lines(y=c(trans.dates[i,8],trans.dates[i,8]),x=c(trans.dates[i,4],trans.dates[i,6]))
}


  
  
  