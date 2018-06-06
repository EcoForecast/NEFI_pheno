library(rjags)
library(coda)
source("PC_data.R")
source("createModel_SH.R")
source("MODIS_data.R")
source("GOES_data.R")
library("MODISTools")
library("numDeriv")

rescale <- function(c,d,yseq){
  return((yseq-c-d)/c+1)
}

########################################################################

pdf(file="SH_Bayes_Fits.pdf",width=8,height=8)


iseq <- c(8,16,19,20,21)

siteData <- read.csv("GOES_Paper_Sites.csv",header=FALSE)
xpred <- seq(110,424,1)

for (i in iseq){
  i <- 8
  siteName <- as.character(siteData[i,1])
  print(siteName)
  #Lat <- as.character(siteData[i,2])
  #Long <-as.character(siteData[i,3])
  #URL <- as.character(siteData[i,4])
  
  #GOES
  # data.GOES = GOES_data(site.name,startDay = 110,endDay = 424)
  # inputFileName <- paste(siteName,"_GOES_varBurn.RData",sep="")
  # load(inputFileName)
  # burn.sum <- summary(var.burn)
  # 
  # GOES.a <- burn.sum$statistics[1,1]
  # GOES.b <- burn.sum$statistics[2,1]
  # GOES.c <- burn.sum$statistics[3,1]
  # GOES.d <- burn.sum$statistics[4,1]
  # GOES.k <- burn.sum$statistics[5,1]
  # GOES.r <- burn.sum$statistics[7,1]
  # 
  # 
  # xpred <- seq(110,424)
  # var.GOES.pred <- shrublandYvals(a=GOES.a,b=GOES.b,c=GOES.c,d=GOES.d,k=GOES.k,r=GOES.r,xseq=xpred)
  # var.mat<-as.matrix(var.burn)
  # 
  # a<-var.mat[,1]
  # b<-var.mat[,2]
  # c <- var.mat[,3]
  # d <- var.mat[,4]
  # k <- var.mat[,5]
  # r <- var.mat[,7]
  # 
  # ycred <- matrix(0,nrow=10000,ncol=length(xpred))
  # #half.vals <- rep(NA,10000)
  # 
  # for(g in 1:10000){
  #   Ey <- shrublandYvals(a=a[g],b=b[g],c=c[g],d=d[g],k=k[g],r=r[g],xseq=xpred)
  #   ycred[g,] <- Ey
  # 
  # }
  # ci <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  # ci.GOES <- ci
  # ci.GOES.k <- quantile(k,c(0.025,0.5, 0.975), na.rm= TRUE)
  # #trans.dates[i,1:3] <- quantile(half.vals,c(0.025,0.5, 0.975), na.rm= TRUE)
  # 
  #MODIS:
  data.MODIS = MODIS_data(data.source,site.name=siteName)
  inputFileName <- paste(siteName,"_MODIS_varBurn.RData",sep="")
  load(inputFileName)
  
  burn.sum <- summary(var.burn)
  
  MODIS.a <- burn.sum$statistics[1,1]
  MODIS.b <- burn.sum$statistics[2,1]
  MODIS.c <- burn.sum$statistics[3,1]
  MODIS.d <- burn.sum$statistics[4,1]
  MODIS.k <- burn.sum$statistics[5,1]
  MODIS.r <- burn.sum$statistics[7,1]
  
  var.MODIS.pred <- shrublandYvals(a=MODIS.a,b=MODIS.b,c=MODIS.c,d=MODIS.d,k=MODIS.k,r=MODIS.r,xseq=xpred)
  
  var.mat<-as.matrix(var.burn)
  
  a<-var.mat[,1]
  b<-var.mat[,2]
  c <- var.mat[,3]
  d <- var.mat[,4]
  k <- var.mat[,5]
  r <- var.mat[,7]
  ycred <- matrix(0,nrow=10000,ncol=length(xpred))
  
  for(g in 1:10000){
    Ey <- shrublandYvals(a=a[g],b=b[g],c=c[g],d=d[g],k=k[g],r=r[g],xseq=xpred)
    ycred[g,] <- Ey
  }
  ci.MODIS <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  ci.MODIS.k <- quantile(k,c(0.025,0.5, 0.975), na.rm= TRUE)
  
  #PC:
  # data.PC <-  PC_data(URL=URL,startDay = 110,endDay = 424)
  # inputFileName <- paste(siteName,"_PC_varBurn.RData",sep="")
  # load(inputFileName)
  # burn.sum <- summary(var.burn)
  # 
  # PC.a <- burn.sum$statistics[1,1]
  # PC.b <- burn.sum$statistics[2,1]
  # PC.c <- burn.sum$statistics[3,1]
  # PC.d <- burn.sum$statistics[4,1]
  # PC.k <- burn.sum$statistics[5,1]
  # PC.r <- burn.sum$statistics[7,1]
  # 
  # var.PC.pred <- shrublandYvals(a=PC.a,b=PC.b,c=PC.c,d=PC.d,k=PC.k,r=PC.r,xseq=xpred)
  # var.mat<-as.matrix(var.burn)
  # 
  # a<-var.mat[,1]
  # b<-var.mat[,2]
  # c <- var.mat[,3]
  # d <- var.mat[,4]
  # k <- var.mat[,5]
  # r <- var.mat[,7]
  # 
  # ycred <- matrix(0,nrow=10000,ncol=length(xpred))
  # 
  # for(g in 1:10000){
  #   Ey <- shrublandYvals(a=a[g],b=b[g],c=c[g],d=d[g],k=k[g],r=r[g],xseq=xpred)
  #   ycred[g,] <- Ey
  # }
  # ci.PC <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  # ci.PC.k <- quantile(k,c(0.025,0.5, 0.975), na.rm= TRUE)

  #par(mfrow=c(1,1))
  plot(x=list(),y=list(),xlim=c(105,425),ylim=c(-0.2,1.2),ylab="Value",xlab="Day of Year",main=siteName,cex.axis=2,cex.lab=2,cex.main=2)
  #lines(xpred,rescale(GOES.c,GOES.d,var.GOES.pred),col="black",lwd=2)
  #lines(xpred,rescale(GOES.c,GOES.d,ci.GOES[1,]), col="black", lty = 2,lwd=2)
  #lines(xpred,rescale(GOES.c,GOES.d,ci.GOES[3,]),col="black",lty=2,lwd=2)
  #abline(v=GOES.k,col="black")
  #points(data.GOES$x,rescale(var.GOES.c,var.GOES.d,data.GOES$y),col="Black",pch=20)
  #lines(xpred,rescale(PC.c,PC.d,var.PC.pred),col="cyan",lwd=2)
  #lines(xpred,rescale(PC.c,PC.d,ci.PC[1,]), col="cyan", lty = 2,lwd=2)
  #lines(xpred,rescale(PC.c,PC.d,ci.PC[3,]),col="cyan",lty=2,lwd=2)
  #abline(v=PC.k,col="cyan")
  #points(data.PC$x,rescale(var.PC.c,var.PC.d,data.PC$y),col="cyan",pch=20)
  lines(xpred,rescale(MODIS.c,MODIS.d,var.MODIS.pred),col="red",lwd=2)
  lines(xpred,rescale(MODIS.c,MODIS.d,ci.MODIS[1,]), col="red", lty = 2,lwd=2)
  lines(xpred,rescale(MODIS.c,MODIS.d,ci.MODIS[3,]),col="red",lty=2,lwd=2)
  abline(v=MODIS.k,col="red")
  points(data.MODIS$x,rescale(MODIS.c,MODIS.d,data.MODIS$y),col="red",pch=20)
  
}
# par(mfrow=c(1,3),pty="s")
# plot(ci.GOES.k[2],ci.PC.k[2],ylab="PhenoCam Date",xlab="GOES Date",xlim=c(200,250),ylim=c(200,250),asp=1,pch=20,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
# abline(a=0,b=1,col="black")
# lines(x=c(ci.GOES.k[1],ci.GOES.k[3]),y=c(ci.PC.k[2],ci.PC.k[2]),col="red")
# lines(y=c(ci.PC.k[1],ci.PC.k[3]),x=c(ci.GOES.k[2],ci.GOES.k[2]),col="red")
# 
# plot(ci.GOES.k[2],ci.MODIS.k[2],ylab="MODIS Date",xlab="GOES Date",xlim=c(200,250),ylim=c(200,250),asp=1,pch=20,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
# abline(a=0,b=1,col="black")
# lines(x=c(ci.GOES.k[1],ci.GOES.k[3]),y=c(ci.MODIS.k[2],ci.MODIS.k[2]),col="red")
# lines(y=c(ci.MODIS.k[1],ci.MODIS.k[3]),x=c(ci.GOES.k[2],ci.GOES.k[2]),col="red")
# 
# plot(ci.PC.k[2],ci.MODIS.k[2],ylab="MODIS Date",xlab="PhenoCam Date",xlim=c(200,250),ylim=c(200,250),asp=1,pch=20,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
# abline(a=0,b=1,col="black")
# lines(x=c(ci.PC.k[1],ci.PC.k[3]),y=c(ci.MODIS.k[2],ci.MODIS.k[2]),col="red")
# lines(y=c(ci.MODIS.k[1],ci.MODIS.k[3]),x=c(ci.PC.k[2],ci.PC.k[2]),col="red")



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


