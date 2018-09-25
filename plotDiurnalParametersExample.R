library("ecoforecastR")
library("rjags")

diurnalExp <- function(a,c,k,xseq){
  k <- round(k,digits=1)
  #print(k)
  bk <- which(round(xseq,digits=1)==k)
  #print(bk)
  left <- -a*exp(-1*(xseq[1:bk]-k))+c
  right.xseq <- xseq[(bk+1):length(xseq)]
  right <- -a*exp((right.xseq-k))+c
  #print(length(c(left,right)))
  return(c(left,right))
}

siteName <- "russellSage"
xseq <- seq(0,25,0.1)

outputFileName <- paste(siteName,"_diurnalPosteriorExampleALL.pdf",sep="")
pdf(file=outputFileName,width=10,height=5)
par(mfrow=c(2,4),mai=c(0.4,0.4,0.2,0.2))

#fileName <- "GOES_Diurnal_russellSage_2017186.csv"

load("russellSage_231_varBurn_ALLParamaters.RData")
out.mat <- as.matrix(var.burn)
colnames(out.mat)
a <- out.mat[,1]
alp <- out.mat[,2]
bet <- out.mat[,3]
c <- out.mat[,4]
k <- out.mat[,5]
p.cloud <- out.mat[,6]
prec <- out.mat[,7]

h = hist(a,plot=FALSE)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE,main="Posterior Density of a",xlab="",ylab="",cex.axis=1.5,cex.main=1.5)
qnts <- quantile(a,c(0.025,0.975))
polygon(c(qnts[1],qnts[1],qnts[2],qnts[2]),c(0,1,1,0),border=NA,col=rgb(0,0,1,0.3))
abline(v=mean(a),col="red",lwd=2)

h = hist(c,plot=FALSE)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE,main="Posterior Density of c",xlab="",ylab="",cex.axis=1.5,cex.main=1.5)
qnts <- quantile(c,c(0.025,0.975))
polygon(c(qnts[1],qnts[1],qnts[2],qnts[2]),c(0,1,1,0),border=NA,col=rgb(0,0,1,0.3))
abline(v=mean(c),col="red",lwd=2)

h = hist(k,plot=FALSE)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE,main="Posterior Density of k",xlab="",ylab="",cex.axis=1.5,cex.main=1.5)
qnts <- quantile(k,c(0.025,0.975))
polygon(c(qnts[1],qnts[1],qnts[2],qnts[2]),c(0,1,1,0),border=NA,col=rgb(0,0,1,0.3))
abline(v=mean(k),col="red",lwd=2)

h = hist(prec,plot=FALSE)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE,main="Posterior Density of prec",xlab="",ylab="",cex.axis=1.5,cex.main=1.5)
qnts <- quantile(prec,c(0.025,0.975))
polygon(c(qnts[1],qnts[1],qnts[2],qnts[2]),c(0,1,1,0),border=NA,col=rgb(0,0,1,0.3))
abline(v=mean(prec),col="red",lwd=2)

h = hist(alp,plot=FALSE)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE,main="Posterior Density of alp",xlab="",ylab="",cex.axis=1.5,cex.main=1.5)
qnts <- quantile(alp,c(0.025,0.975))
polygon(c(qnts[1],qnts[1],qnts[2],qnts[2]),c(0,1,1,0),border=NA,col=rgb(0,0,1,0.3))
abline(v=mean(alp),col="red",lwd=2)

h = hist(bet,plot=FALSE)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE,main="Posterior Density of bet",xlab="",ylab="",cex.axis=1.5,cex.main=1.5)
qnts <- quantile(bet,c(0.025,0.975))
polygon(c(qnts[1],qnts[1],qnts[2],qnts[2]),c(0,1,1,0),border=NA,col=rgb(0,0,1,0.3))
abline(v=mean(bet),col="red",lwd=2)

h = hist(p.cloud,plot=FALSE)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE,main="Posterior Density of p.cloud",xlab="",ylab="",cex.axis=1.5,cex.main=1.5)
qnts <- quantile(p.cloud,c(0.025,0.975))
polygon(c(qnts[1],qnts[1],qnts[2],qnts[2]),c(0,1,1,0),border=NA,col=rgb(0,0,1,0.3))
abline(v=mean(p.cloud),col="red",lwd=2)

dev.off()