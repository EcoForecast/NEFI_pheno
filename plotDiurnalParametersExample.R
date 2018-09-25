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
plot(h,freq=FALSE,main="Posterior Density of a",xlab="",ylab="",cex.lab=2)
h = hist(c,plot=FALSE)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE,main="Posterior Density of c",xlab="",ylab="",cex.lab=2)
h = hist(k,plot=FALSE)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE,main="Posterior Density of k",xlab="",ylab="",cex.lab=2)
h = hist(prec,plot=FALSE)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE,main="Posterior Density of prec",xlab="",ylab="",cex.lab=2)
h = hist(alp,plot=FALSE)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE,main="Posterior Density of alp",xlab="",ylab="",cex.lab=2)
h = hist(bet,plot=FALSE)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE,main="Posterior Density of bet",xlab="",ylab="",cex.lab=2)
h = hist(p.cloud,plot=FALSE)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE,main="Posterior Density of p.cloud",xlab="",ylab="",cex.lab=2)

dev.off()