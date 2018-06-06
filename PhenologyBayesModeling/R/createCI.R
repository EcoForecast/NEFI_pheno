createCI <- function(PFT,var.mat,xseq){
  ycred <- matrix(0,nrow=10000,ncol=length(xseq))
  if(PFT=="DB"){
    a<-var.mat[,1]
    b<-var.mat[,2]
    c <- var.mat[,3]
    d <- var.mat[,4]
    for(g in 1:10000){
      Ey <- rescale(c=c[g],d=d[g],yseq=pheno.logistic(a=a[g],b=b[g],c=c[g],d=d[g],xseq=xseq))
      ycred[g,] <- Ey
    }
  }
  else if(PFT=="SH"){
    a<-var.mat[,1]
    b<-var.mat[,2]
    c <- var.mat[,3]
    d <- var.mat[,4]
    k <- var.mat[,5]
    r <- var.mat[,7]

    for(g in 1:10000){
      Ey <- rescale(c=c[g],d=d[g],yseq=shrublandYvals(a=a[g],b=b[g],c=c[g],d=d[g],k=k[g],r=r[g],xseq=xseq))
      ycred[g,] <- Ey
    }
  }
  return(apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE))
}


