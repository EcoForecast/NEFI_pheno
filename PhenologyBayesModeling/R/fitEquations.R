
pheno.logistic <- function(Tran,b,c,d,xseq){
  return(c/(1+exp(b*(xseq-Tran)))+d)
}

rescale <- function(c,d,yseq){
  return((yseq-c-d)/c+1)
}

shrublandYvals <- function(Tran,b,c,d,k,r,xseq){
  bk <- which(xseq==round(k,digits=0))
  if(k<110){
    bk <- 1
  }
  greenup <- pheno.logistic(Tran=Tran,b=b,c=c,d=d,xseq[1:bk])
  gd.xseq <- xseq[(bk+1):length(xseq)]-k
  greendown <- c*exp(r*gd.xseq)+d
  return(c(greenup,greendown))
}

deciduousYvals <- function(TranF,bF,TranS,bS,c,d,k,xseq){
  bk <- which(xseq==round(k,digits=0))
  greendown <- pheno.logistic(Tran=TranF,b=bF,c=c,d=d,xseq[1:bk])
  gu.xseq <- xseq[(bk+1):length(xseq)]
  greenup <- pheno.logistic(Tran=TranS,b=bS,c=c,d=d,gu.xseq)
  return(c(greendown,greenup))
}

gaussianCurve <- function(a,b,c,d,xseq){
  return(a*exp((-1*(xseq-b)**2)/(2*c**2))+d)
}
