
pheno.logistic <- function(a,b,c,d,xseq){
  return(c/(1 + exp(a+b*xseq))+d)
}

rescale <- function(c,d,yseq){
  return((yseq-c-d)/c+1)
}

shrublandYvals <- function(a,b,c,d,k,r,xseq){
  bk <- which(xseq==round(k,digits=0))
  if(k<110){
    bk <- 1
  }
  greenup <- pheno.logistic(a=a,b=b,c=c,d=d,xseq[1:bk])
  gd.xseq <- xseq[(bk+1):length(xseq)]-k
  greendown <- c*exp(r*gd.xseq)+d
  return(c(greenup,greendown))
}
