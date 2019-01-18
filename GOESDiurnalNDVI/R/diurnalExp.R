#' Calculates the y value sequence for the double diurnal curve
#'
#' @param a One value for the a (shape) parameter
#' @param c One value for the c (midday NDVI) parameter
#' @param k One value for the k (changepoint) parameter
#' @param xseq Vector of x values to calculate y values for
#' @export
diurnalExp <- function(a,c,k,xseq){
  k <- round(k,digits=3)
  #print(k)
  bk <- which(round(xseq,digits=3)==k)
  #print(bk)
  left <- -a*exp(-1*(xseq[1:bk]-k))+c
  right.xseq <- xseq[(bk+1):length(xseq)]
  right <- -a*exp((right.xseq-k))+c
  #print(length(c(left,right)))
  return(c(left,right))
}


