createInits <- function(data,PFT){
  x <- data$x
  y <- data$y
  d <- mean(sort(y)[1:10])
  c <- mean(sort(y,decreasing = TRUE)[1:5])-d
  init.mus <- list(d=d,c=c)
  if(PFT=="SH"){
    data$k <- x[which(y==max(y,na.rm = TRUE))]
  }
  return(init.mus)
}


