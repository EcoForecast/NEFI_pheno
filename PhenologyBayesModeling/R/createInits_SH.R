createInits_SH <- function(data){
  x <- data$x
  y <- data$y
  k <- x[which(y==max(y,na.rm = TRUE))]
  d <- mean(sort(y)[1:10])
  c <- mean(sort(y,decreasing = TRUE)[1:5])-d
  init.mus <- list(k=k,d=d,c=c)
  return(init.mus)
}
