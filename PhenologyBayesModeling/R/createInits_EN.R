createInits_EN <- function(data){
  x <- data$x
  y <- data$y
  a <- max(y,na.rm = TRUE)
  b <- x[which(y==max(y,na.rm = TRUE))]
  d <- mean(sort(y)[1:5])
  init.mus <- list(k=k,d=d,c=c)
  return(init.mus)
}
