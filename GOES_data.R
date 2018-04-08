##' For GOES NDVI data, construct the data object for input into MCMC
##' 
##' @param site.name Site Name
##' @param season spring or autumn
GOES_data <- function(site.name,season) {
  ##Data
  
  NDVI.fileName <- paste("GOES_NDVI_",site.name,"2017.csv",sep="")
  GOES <- read.csv(NDVI.fileName,header=FALSE) 
  #GOES <- t(na.omit(t(GOES)))
  
  GOES_Days <- as.numeric(GOES[1,])
  GOES_NDVI <- as.numeric(GOES[2,])
  
  sep.val <- min(which(GOES_Days>190)) 
  #print(GOES_Days[sep.val])
  #print(GOES_NDVI[sep.val])
  if(length(GOES_Days)<200){
    print("Error: Missing Spring")
  }
  if(season=="spring"){
    y <- GOES_NDVI[1:sep.val]
    x <- GOES_Days[1:sep.val]
  }
  else if(season=="autumn"){
    y <- GOES_NDVI[sep.val:length(GOES_Days)]
    x <- GOES_Days[sep.val:length(GOES_Days)]
  }
  
  data <- list(x=x,y=y,n=length(y))
  ##Specify Priors
  data$beta.c <- 5
  data$alpha.c <- 3
  data$alpha.d <- 4
  data$beta.d <- 5
  data$s1 <- 0.5
  data$s2 <- 0.2
  data$v.a <- 3
  data$v.b <- 2
  if(season=="spring"){
    data$mean.a <- 30
    data$mean.b <- -0.5
  }
  else if(season=="autumn"){
    data$mean.a <- -30
    data$mean.b <- 0.5
  }
  return(data)
}