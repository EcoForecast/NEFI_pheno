#' Determines the index in the data matrix for the site
#'
#' @param vals The y,x coordinates on the ABI grid (note: the order is y,x not x,y to be consistent with the ABI grid)
#' @param ch The channel number (2 indicating the red band, 3 indicating the near infrared band or NDVI, and ACM indicating the cloud mask)
#' @param orbitVersion NEW or OLD depending on the position of the satellite (only days before 17 November 2017 are OLD). Default value is NEW.
#' @return Vector of the i,j index for the product matrices
#' @source Based on PUG_L1b-vol3 pg 15
getDataIndex <- function(vals,ch,orbitVersion=NEW){
  if(orbitVersion=="OLD"){
    if(ch==3){
      x.scale_factor <- 2.80000003840541e-05 #values from netcdf files under dimensions:x:scale_factor
      x.add_offset <- -0.0750259980559349 #All add_offset values changed when they moved the satellite
      y.scale_factor <- -2.80000003840541e-05
      y.add_offset <- 0.126545995473862
    }
    if(ch==2){
      x.scale_factor <- 1.40000001920271e-05
      x.add_offset <- -0.075033001601696
      y.scale_factor <- -1.40000001920271e-05
      y.add_offset <- 0.126552999019623
    }
    if(ch=="ACM"){
      x.scale_factor <- 5.60000007681083e-05
      x.add_offset <- -0.0750119984149933
      y.scale_factor <- -5.60000007681083e-05
      y.add_offset <- 0.126532003283501

    }
  }
  else{
    if(ch==3){
      x.scale_factor <- 2.80000003840541e-05 #values from netcdf files under dimensions:x:scale_factor
      x.add_offset <- -0.101346001029015
      y.scale_factor <- -2.80000003840541e-05
      y.add_offset <- 0.12822599709034
    }
    if(ch==2){
      x.scale_factor <- 1.40000001920271e-05
      x.add_offset <- -0.101352997124195
      y.scale_factor <- -1.40000001920271e-05
      y.add_offset <- 0.128233000636101
    }
    if(ch=="ACM"){
      x.scale_factor <- 5.60000007681083e-05
      x.add_offset <- -0.101332001388073
      y.scale_factor <- -5.60000007681083e-05
      y.add_offset <- 0.128212004899979
    }
  }
  i <- (vals[2]-x.add_offset)/x.scale_factor
  j <- (vals[1]-y.add_offset)/y.scale_factor
  return(c(i,j))
}
