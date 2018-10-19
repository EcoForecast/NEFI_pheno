#' Calculates NDVI (normalized difference vegetation index)
#'
#' @param R2 The reflectance factor for the red band (channel 2)
#' @param R3 The reflectance factor for the near infrared band (channel 3)
calNDVI <- function(R2,R3){
  return((R3-R2)/(R3+R2))
}
