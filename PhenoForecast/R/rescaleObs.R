#' Function to rescale phenology measurements to percent canopy cover for that calendar year
#'
#' @param time A vector of days (x-values)
#' @param vals A vector of the measurements corresponding for the days (y-values)
#' @export
rescaleObs <- function(time,vals){
  new.ys <- numeric()
  for(yr in ((lubridate::year(range(time)[1])+1):2018)){
    year.vals <- vals[(lubridate::year(time)==yr)] ##The observations for each year
    year.max <- max(year.vals,na.rm=TRUE) ##Max for that year
    year.min <- min(year.vals,na.rm=TRUE)  ##Min for that year
    year.vals.new <- (year.vals-year.max)/(year.max-year.min)+1 ##Rescale so the range is between (0,1)
    new.ys <- c(new.ys,year.vals.new)
  }
  ##Rescale partial start year
  yr <- lubridate::year(range(time)[1])
  year.vals <- vals[(lubridate::year(time)==yr)]
  next.year.vals <- vals[(lubridate::year(time)==(yr+1))]
  year.max <- max(next.year.vals,na.rm=TRUE)
  year.min <- min(next.year.vals,na.rm=TRUE)
  year.vals.new <- (year.vals-year.max)/(year.max-year.min)+1
  new.ys <- c(year.vals.new,new.ys)

  ##Rescale partial current year
  yr <- 2019
  year.vals <- vals[(lubridate::year(time)==yr)]
  past.year.vals <- vals[(lubridate::year(time)==(yr-1))]
  year.max <- max(past.year.vals,na.rm=TRUE)
  year.min <- min(past.year.vals,na.rm=TRUE)
  year.vals.new <- (year.vals-year.max)/(year.max-year.min)+1
  new.ys <- c(new.ys,year.vals.new)
  return(new.ys)
}
