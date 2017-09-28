#Install packages needed to map
#install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
#install.packages(c("maps", "mapdata"))

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(nneo)

neon_sites <- nneo_sites()

dta <-  map_data("world",c("usa","hawaii","alaska"),xlim=c(-180,-65), ylim=c(19,72))

plot.new()
gg1 <- ggplot() + geom_polygon(data = dta, aes(x=long, y=lat, group=group),fill=NA,colour="black") + coord_fixed(1.3)
#gg1 <- gg1 + geom_point(data=neon_sites, aes(x = siteLongitude, y = siteLatitude)) #Can uncomment for all sites with data

productName <-  "Precipitation" ####Change for the desired product
neon_sites$containsProduct <- NA
neon_sites$productLength <- NA
for (i in 1:(dim(neon_sites)[1])){
  availableProducts = neon_sites[i,]$dataProducts[[1]]$dataProductTitle
  availableDates = list()
  containsProductBool <-  FALSE
  if(length(availableProducts>0)){
  for (j in 1:length(availableProducts)){
    if (availableProducts[j]== productName){
      containsProductBool <-  TRUE
      availableDates <- c(availableDates,neon_sites[i,]$dataProducts[[1]]$availableMonths[j])
    }
  }
  }
  if(length(availableDates)>0){
    neon_sites$productLength[i] <- length(availableDates[[1]])
  }
  else{
    neon_sites$productLength[i] <- 0
  }
  neon_sites[i,]$containsProduct <- containsProductBool

}
availableSites <- subset(neon_sites,containsProduct)

clrs <- c("cadetblue","darkmagenta","darkolivegreen1","firebrick","hotpink","darkorchid","darkorange","darkseagreen")

for(i in 1:8){
  gg1 <- gg1 + geom_point(data=subset(availableSites,(availableSites$productLength<=(i*5) & availableSites$productLength>(i*5-5))), aes(x = siteLongitude, y = siteLatitude),colour=clrs[i])
}

gg1 <-gg1 + geom_point(data=subset(availableSites,(availableSites$productLength>40)), aes(x = siteLongitude, y = siteLatitude),colour="darkslategreen")

#gg1 <- gg1 + geom_point(data=availableSites, aes(x = siteLongitude, y = siteLatitude),colour="green") #will plot all sites that have data for the desired product

gg1