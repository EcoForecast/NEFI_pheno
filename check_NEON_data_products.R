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

gg1 <- ggplot() + geom_polygon(data = dta, aes(x=long, y=lat, group=group),fill=NA,colour="black") + coord_fixed(1.3)
gg1 <- gg1 + geom_point(data=neon_sites, aes(x = siteLongitude, y = siteLatitude))

productName <-  "Tick-borne pathogen status" 
neon_sites$containsProduct <- NA
for (i in 1:(dim(neon_sites)[1])){
  availableProducts = neon_sites[i,]$dataProducts[[1]]$dataProductTitle
  containsProductBool <-  FALSE
  if(length(availableProducts>0)){
  for (j in 1:length(availableProducts)){
    if (availableProducts[j]== productName){
      containsProductBool <-  TRUE
    }
  }
  }
  neon_sites[i,]$containsProduct <- containsProductBool

}
availableSites <- subset(neon_sites,containsProduct)

gg1 <- gg1 + geom_point(data=availableSites, aes(x = siteLongitude, y = siteLatitude),colour="green")
gg1

