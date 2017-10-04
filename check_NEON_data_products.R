#install.packages("nneo")
#Install packages needed to map
#install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
#install.packages(c("maps", "mapdata"))
#install.packages("ggmap")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(nneo)

pdf("NEON_Site_Product_Availability.pdf", width=6, height=4)
neon_sites <- nneo_sites()
desiredProducts <- read.csv(file="NEON_Products.csv",head=TRUE)

dta <-  map_data("world",c("usa","hawaii","alaska"),xlim=c(-180,-65), ylim=c(19,72))

for (k in 1:(dim(desiredProducts)[1])){

gg1 <- ggplot(main=desiredProducts$NEON.Product[k]) + geom_polygon(data = dta, aes(x=long, y=lat, group=group),fill=NA,colour="black") + coord_fixed(1.3)
gg1 <- gg1 + geom_point(data=neon_sites, aes(x = siteLongitude, y = siteLatitude),colour="red",show.legend = TRUE) 

productName <-  desiredProducts$NEON.Product[k] 
productID <- paste(substring(desiredProducts$NEON.Product.ID[k],6,14),".001",sep="") #properly format the product ID's
neon_sites$containsProduct <- NA
neon_sites$productLength <- NA
for (i in 1:(dim(neon_sites)[1])){
  availableProducts = neon_sites[i,]$dataProducts[[1]]$dataProductCode
  availableDates = list()
  containsProductBool <-  FALSE
  if(length(availableProducts>0)){
  for (j in 1:length(availableProducts)){
    if (availableProducts[j]== productID){
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

gg1 <- gg1 + geom_point(data=availableSites, aes(x = siteLongitude, y = siteLatitude,color=productLength))

gg1 <- gg1 + ggtitle(productName)+ labs(colour='Number of Months of Observations')
plot(gg1)
}
dev.off()
