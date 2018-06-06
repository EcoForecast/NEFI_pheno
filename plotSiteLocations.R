library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(nneo)
library(googlesheets)

dta <-  map_data("world","usa",xlim=c(-100,-65), ylim=c(19,50))

siteData <- data.frame(read.csv("GE509_Project_Sites.csv",header=FALSE))
colnames(siteData) <- c("Name","Latitude","Longitude","URL","Type")

iseq <- c(1,2,3,7,8,9)

gg1 <- ggplot(main="Selected Site Locations") +geom_polygon(data = dta, aes(x=long, y=lat, group=group),fill=NA,colour="black")# + cord_fixed(1.3)
gg1 <- gg1 + geom_point(data=siteData[iseq,], aes(x = Longitude, y = Latitude))+theme_bw()
gg1 <- gg1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
gg1 <- gg1 + labs(x="Longitude",y="Latitude")

labs(x = "New x label")

plot(gg1)
