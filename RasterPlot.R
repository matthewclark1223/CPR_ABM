library(rgdal)
library(raster)
Resources<-10000
Users<-100
StartingIntegrity<-0.75
PercProtec<-0.3
Regrowth<-1.5
HarvestLim<-3
Timestps<-25

#############

simgrid <- expand.grid(1:sqrt(Resources), 1:sqrt(Resources))
n <- nrow(simgrid)

NumProtec<-Resources*PercProtec
Protected <-c(rep(2,NumProtec),rep(0,Resources-NumProtec))

#show degredation
Resrc<-rbinom(Resources,1,StartingIntegrity)

# Visualize results

DegRaster <- rasterFromXYZ(cbind(simgrid[, 1:2] - 0.5, Resrc))

plot(DegRaster)

#create protected area raster
NumProtec<-Resources*PercProtec
Protected <-c(rep(2,NumProtec),rep(1,Resources-NumProtec)) #Dummy values for protected vs not
ProtDat<-cbind(simgrid[, 1:2] - 0.5,Protected)
ProtRaster<-rasterFromXYZ(ProtDat)#make the categorical raster
plot(ProtRaster,
     legend = FALSE,
     col = c("blue", "green"), axes = FALSE,
     main = "words")

legend("topright",
       legend = c("Not Protected", "Protected"),
       fill = c( "blue", "green"),
       border = FALSE,
       bty = "n") # turn off legend border

#Make the raster stack

dat<-stack(DegRaster,ProtRaster)
plot(dat)
