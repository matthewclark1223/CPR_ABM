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

#DegRaster <- rasterFromXYZ(cbind(simgrid[, 1:2] - 0.5, Resrc))

#plot(DegRaster)

#create protected area raster
#NumProtec<-Resources*PercProtec
#Protected <-c(rep("Protected",NumProtec),rep("Not Protected",Resources-NumProtec)) #Dummy values for protected vs not
#ProtDat<-cbind(simgrid[, 1:2] - 0.5,Protected)
#ProtRaster<-rasterFromXYZ(ProtDat)#make the categorical raster
#plot(ProtRaster,
     #legend = FALSE,
     #col = c("blue", "green"), axes = FALSE,
     #main = "words")

#legend("topright",
      # legend = c("Not Protected", "Protected"),
      # fill = c( "blue", "green"),
      # border = FALSE,
      # bty = "n") # turn off legend border

#Make the raster stack

#dat<-stack(DegRaster,ProtRaster)
#plot(dat)



##gt paper plot
Resources<-10000
simgrid <- expand.grid(1:sqrt(Resources), 1:sqrt(Resources))
n <- nrow(simgrid)


Protected <-c(rep(2,NumProtec),rep(0,Resources-NumProtec))
PercProtec<-0.0
NumProtec<-Resources*PercProtec
StartingIntegrity<-1
Resrc<-rbinom(Resources,1,StartingIntegrity)
Protected <-c(rep("Protected",NumProtec),rep("Working",Resources-NumProtec)) #Dummy values for protected vs not
ProtDat<-cbind(simgrid[, 1:2] - 0.5,Protected)
x<-cbind(ProtDat,Resrc)
mp1<-ggplot() +
  geom_raster(data = x , aes(x = Var1, y = Var2, fill = Protected)) + theme_void()+
  coord_quickmap()+
  scale_fill_manual(name="Area",breaks=c("Working"),values=c("#1f78b4"))



Protected <-c(rep(2,NumProtec),rep(0,Resources-NumProtec))
PercProtec<-0.6
NumProtec<-Resources*PercProtec
StartingIntegrity<-1
Resrc<-rbinom(Resources,1,StartingIntegrity)
Protected <-c(rep("Protected",NumProtec),rep("Working",Resources-NumProtec)) #Dummy values for protected vs not
ProtDat<-cbind(simgrid[, 1:2] - 0.5,Protected)
x<-cbind(ProtDat,Resrc)

mp2<-ggplot() +
  geom_raster(data = x , aes(x = Var1, y = Var2, fill = Protected)) + theme_void()+
  coord_quickmap()+
  scale_fill_manual(name="Area",breaks=c("Working","Protected",""),values=c("#33a02c","#1f78b4","white"))


Protected <-c(rep(2,NumProtec),rep(0,Resources-NumProtec))
PercProtec<-0.6
NumProtec<-Resources*PercProtec
StartingIntegrity<-0.2
Resrc<-rbinom(Resources,1,StartingIntegrity)
Protected <-c(rep("Protected",NumProtec),rep("Working",Resources-NumProtec)) #Dummy values for protected vs not
ProtDat<-cbind(simgrid[, 1:2] - 0.5,Protected)
x<-cbind(ProtDat,Resrc)
x[x$Resrc==0,]$Protected<-NA

mp3<-ggplot() +
  geom_raster(data = x , aes(x = Var1, y = Var2, fill = Protected)) + theme_void()+
  coord_quickmap()+
  scale_fill_manual(name="Area",breaks=c("Working","Protected",""),values=c("#33a02c","#1f78b4","white"))

Protected <-c(rep(2,NumProtec),rep(0,Resources-NumProtec))
PercProtec<-0.2
NumProtec<-Resources*PercProtec
StartingIntegrity<-0.2
Resrc<-rbinom(Resources,1,StartingIntegrity)
Protected <-c(rep("Protected",NumProtec),rep("Working",Resources-NumProtec)) #Dummy values for protected vs not
ProtDat<-cbind(simgrid[, 1:2] - 0.5,Protected)
x<-cbind(ProtDat,Resrc)
x[x$Resrc==0,]$Protected<-NA

mp4<-ggplot() +
  geom_raster(data = x , aes(x = Var1, y = Var2, fill = Protected)) + theme_void()+
  coord_quickmap()+
  scale_fill_manual(name="Area",breaks=c("Working","Protected",""),values=c("#33a02c","#1f78b4","white"))





