library(sf)
library(tidyverse)
library(magrittr)
library(rmapshaper)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(grid)
library(raster)

Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")
ggplot() + geom_sf(data = Pemba) + theme_bw()
pt_crs<-st_crs(Pemba)

#elevation
elevation<-raster::raster("~/Pemba_Project/AmyData/elevation.tif")
raster::plot(elevation)

#slope
slope<-raster::raster("~/Pemba_Project/AmyData/slope.tif")
raster::plot(slope)

#2017 forest cover
FC2017<-raster::raster("~/Pemba_Project/AmyData/F2017_15msmooth.tif")
raster::plot(FC2017)

#Human Density
HumanDens<-raster::raster("~/Pemba_Project/AmyData/human_density.tif")
raster::plot(HumanDens)

#Distance to road
RoadProx<-raster::raster("~/Pemba_Project/AmyData/roads_proximity.tif")
raster::plot(RoadProx)

#soils
soils<-raster::raster("~/Pemba_Project/AmyData/soil_cat.tif")
raster::plot(soils)
raster::crs(soils)




fr<-raster::raster("ESA_Small_Fires_2019_Pemba.tif")
raster::plot(fr)
