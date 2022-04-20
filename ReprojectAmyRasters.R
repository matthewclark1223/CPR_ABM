library(raster)
library(sf)
#fires
r_merged<-raster::raster("~/Pemba_Project/ESA_Small_Fires_2019_Pemba_CERTAIN.tif")
#slope
slope<-raster::raster("~/Pemba_Project/AmyData/slope.tif")

#Distance to road NOT FROM AMY
RoadProx<-raster::raster("~/Pemba_Project/PemmyDistRdsCLEAN.tif")

#soils
soils<-raster::raster("~/Pemba_Project/AmyData/soil_cat.tif")

#This works!!!
preprocess_raster<-function(raster,method){
  if(method=="ngb"){ x<-raster::projectRaster(raster,crs=crs(r_merged),method="ngb")}
 if(method=="bilinear"){ x<-raster::projectRaster(raster,crs=crs(r_merged))}
  return(resample(x,r_merged,method=method))
}

SlopeRS<-preprocess_raster(slope,method="bilinear")

SoilRS<-preprocess_raster(soils,method="ngb")

stackRS <- raster::stack(RoadProx, SlopeRS,SoilRS,r_merged)
raster::writeRaster(stackRS,filename = "PembaFiresAndPredictors.tif",options="INTERLEAVE=BAND", overwrite=TRUE)
