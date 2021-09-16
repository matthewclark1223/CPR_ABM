library(raster)
#fires
r_merged<-raster::raster("ESA_Small_Fires_2019_Pemba.tif")
#slope
slope<-raster::raster("~/Pemba_Project/AmyData/slope.tif")
#Distance to road
RoadProx<-raster::raster("~/Pemba_Project/AmyData/roads_proximity.tif")
#soils
soils<-raster::raster("~/Pemba_Project/AmyData/soil_cat.tif")

#This works!!!
preprocess_raster<-function(raster,method){
  if(method=="ngb"){ x<-raster::projectRaster(raster,crs=crs(r_merged),method="ngb")}
 if(method=="bilinear"){ x<-raster::projectRaster(raster,crs=crs(r_merged))}
  return(resample(x,r_merged,method=method))
}

RoadProxRS<-preprocess_raster(raster=RoadProx,method="bilinear")
SlopeRS<-preprocess_raster(slope,method="bilinear")
#beginCluster()
SoilRS<-preprocess_raster(soils,method="ngb")
#endCluster()
stackRS <- raster::stack(RoadProxRS, SlopeRS,SoilRS,r_merged)
raster::writeRaster(stackRS,filename = "PembaFiresAndPredictors.tif",options="INTERLEAVE=BAND", overwrite=TRUE)
