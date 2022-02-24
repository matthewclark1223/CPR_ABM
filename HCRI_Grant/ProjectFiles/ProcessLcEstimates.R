library(raster)
Pem2018LC<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/UnProcessed_Pem_2018_LC_20m_V2.tif")
fireRS<-raster::raster("PredFire2019.tif")
stackRS<-raster::stack("PembaFiresAndPredictors.tif")
preprocess_raster<-function(raster,method){
  if(method=="ngb"){ x<-raster::projectRaster(raster,crs=crs(fireRS),method="ngb")}
  if(method=="bilinear"){ x<-raster::projectRaster(raster,crs=crs(fireRS))}
  return(resample(x,fireRS,method=method))
}
Pem2018LC<-preprocess_raster(Pem2018LC,method="ngb")
Pem2018LC<-raster::mask(Pem2018LC,stackRS$PembaFiresAndPredictors.1)
plot(Pem2018LC)
raster::writeRaster(Pem2018LC,"~/Pemba_Project/HCRI_Grant/ProjectFiles/Pem_2018_LC_20m_V2.tif",overwrite=T)

Pem2018LC<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/Pem_2018_LC_20m_V2.tif")
#Plot it
r <- as.factor(Pem2018LC)

## Add a landcover column to the Raster Attribute Table
rat <- levels(r)[[1]]
rat[["landcover"]] <- c("Mangrove","High Forest","Ag", "Urban","Bare","Woody veg &\nagroforestry","Water")
levels(r) <- rat

## Plot
cols<-c("#a6cee3","#33a02c","#fb9a99","#1f78b4","darkgrey","#b2df8a","white")
rasterVis::levelplot(r, col.regions=terrain.colors(7), xlab="", ylab="")
rasterVis::levelplot(r, col.regions=cols, xlab="", ylab="")


