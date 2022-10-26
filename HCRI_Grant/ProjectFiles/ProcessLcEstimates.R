library(raster)
#Pem2018LC<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/UnProcessed_pemmyRF2018_R5.tif")
Pem2018LC<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/UnProcessed_pemmyLC2018update.tif")
Pem2018LC<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/SARLayers/pemmyLC2018updateSAR_unprocessed.tif")
fireRS<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/PredFire2019.tif")
stackRS<-raster::stack("~/Pemba_Project/HCRI_Grant/ProjectFiles/PembaFiresAndPredictors.tif")
preprocess_raster<-function(raster,method){
  if(method=="ngb"){ x<-raster::projectRaster(raster,crs=crs(fireRS),method="ngb")}
  if(method=="bilinear"){ x<-raster::projectRaster(raster,crs=crs(fireRS))}
  return(resample(x,fireRS,method=method))
}
Pem2018LC<-preprocess_raster(Pem2018LC,method="ngb")
Pem2018LC<-raster::mask(Pem2018LC,stackRS$PembaFiresAndPredictors.1)
plot(Pem2018LC)
#raster::writeRaster(Pem2018LC,"~/Pemba_Project/HCRI_Grant/ProjectFiles/pemmyRF2018_R5.tif",overwrite=T)
raster::writeRaster(Pem2018LC,"~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/pemmyLC2018update.tif",overwrite=T)
raster::writeRaster(Pem2018LC,"~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/SARLayers/pemmyLC2018updateSAR.tif",overwrite=T)
Pem2018LC<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/pemmyRF2018_R5.tif")
#Plot it
r <- as.factor(Pem2018LC)

## Add a landcover column to the Raster Attribute Table
rat <- levels(r)[[1]]
rat[["landcover"]] <- c("Mangrove","High Forest","Ag", "Urban","Bare","Coral rag","Woody veg &\nagroforestry","Water")
levels(r) <- rat

## Plot
#cols<-c("#a6cee3","#33a02c","#fb9a99","#1f78b4","darkgrey","#b2df8a","white")
#rasterVis::levelplot(r, col.regions=terrain.colors(8), xlab="", ylab="")
#rasterVis::levelplot(r, col.regions=cols, xlab="", ylab="")

###
Pem2018LC<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/pemmyLC2018update.tif")
Pem2018LC_df<-raster::as.data.frame(Pem2018LC, xy = TRUE) 
Pem2018LC_df<-na.omit(Pem2018LC_df)
names(Pem2018LC_df)[3]<-"layer"
#0 = Mangrove
#1 = HF
#2 = agriculture
#3 = Urban
#4 = Bare
#5 = Coral rag
#6 = OWV/agroforestry
#7 = water
cols <- c("0" = "#c7e9c0", "1" = "#00441b", "2" = "#fdbf6f", "3" = "#4d4d4d",
          "4"= "#ffffbf","5"="#e7298a","6"="#41ab5d","7"="#4575b4")
Pemba <- sf::read_sf("~/Pemba_Project/PembaShapeFile.shp")
library(tidyverse)
ggplot(data = Pemba)+
  geom_sf(color="#f0f0f0",fill=NA, size=0.3) + 
  geom_tile(data = Pem2018LC_df , 
            aes(x = x, y = y,fill=as.character(layer))) +
  
  #geom_sf_label(aes(label=NAME_3))+
  scale_fill_manual(values = cols,labels = c("Mangrove","High Forest",
                                             "Agriculture","Urban","Bare",
                                             "Coral rag","Other woody veg &\nAgroforestry","Water"),
                    name="Landcover")+
  ggtitle("Landcover classification estimates (2018); Pemba")+
  
  theme_bw()

#Clip to kanganani
kojani<-filter(Pemba,NAME_3=="Kojani")
LC_clipped<- crop(Pem2018LC, extent(kojani))
LC_clipped <- mask(LC_clipped, kojani)
LC_clipped_df<-raster::as.data.frame(LC_clipped, xy = TRUE) 
names(LC_clipped_df)[3]<-"layer"

LC_clipped_df<-na.omit(LC_clipped_df)


#plot it
ggplot(data = kojani)+
  
  geom_tile(data = LC_clipped_df , 
            aes(x = x, y = y,fill=as.character(layer))) +
  geom_sf(color="#f0f0f0",fill=NA, size=0.3) + 
  scale_fill_manual(values = cols,labels = c("Mangrove","High Forest",
                                             "Agriculture","Urban","Bare",
                                             "Coral rag","Other woody veg &\nAgroforestry","Water"),
                    name="Landcover")+
  ggtitle("Landcover classification estimates (2018); Kojani")+
  
  theme_bw()

#Clip to fundo
fundo<-filter(Pemba,NAME_3=="Maziwa Ng'ombe")
LC_clipped<- crop(Pem2018LC, extent(fundo))
LC_clipped <- mask(LC_clipped, fundo)
LC_clipped_df<-raster::as.data.frame(LC_clipped, xy = TRUE) 
names(LC_clipped_df)[3]<-"layer"

LC_clipped_df<-na.omit(LC_clipped_df)


#plot it
ggplot(data = fundo)+
  
  geom_tile(data = LC_clipped_df , 
            aes(x = x, y = y,fill=as.character(layer))) +
  geom_sf(color="#f0f0f0",fill=NA, size=0.3) + 
  scale_fill_manual(values = cols,labels = c("Mangrove","High Forest",
                                             "Agriculture","Urban","Bare",
                                             "Coral rag","Other woody veg &\nAgroforestry","Water"),
                    name="Landcover")+
  ggtitle("Landcover classification estimates (2018); Fundo")+
  
  theme_bw()
