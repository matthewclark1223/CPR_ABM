library(tidyverse)
library(sf)
library(raster)
Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")
ggplot() + geom_sf(data = Pemba) + theme_bw()
pt_crs<-st_crs(Pemba)

z<-raster::raster( "~/Pemba_Project/SmallFires/h43v18/01_2019/20190101-LC.tif")
unique(z)

h43v19files<-vector()
months<-c(paste0("0",1:9,"_2019/"),paste0(10:12,"_2019/"))
for(m in months){
  x<-list.files(path=paste0("~/Pemba_Project/SmallFires/h43v19/",m))
  x<-x[5] #LC is the 5th file 
  x<-paste0("~/Pemba_Project/SmallFires/h43v19/",m,x)
  h43v19files<-c(h43v19files,x)
}

h43v18files<-vector()
months<-c(paste0("0",1:9,"_2019/"),paste0(10:12,"_2019/"))
for(m in months){
  x<-list.files(path=paste0("~/Pemba_Project/SmallFires/h43v18/",m))
  x<-x[5]
  x<-paste0("~/Pemba_Project/SmallFires/h43v18/",m,x)
  h43v18files<-c(h43v18files,x)
}

LC_Files<-c(h43v19files,h43v18files)

addRstrz<-function(rasters){
  rasterstackList<-list()
  for (f in rasters){
    x<-raster::raster(f)
    x<-raster::crop(x,Pemba)#crop to pemba
    #x[x<=0]<-0
    #x[x>0]<-1
    rasterstackList<-c(rasterstackList,x)
  }
  
  rasterstack<-raster::stack(rasterstackList)
  cummRaster<-sum(rasterstack)
  #cummRaster[cummRaster<=0]<-0
  #cummRaster[cummRaster>0]<-1
  return(cummRaster)
}

bottom<-addRstrz(rasters=h43v19files)
raster::plot(bottom)

top<-addRstrz(rasters=h43v18files)
raster::plot(top)

r_merged<- raster::merge(top,bottom)
raster::plot(r_merged)
#raster::writeRaster(r_merged,"ESA_Small_Fires_2019_Pemba.tif")
r_merged_df<-raster::as.data.frame(r_merged, xy = TRUE) 

r_merged_df<-filter(r_merged_df,layer==1)
ggplot()+
  geom_sf(data = Pemba) + 
  geom_tile(data = r_merged_df , 
            aes(x = x, y = y,color=layer)) +
  
  theme_bw()
