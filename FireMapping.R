library(sf)
library(tidyverse)
library(magrittr)
library(rmapshaper)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(grid)

gadm3_3 <- readRDS("~/Pemba_Project/MapData/gadm36_TZA_3_sf.rds")
#ggplot() + geom_sf(data = gadm3_3) + theme_bw()
Pemba <- st_crop(gadm3_3, xmin = 39.5, xmax = 39.9,
                 ymin = -5.6, ymax = -4.75)

#ggplot() + geom_sf(data = Pemba) + theme_bw()


#fires_2000<-read_csv("~/Pemba_Project/viirs-snpp_2018_Tanzania.csv")

pt_crs<-st_crs(Pemba)
#fires_2000<-st_as_sf(fires_2000, coords = c("longitude", "latitude"), crs = pt_crs)
#fires_2000<-fires_2000[Pemba,]#crop to pemba


#ggplot() + geom_sf(data = Pemba) + theme_bw()+
 # geom_sf(data = fires_2000) 



for(i in 2012:2020){
  x<-read_csv(paste0("~/Pemba_Project/VIIRS_Data/viirs-snpp_",i,"_Tanzania.csv"))
  x$year<-rep(i,nrow(x))
  x<-st_as_sf(x, coords = c("longitude", "latitude"), crs = pt_crs)
  assign(paste0("fires",i),x[Pemba,])#crop to pemba
}



firesall<-rbind(fires2012,fires2013,fires2014,fires2015,fires2016,fires2017,fires2018,fires2019,fires2020)
ggplot() + geom_sf(data = Pemba) + theme_bw()+
  geom_sf(data = firesall)


firesalldf <- firesall %>%
  mutate(lat = unlist(map(firesall$geometry,1)),
         long = unlist(map(firesall$geometry,2)))




ggplot() + geom_sf(data = Pemba) + theme_bw()+
stat_density2d(aes(x=lat,y=long,fill = ..level..), alpha = .4,
               geom = "polygon", data = firesalldf) + 
  geom_sf(data = firesall,alpha=0.4,color="black",size=2)+
  scale_fill_viridis_c(option="rocket",begin=0.2,end=.9,direction=-1)#cividis,plasma

p<-ggplot() + geom_sf(data = Pemba) + theme_bw()+
  geom_sf(data = firesall,color="darkred",size=2,alpha=0.5)+
  transition_states(year, transition_length = 1, state_length = 3, wrap = TRUE ) +
  ease_aes('linear') +
  shadow_mark()

animate(p)

ggplot() + geom_sf(data = Pemba) + theme_bw()+
  #stat_density2d(data=firesall)+
  geom_density2d_filled(data=firesall)
 
####Netcdf data below, not working
small_fires_2016<-ncdf4::nc_open("~/Pemba_Project/Small_fires.nc", verbose = TRUE)

small_fires_2016<-st_read(small_fires_2016)
plot(small_fires_2016)
