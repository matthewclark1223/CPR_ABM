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

firesall<-filter(firesall,confidence != "l")


ggplot() + geom_sf(data = Pemba) + theme_bw()+
stat_density2d(aes(x=lat,y=long,fill = ..level..), alpha = .4,
               geom = "polygon", data = firesalldf) + 
  geom_sf(data = firesall,alpha=0.4,color="black",size=2)+
  scale_fill_viridis_c(option="plasma",begin=0.2,end=.9,direction=-1,name = "Number of \nRecorded Fires" )+#cividis,plasma,rocket
  scale_size(range = c(5,15)) +
  ylab("")+xlab("")+
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.03, "in"), 
                         style = north_arrow_fancy_orienteering)+
  theme( panel.background = element_rect(fill = "white"),panel.grid.major = element_blank(),
         axis.text=element_blank(), axis.ticks = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.5) #+ theme(legend.position = "none")


ggplot(firesall,aes(x=acq_date))+geom_histogram()





