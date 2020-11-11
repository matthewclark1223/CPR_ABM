library(sf)
library(tidyverse)
library(rmapshaper)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
africamap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf',continent="africa")
# have a look at these two columns only
head(worldmap[c('name', 'continent')])


ggplot() + geom_sf(data = worldmap) + theme_bw()
ggplot() + geom_sf(data = africamap) + theme_bw()



TZ <- africamap[africamap$name == 'Tanzania',]
ggplot() + geom_sf(data = TZ) + theme_bw()


#x<-read_sf("~/Pemba_Project/MapData/ne_110m_land.shp")
Tanzania<-read_sf("~/Pemba_Project/MapData/TZA_adm0.shp")
ggplot() + geom_sf(data = Tanzania) + theme_bw()

EastAfrica <- st_crop(africamap, xmin = 32, xmax = 60,
                          ymin = -26, ymax = 5)

EAfrFin<-ggplot() + geom_sf(data = EastAfrica, fill="antiquewhite")+coord_sf(expand = FALSE)+ theme_bw()+
  geom_rect(aes(xmin=39.5, xmax=40.1, ymin=-5.7, ymax=-4.5),fill="#b2df8a", color="#33a02c", alpha=0.5)+
annotate(geom = "text", x = 38, y = 0.8, label = "Kenya", fontface = "italic", color = "grey22", size = 4)+
  annotate(geom = "text", x = 35, y = -6, label = "Tanzania", fontface = "italic", color = "grey22", size = 4)+
  annotate(geom = "text", x = 47, y = -18, label = "Madagascar", fontface = "italic", color = "grey22", size = 4)+
  annotate(geom = "text", x = 37.5, y = -13, label = "Mozambique", fontface = "italic", color = "grey22", size = 4)+
   theme( panel.background = element_rect(fill = "#c6dbef"),panel.grid.major = element_blank(),
          axis.text=element_blank(), axis.ticks = element_blank())+
   annotation_scale(location = "bl", width_hint = 0.5) +ylab("")+xlab("")+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"), 
                         style = north_arrow_fancy_orienteering)+theme(plot.margin=unit(c(0.5,-4,0.5,0),"cm"))




Pemba <- st_crop(Tanzania, xmin = 39.5, xmax = 39.9,
                 ymin = -5.6, ymax = -4.9)

sites<-data.frame(Site=c("Makangale","Kifundi","Msituu mkuu"),
                  lon=c(39.690753,39.710927,39.837124),
                  lat=c(-4.906206,-4.940105,-4.997158))

Pem_fin<-ggplot() + geom_sf(data = Pemba, fill="antiquewhite")+coord_sf()+ theme_bw()+
  geom_point(data = sites, aes(x = lon, y = lat), size = 4, 
             shape = 18) +
  annotate(geom = "text", x = 39.77, y = -5.13, label = "Pemba Island", fontface = "italic", color = "grey22", size = 4)+
  geom_text(data = sites, 
            aes(x=lon, y=lat, label = Site),
            size = 4,
            fontface = "bold",
            hjust = c(0,-0.3,1.1),vjust = c(-1,0,0)) +ylab("")+xlab("")+
  theme( panel.background = element_rect(fill = "#c6dbef"),panel.grid.major = element_blank(),
         axis.text=element_blank(), axis.ticks = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.5) +theme(plot.margin=unit(c(0.5,0,0.5,-4),"cm"))
  

Map<-gridExtra::grid.arrange(EAfrFin,Pem_fin,nrow=1)



#alternative method. Sloppy right now
library(grid)
Pem_fin
print(EAfrFin, vp = viewport(0.8, 0.27, width = 0.5, height = 0.5))



