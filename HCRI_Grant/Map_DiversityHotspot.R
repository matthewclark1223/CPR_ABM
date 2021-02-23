library(sf)
library(tidyverse)
library(magrittr)
library(rmapshaper)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(grid)

gadm3_3 <- readRDS("~/Pemba_Project/MapData/gadm36_TZA_3_sf.rds")
Pemba <- st_crop(gadm3_3, xmin = 39.5, xmax = 39.9,
                 ymin = -5.6, ymax = -4.75)
Pemba[Pemba$NAME_3 == "Mgogoni" & Pemba$NAME_2 =="Chake",]$NAME_3<-"Mgogoni_2" #duplicate "mgogni" ward Does NOT have Cofma

CofmaWards<-c("Changaweni", "Fundo", "Gando", "Kambini","Kangani","Kifundi","Kisiwa Panza",
              "Mgelema","Mgogoni","Michenzani","Mjimbini","Mjini Wingwi","Msuka Magharibi","Mtambwe Kaskazini",
              "Mtambwe Kusini","Shumba Mjini","Tondooni","Tumbe Magharibi")


Pemba$CoFMA<-ifelse(Pemba$NAME_3 %in% CofmaWards, "CoFMA","No CoFMA")
CWs<-Pemba%>%filter( CoFMA =="CoFMA")%>%
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]]))

cols <- c("No CoFMA" = "#a6cee3", "CoFMA" = "#33a02c")

africamap <- ne_countries(scale = 'medium', type = 'map_units',
                          returnclass = 'sf',continent="africa")


EastAfrica <- st_crop(africamap, xmin = 32, xmax = 60,
                      ymin = -26, ymax = 5)


hotspots<-read_sf("hotspots_2016_1.shp")
CFEA<-hotspots%>%filter(NAME=="Coastal Forests of Eastern Africa" )
ggplot()+geom_sf(data=CFEA)

EAfr<-ggplot() + geom_sf(data = EastAfrica, fill="#d9d9d9")+
    geom_sf(data = CFEA, fill="red")+
    coord_sf(expand = FALSE)+ theme_bw()+
  geom_rect(aes(xmin=39.5, xmax=40.1, ymin=-5.7, ymax=-4.5),fill="#969696", color="black", alpha=0.3)+
  annotate(geom = "text", x = 38, y = 0.8, label = "Kenya", fontface = "italic", color = "black", size = 3.5)+
  annotate(geom = "text", x = 35, y = -6, label = "Tanzania", fontface = "italic", color = "black", size = 3.5)+
  annotate(geom = "text", x = 47, y = -18, label = "Madagascar", fontface = "italic", color = "black", size = 3.5)+
  annotate(geom = "text", x = 37.5, y = -13, label = "Mozambique", fontface = "italic", color = "black", size = 3.5)+
  theme( panel.background = element_rect(fill = "white"),panel.grid.major = element_blank(),
         axis.text=element_blank(), axis.ticks = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.5) +ylab("")+xlab("")+
  theme(plot.margin=unit(c(0.5,-4,0.5,0),"cm"))

Mapp<- ggplot() + geom_sf(data = Pemba,color="black",fill="#d9d9d9")+theme_bw()+
  xlim(39.4,39.88)+
  scale_size(range = c(5,15)) +
  ylab("")+xlab("")+
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.03, "in"), 
                         style = north_arrow_fancy_orienteering)+
  theme( panel.background = element_rect(fill = "white"),panel.grid.major = element_blank(),
         axis.text=element_blank(), axis.ticks = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.5) #+theme(plot.margin=unit(c(0.5,-10,0.5,0.5),"cm"))


Mapp
print(EAfr, vp = viewport(0.31, 0.762, width = 0.4, height = 0.5))
