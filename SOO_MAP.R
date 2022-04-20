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

st_write(Pemba, "PembaShapeFile.shp")

ggplot() + geom_sf(data = Pemba) + theme_bw()
Pemba[Pemba$NAME_3 == "Mgogoni" & Pemba$NAME_2 =="Chake",]$NAME_3<-"Mgogoni_2" #duplicate "mgogni" ward Does NOT have Cofma

StudyWards<-c("Tondooni","Msuka Magharibi","Tumbe Magharibi","Shumba Mjini","Kifundi","Mgogoni",
              "Gando","Mjini Wingwi","Mtambwe Kaskazini","Mtambwe Kusini","Fundo","Mgelema","Kisiwa Panza",
              "Kangani","Changaweni","Kambini","Chumbageni","Shungi","Ziwani","Piki",
              "Ukunjwi","Mjimbini","Michenzani","Kilindi")


##ea Map
africamap <- ne_countries(scale = 'medium', type = 'map_units',
                          returnclass = 'sf',continent="africa")


EastAfrica <- st_crop(africamap, xmin = 32, xmax = 60,
                      ymin = -26, ymax = 5)

EAfr<-ggplot() + geom_sf(data = EastAfrica, fill="#d9d9d9")+coord_sf(expand = FALSE)+ theme_bw()+
  geom_rect(aes(xmin=39.5, xmax=40.1, ymin=-5.7, ymax=-4.5),fill="#969696", color="black", alpha=0.3)+
  annotate(geom = "text", x = 38, y = 0.8, label = "Kenya", fontface = "italic", color = "black", size = 4)+
  annotate(geom = "text", x = 35, y = -6, label = "Tanzania", fontface = "italic", color = "black", size = 4)+
  annotate(geom = "text", x = 47, y = -18, label = "Madagascar", fontface = "italic", color = "black", size = 4)+
  annotate(geom = "text", x = 37.5, y = -13, label = "Mozambique", fontface = "italic", color = "black", size = 4)+
  theme( panel.background = element_rect(fill = "white"),panel.grid.major = element_blank(),
         axis.text=element_blank(), axis.ticks = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.5) +ylab("")+xlab("")+
  theme(plot.margin=unit(c(0.5,-4,0.5,0),"cm"))


Pemba$Study<-ifelse(Pemba$NAME_3 %in% StudyWards, "Study Communities","Not")



cols <- c("Not" = "#d9d9d9", "Study Communities" = "#525252")
Pem_map<-ggplot() + geom_sf(data = Pemba,aes(fill=Study),alpha=0.75,color="black")+theme_bw()+
  xlim(39.4,39.87)+
  scale_size(range = c(5,15)) +
  scale_colour_manual(
    values = cols,name=NULL,
    aesthetics = ( "fill")
  )+
  ylab("")+xlab("")+
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.03, "in"), 
                         style = north_arrow_fancy_orienteering)+
  theme( panel.background = element_rect(fill = "white"),panel.grid.major = element_blank(),
         axis.text=element_blank(), axis.ticks = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.5) #+ theme(legend.position = "none")

Pem_map
print(EAfr, vp = viewport(0.32, 0.655, width = 0.6, height = 0.6))

###Inset map no moons  

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
print(EAfr, vp = viewport(0.31, 0.762, width = 0.5, height = 0.5))


###############################
CofmaWards<-c("Changaweni", "Fundo", "Gando", "Kambini","Kangani","Kifundi","Kisiwa Panza",
              "Mgelema","Mgogoni","Michenzani","Mjimbini","Mjini Wingwi","Msuka Magharibi","Mtambwe Kaskazini",
              "Mtambwe Kusini","Shumba Mjini","Tondooni","Tumbe Magharibi")
Pemba$CoFMA<-ifelse(Pemba$NAME_3 %in% CofmaWards, "CoFMA","No CoFMA")
CWs<-Pemba%>%filter( CoFMA =="CoFMA")%>%
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]]))



Pem_map<-ggplot() + geom_sf(data = Pemba,aes(fill=Study),alpha=0.75,color="black")+
 #geom_sf_label(data=CWs,aes(label = NAME_3),size=2)+ 
  geom_sf_label(data=CWs,label = "REDD+",size=2.5, color="black",fontface="bold" )+ 
  theme_bw()+
  xlim(39.4,39.87)+
  scale_size(range = c(5,15)) +
  scale_colour_manual(
    values = cols,name=NULL,
    aesthetics = ( "fill")
  )+
  ylab("")+xlab("")+
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.03, "in"), 
                         style = north_arrow_fancy_orienteering)+
  theme( panel.background = element_rect(fill = "white"),panel.grid.major = element_blank(),
         axis.text=element_blank(), axis.ticks = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.5) #+ theme(legend.position = "none")

Pem_map


