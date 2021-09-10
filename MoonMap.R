library(sf)
library(tidyverse)
library(magrittr)
library(rmapshaper)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(grid)

gadm3_3 <- readRDS("~/Pemba_Project/MapData/gadm36_TZA_3_sf.rds")
ggplot() + geom_sf(data = gadm3_3) + theme_bw()
Pemba <- st_crop(gadm3_3, xmin = 39.5, xmax = 39.9,
                 ymin = -5.6, ymax = -4.75)

ggplot() + geom_sf(data = Pemba) + theme_bw()
Pemba[Pemba$NAME_3 == "Mgogoni" & Pemba$NAME_2 =="Chake",]$NAME_3<-"Mgogoni_2" #duplicate "mgogni" ward Does NOT have Cofma

CofmaWards<-c("Changaweni", "Fundo", "Gando", "Kambini","Kangani","Kifundi","Kisiwa Panza",
"Mgelema","Mgogoni","Michenzani","Mjimbini","Mjini Wingwi","Msuka Magharibi","Mtambwe Kaskazini",
"Mtambwe Kusini","Shumba Mjini","Tondooni","Tumbe Magharibi")


Pemba$CoFMA<-ifelse(Pemba$NAME_3 %in% CofmaWards, "CoFMA","No CoFMA")
CWs<-Pemba%>%filter( CoFMA =="CoFMA")%>%
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
                                      lat=map_dbl(geometry, ~st_centroid(.x)[[2]]))

cols <- c("No CoFMA" = "#a6cee3", "CoFMA" = "#33a02c")
ggplot() + geom_sf(data = Pemba, aes(fill=CoFMA))+ geom_sf_label(data=CWs,aes(label = NAME_3),size=2)+ 
  theme_bw() +scale_colour_manual(
    values = cols,
    aesthetics = ( "fill")
  )

#
sizes<-read_csv("~/Pemba_Project/CoFMASizes.csv")
sizes%<>%group_by(CoFMA)%>%mutate(Total=sum(AlternativeUse,ConservationArea,UtilizationArea))%>%
  mutate(UseArea=sum(AlternativeUse,UtilizationArea))%>%
  mutate(UseAreaPerc=UseArea/Total)%>%mutate(ConsAreaPerc=ConservationArea/Total)
CWs$CoFMA<-CWs$NAME_3

CWs<-merge.data.frame(CWs,sizes,by="CoFMA")

CWs$lonadj<-CWs$lon+c(0, -0.025, 0.01, -0.025, 0.025, 0.03, -0.03, 
                      0.025, -0.025, 0.03, 0.03, -0.025, 0.025, 0.035, 
                      0.025, -0.005, -0.025, -0.025)
CWs$latadj<-CWs$lat+c(0.015, 0, 0.015, -0.035, 0, -0.018, -0.05, 
                      0.005, -0.025, -0.01, 0.018, -0.005, 0.035, 0, 
                      0, 0.035, 0, -0.005)


library(gggibbous)
cols <- c("No CoFMA" = "#d9d9d9", "CoFMA" = "#525252")

ggplot() + geom_sf(data = Pemba,aes(fill=CoFMA))+theme_bw()+
  geom_segment(data = CWs,aes(x=lon,xend = lonadj,y=lat, yend = latadj), color = "black") +
  geom_point(data = CWs,aes(lon, lat), size = 2, color = "black") +
  geom_moon(data=CWs,aes(x=lonadj,y=latadj,ratio=UseAreaPerc,size=Total),right = FALSE, fill = "#a6cee3", color = "#a6cee3",
            key_glyph = draw_key_moon_left
  ) +
  geom_moon(data=CWs,
    aes(x=lonadj,y=latadj,ratio = ConsAreaPerc,size=Total),
    fill = "#33a02c", color = "#33a02c"
  )+
  scale_size(range = c(6,12)) +
  geom_label(data=CWs,aes(x=lonadj,y=latadj+0.02,label = NAME_3),size=3,fill="white")+
  scale_colour_manual(
    values = cols,name=NULL,
    aesthetics = ( "fill")
  )+
  ylab("")+xlab("")+
  theme( panel.background = element_rect(fill = "white"),panel.grid.major = element_blank(),
         axis.text=element_blank(), axis.ticks = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.5) 




CWs$lonadj<-CWs$lon+c(0, -0.025, 0.015, -0.025, 0.025, 0.03, -0.03, 
                      0.025, 0.025, 0.03, 0.03, -0.025, 0.025, 0.035, 
                      0.03, -0.005, -0.035, -0.025)
CWs$latadj<-CWs$lat+c(0.022, 0, -0.02, -0.035, 0, -0.018, -0.03, 
                      0.005, -0.03, -0.01, 0.018, -0.005, 0.035, 0, 
                      0, 0.035, 0, -0.005)

ggplot() + geom_sf(data = Pemba,aes(fill=CoFMA),color="black")+theme_bw()+
  geom_segment(data = CWs,aes(x=lon,xend = lonadj,y=lat, yend = latadj),size=1 ,color = "black") +
  geom_point(data = CWs,aes(lon, lat), size = 2, color = "black") +
  geom_moon(data=CWs,aes(x=lonadj,y=latadj,ratio=UseAreaPerc,size=Total),right = FALSE, fill = "#74a9cf", color = "#74a9cf",
            key_glyph = draw_key_moon_left
  ) +
  geom_moon(data=CWs,
            aes(x=lonadj,y=latadj,ratio = ConsAreaPerc,size=Total),
            fill = "#33a02c", color = "#33a02c"
  )+
  scale_size(range = c(5,15)) +
  scale_colour_manual(
    values = cols,name=NULL,
    aesthetics = ( "fill")
  )+
  ylab("")+xlab("")+
  theme( panel.background = element_rect(fill = "white"),panel.grid.major = element_blank(),
         axis.text=element_blank(), axis.ticks = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.5) 



##make inset moon
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




 MoonMap<- ggplot() + geom_sf(data = Pemba,aes(fill=CoFMA),color="black")+theme_bw()+
    geom_segment(data = CWs,aes(x=lon,xend = lonadj,y=lat, yend = latadj),size=1 ,color = "black") +
    geom_point(data = CWs,aes(lon, lat), size = 2, color = "black") +
    geom_moon(data=CWs,aes(x=lonadj,y=latadj,ratio=UseAreaPerc,size=Total),right = FALSE, fill = "#74a9cf", color = "#74a9cf",
              key_glyph = draw_key_moon_left
    ) +
    geom_moon(data=CWs,
              aes(x=lonadj,y=latadj,ratio = ConsAreaPerc,size=Total),
              fill = "#33a02c", color = "#33a02c"
    )+
    xlim(39.4,39.88)+
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
    annotation_scale(location = "bl", width_hint = 0.5) #+theme(plot.margin=unit(c(0.5,-10,0.5,0.5),"cm"))


MoonMap
 print(EAfr, vp = viewport(0.11, 0.755, width = 0.5, height = 0.5))

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
  
