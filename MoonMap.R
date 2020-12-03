library(sf)
library(tidyverse)
library(magrittr)
library(rmapshaper)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

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


#library(gggibbous)
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



