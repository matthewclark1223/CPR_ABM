library(raster)
library(tidyverse)
library(ggspatial)
library(sf)
library(magrittr)
library(rmapshaper)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(grid)


#Inset map with roads and shehia
gadm3_3 <- readRDS("~/Pemba_Project/MapData/gadm36_TZA_3_sf.rds")
Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")
PemRoads<- sf::read_sf("~/Pemba_Project/PembaRoads.shp")
PemRoads$Roads<-rep("Road\nnetwork",nrow(PemRoads))


maptheme<-theme(axis.title = element_text(color="black",size=16),
                axis.text = element_text(color="black",size=8),
                axis.ticks  = element_line(color="black"))


bigMap<-ggplot(Pemba)+
  geom_sf(fill="lightgrey",color=NA,size=0.3)+
  xlim(39.4,39.88)+
  scale_size(range = c(5,15)) +
  geom_sf(data=PemRoads,aes(color=Roads),fill=NA, size=0.5,alpha=0.5,show.legend=TRUE)+
  scale_color_manual(values=c("Road\nnetwork"="black"),name="")+
  annotation_scale(location = "br", width_hint = 0.5) +ylab("")+xlab("")+
  theme_bw()+maptheme+xlab("Longitude")+ylab("Latitude")





africamap <- ne_countries(scale = 'medium', type = 'map_units',
                          returnclass = 'sf',continent="africa")

Tz <- filter(africamap,admin=="United Republic of Tanzania")
TzBbox<-st_bbox(Tz)
sf_use_s2(FALSE)
Tanzania<-st_crop(africamap, TzBbox)



smallMap<-ggplot() + geom_sf(data = Tz, fill="#bdbdbd")+
  coord_sf(expand = FALSE)+ theme_bw()+
  geom_rect(aes(xmin=39.5, xmax=40.1, ymin=-5.7, ymax=-4.5),fill="#e7298a", color="black", alpha=0.3)+
  annotate(geom = "text", x = 35, y = -6, label = "Tanzania", fontface = "italic", color = "black", size = 3.5)+
  #theme( panel.background = element_blank(),panel.grid.major = element_blank(),
  #     axis.text=element_blank(), axis.ticks = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.5) +ylab("")+xlab("")+
  theme(plot.margin=unit(c(0.5,-4,0.5,0),"cm"))+theme_void()



cowplot::ggdraw() +
  cowplot::draw_plot(bigMap) +
  cowplot::draw_plot(smallMap, x = 0.24, y = 0.68, width = 0.3, height = 0.3)




#2018 all of Pemba map

Pem2018LC<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/SARLayers/pemmyLC2018updateSAR.tif")
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



ggplot(data = Pemba)+
  geom_tile(data = Pem2018LC_df , 
            aes(x = x, y = y,fill=as.character(layer))) +
  #geom_sf(data=PemRoads,color="black",fill=NA, size=0.3) + 
  #geom_sf_label(aes(label=NAME_3))+
  scale_fill_manual(values = cols,labels = c("Mangrove","High Forest",
                                             "Agriculture","Urban","Bare",
                                             "Coral rag","Other woody veg &\nAgroforestry","Water"),
                    name="Landcover")+
  scale_x_continuous(breaks=c(39.6,39.7,39.8))+
  annotation_scale(location = "bl", width_hint = 0.5)+xlab("Longitude")+ylab("Lattitude")+
  theme_bw()+maptheme


studyShehia<-c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
               "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
               "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi")
studyShehia<-Pemba%>%filter(NAME_3%in%studyShehia)
studyShehia$NAME_3<-rep("Study communities",nrow(studyShehia))

ggplot(data = Pemba)+
  geom_tile(data = Pem2018LC_df , 
            aes(x = x, y = y,fill=as.character(layer))) +
  #geom_sf(data=PemRoads,aes(color=Road),fill=NA, size=0.3,alpha=0.8,show.legend = "line") + 
  geom_sf(data=studyShehia,aes(color=NAME_3),fill=NA, size=0.8,alpha=0.5,show.legend = "line")+

  scale_fill_manual(values = cols,labels = c("Mangrove","High Forest",
                                             "Agriculture","Urban","Bare",
                                             "Coral rag","Other woody veg &\nAgroforestry","Water"),
                    name="Landcover",
                    guide = guide_legend(override.aes = list(fill = c( "#c7e9c0", "#00441b",  "#fdbf6f",  "#4d4d4d",
                                                                      "#ffffbf","#e7298a","#41ab5d","#4575b4"),
                                                             linetype=0) ))+
  scale_colour_manual(
    name="", values = c("Study communities"="black"),
    guide=guide_legend(override.aes = list(linetype=1,color="black",alpha=1)))+
  
  scale_x_continuous(breaks=c(39.6,39.7,39.8))+
  annotation_scale(location = "br", width_hint = 0.5)+xlab("Longitude")+ylab("Lattitude")+
  theme_bw()+maptheme



ggplot(data=filter(Pemba,NAME_2=="Micheweni"))+
  geom_sf(fill=NA)+
  geom_sf_label(aes(label=NAME_3),size=3)
