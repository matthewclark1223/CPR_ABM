library(sf)
library(tidyverse)
library(magrittr)
library(rmapshaper)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(grid)

gadm3_3 <- readRDS("~/Pemba_Project/MapData/gadm36_TZA_3_sf.rds")
Pemba<-read_sf("Data/PembaShapeFile.shp")

PartDat<-read_sf("~/Pemba_Project/ParticipatoryMapping/ParticipatoryMangroveChange.shp")

PartDat$SumValue<-ifelse(PartDat$Mangrove==FALSE,NA,PartDat$SumValue)
PartDat$MeanValue<-ifelse(PartDat$Mangrove==FALSE,NA,PartDat$MeanValue)

bigMap<-ggplot(data=Pemba)+
  geom_sf(color=alpha("grey",0.99),fill=NA,alpha=0.99)+
  geom_sf(data=PartDat,aes(fill=MeanValue),alpha=0.99,color=alpha("darkgrey",0.01))+
  scale_fill_viridis_c( na.value=NA,name="Average response",
                        breaks=c(-1,0,1),labels=c("Decreasing cover (-1)","No change","Increasing cover (+1)"), )+
  scale_x_continuous(breaks=c(39.6,39.8), limits = c(39.4,39.88))+
  scale_y_continuous(breaks=c(-4.9,-5.1,-5.3,-5.5))+
  
  scale_size(range = c(5,15)) +
  annotation_scale(location = "br", width_hint = 0.5)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+theme(#panel.grid = element_blank(),
                   axis.text = element_text(angle = 0,size=12,color="black"))+
  theme(legend.position = c(.2,.46))


#small map
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




#together
cowplot::ggdraw() +
  cowplot::draw_plot(bigMap) +
  cowplot::draw_plot(smallMap, x = 0.28, y = 0.68, width = 0.3, height = 0.3)




#SatelliteMap

PartDat<-read_sf("CompleteGriddedData.shp")
names(PartDat)<-c("CellNumber","Mangrove","SumValue","MeanValue","MedValue",
                  "Biomass","SatelliteSUM","SatelliteMEAN","geometry")

PartDat$SatelliteSUM[]<-ifelse(PartDat$Mangrove==0,NA,PartDat$SatelliteSUM[])


ggplot(data=Pemba)+
  geom_sf(color=alpha("grey",0.99),fill=NA,alpha=0.99)+
  geom_sf(data=PartDat,aes(fill=SatelliteSUM),alpha=0.99,color=alpha("darkgrey",0.01))+
  scale_fill_viridis_b(na.value=NA,name="Net satellite observed\npixel change")+
  #scale_fill_viridis_c( na.value=NA,name="Average response",
   #                     breaks=c(-1,0,1),labels=c("Decreasing cover","No change","Increasing cover"), )+
  scale_x_continuous(breaks=c(39.6,39.8))+
  scale_y_continuous(breaks=c(-4.9,-5.1,-5.3,-5.5))+
  
  scale_size(range = c(5,15)) +
  annotation_scale(location = "br", width_hint = 0.5)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+theme(#panel.grid = element_blank(),
    axis.text = element_text(angle = 0,size=12,color="black"))
