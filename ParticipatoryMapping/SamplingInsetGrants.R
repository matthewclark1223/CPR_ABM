library(sf)
library(tidyverse)
library(magrittr)
library(rmapshaper)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(grid)

gadm3_3 <- readRDS("~/Pemba_Project/MapData/gadm36_TZA_3_sf.rds")
Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")


d<-read.csv("CommunitySamplingDatasheet.csv")
Sampled<-d[!is.na(d$Sampled),]$Shehia
NeedToSample<-d[is.na(d$Sampled),]$Shehia
which(Sampled %in% Pemba$NAME_3==F)
which(NeedToSample %in% Pemba$NAME_3==F)

Pemba$Sample<-ifelse(Pemba$NAME_3 %in% Sampled, "Sampled",
                     ifelse(Pemba$NAME_3 %in% NeedToSample, "Not yet sampled",NA))


cols <- c("Sampled" = "#b2df8a", "Not yet sampled" = "#1f78b4" )


bigMap<-ggplot(Pemba)+
  geom_sf(aes(fill=Sample))+
  scale_fill_manual(name="Study Communities",values=cols,na.value="#f0f0f0" )+
  xlim(39.4,39.88)+
  scale_size(range = c(5,15)) +
  annotation_scale(location = "br", width_hint = 0.5) +ylab("")+xlab("")+
  theme_bw()+theme(axis.text = element_text(color="black"))





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
  cowplot::draw_plot(smallMap, x = 0.21, y = 0.68, width = 0.3, height = 0.3)

