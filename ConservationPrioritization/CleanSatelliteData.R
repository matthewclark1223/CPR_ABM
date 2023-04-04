library(raster)
library(tidyverse)
library(sf)

Pemba<-read_sf("Data/PembaShapeFile.shp")
Fishnet<-read_sf("~/Pemba_Project/ParticipatoryMapping/ParticipatoryMappingProj/Fishnet.shp")


mangchange<-raster::raster("./Data/MangroveChange2020_2021.tif")

#mangchange[]<-ifelse(mangchange[]==0,NA,mangchange[])

MeanChangeNet<-raster::extract(x = mangchange, y = Fishnet,
                                      fun=sum, 
                                      df=TRUE)


write.csv(MeanChangeNet,"SatelliteObservedMangChangeSUM.csv")


SatMang<-read.csv("~/Pemba_Project/ConservationPrioritization/SatelliteObservedMangChangeSUM.csv")
PartDat<-read_sf("~/Pemba_Project/ParticipatoryMapping/ParticipatoryMangroveChange.shp")

PartDat$SumValue<-ifelse(PartDat$Mangrove==FALSE,NA,PartDat$SumValue)
PartDat$MeanValue<-ifelse(PartDat$Mangrove==FALSE,NA,PartDat$MeanValue)
PartDat$MangroveChange2020_2021<-SatMang$MangroveChange2020_2021
PartDat$MangroveChange2020_2021<-ifelse(PartDat$Mangrove==FALSE,NA,PartDat$MangroveChange2020_2021)


#Plot for mean change
ggplot(data=Pemba)+
  geom_sf(color=alpha("grey",0.99),fill=NA,alpha=0.99)+
  geom_sf(data=PartDat,aes(fill=MangroveChange2020_2021),alpha=0.99,color=alpha("darkgrey",0.01))+
  scale_fill_viridis_c( na.value=NA,name="Satellite observed\nchange (log)",
                        trans = scales::pseudo_log_trans(sigma = 0.001),
                        breaks=c(-0.02,0,0.04),labels=c("Decreasing cover","No change","Increasing cover") )+
  scale_x_continuous(breaks=c(39.6,39.8))+
  scale_y_continuous(breaks=c(-4.9,-5.1,-5.3,-5.5))+
  scale_size(range = c(5,15)) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.5)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+theme(#panel.grid = element_blank(),
    axis.text = element_text(angle = 0,size=12,color="black"))


#Plot for gross change
ggplot(data=Pemba)+
  geom_sf(color=alpha("grey",0.99),fill=NA,alpha=0.99)+
  geom_sf(data=PartDat,aes(fill=MangroveChange2020_2021),alpha=0.99,color=alpha("darkgrey",0.01))+
  scale_fill_viridis_b( na.value=NA,name="Satellite observed\nchange")+#,
                       # breaks=c(-0.02,0,0.04),labels=c("Decreasing cover","No change","Increasing cover") )+
  scale_x_continuous(breaks=c(39.6,39.8))+
  scale_y_continuous(breaks=c(-4.9,-5.1,-5.3,-5.5))+
  scale_size(range = c(5,15)) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.5)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+theme(#panel.grid = element_blank(),
    axis.text = element_text(angle = 0,size=12,color="black"))



###


mangchange_df<-raster::as.data.frame(mangchange, xy = TRUE) 
mangchange_df<-na.omit(mangchange_df)
names(mangchange_df)[3]<-"layer"
#0 = Mangrove
#1 = HF
#2 = agriculture
#3 = Urban
#4 = Bare
#5 = Coral rag
#6 = OWV/agroforestry
#7 = water
cols <- c("-1" = "red", "1" = "green")
Pemba <- sf::read_sf("~/Pemba_Project/PembaShapeFile.shp")
#library(tidyverse)
p<-ggplot(data = Pemba)+
  geom_sf(color="black",fill=NA, size=0.3) + 
  geom_tile(data = mangchange_df , 
            aes(x = x, y = y,fill=as.character(layer))) +
  #geom_sf_label(aes(label=NAME_3))+
  scale_fill_manual(values = cols,labels = c("Lost","Gained"),
                    name="Mangrove Change")+
  
  theme_bw()

#plotly::ggplotly(p) %>%
 # widgetframe::frameWidget()







