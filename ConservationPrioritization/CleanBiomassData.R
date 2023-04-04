library(tidyverse)
library(sf)
library(raster)
Biomass<-raster::raster("./Data/Biomass.tif")
Pemba<-read_sf("Data/PembaShapeFile.shp")

Fishnet<-read_sf("~/Pemba_Project/ParticipatoryMapping/ParticipatoryMappingProj/Fishnet.shp")

BiomassPem<-crop(Biomass, Fishnet)

BiomassPem_df<-raster::as.data.frame(BiomassPem, xy = TRUE) 
BiomassPem_df<-na.omit(BiomassPem_df)
names(BiomassPem_df)[3]<-"layer"

Pemba <- sf::read_sf("~/Pemba_Project/PembaShapeFile.shp")
ggplot(data = Pemba)+
  geom_tile(data = BiomassPem_df , 
            aes(x = x, y = y,fill=layer)) +
  geom_sf(color="#f0f0f0",fill=NA, size=0.3) + 
  scale_fill_viridis_c()+
  
  theme_bw()


BiomassNet<-raster::extract(x = BiomassPem, y = Fishnet,
                               fun=mean, 
                               df=TRUE,na.rm=TRUE )


#write.csv(BiomassNet,"BiomassNet.csv")


BiomassNet<-read.csv("~/Pemba_Project/ConservationPrioritization/BiomassNet.csv")

PartDat<-read_sf("~/Pemba_Project/ParticipatoryMapping/ParticipatoryMangroveChange.shp")

PartDat$SumValue<-ifelse(PartDat$Mangrove==FALSE,NA,PartDat$SumValue)
PartDat$MeanValue<-ifelse(PartDat$Mangrove==FALSE,NA,PartDat$MeanValue)
PartDat$Biomass<-BiomassNet$Biomass
PartDat$Biomass<-ifelse(PartDat$Mangrove==FALSE,NA,PartDat$Biomass)

SatMangSUM<-read.csv("~/Pemba_Project/ConservationPrioritization/SatelliteObservedMangChangeSUM.csv")
SatMangMEAN<-read.csv("~/Pemba_Project/ConservationPrioritization/SatelliteObservedMangChange.csv")

#Add the MEAN data
PartDat$SatMangMEAN<-SatMangMEAN$MangroveChange2020_2021
PartDat$SatMangMEAN<-ifelse(PartDat$Mangrove==FALSE,NA,PartDat$SatMangMEAN)

#Add the SUM data
PartDat$SatMangSUM<-SatMangSUM$MangroveChange2020_2021
PartDat$SatMangSUM<-ifelse(PartDat$Mangrove==FALSE,NA,PartDat$SatMangSUM)

ggplot(data=Pemba)+
  geom_sf(color=alpha("grey",0.99),fill=NA,alpha=0.99)+
  geom_sf(data=PartDat,aes(fill=Biomass),alpha=0.99,color=alpha("darkgrey",0.01))+
  scale_fill_viridis_c( na.value=NA,name="Aboveground\nbiomass (Mg/ha)")+
                       # trans = scales::pseudo_log_trans(sigma = 0.001),
                        #breaks=c(-0.02,0,0.04),labels=c("Decreasing cover","No change","Increasing cover") )+
  scale_x_continuous(breaks=c(39.6,39.8))+
  scale_y_continuous(breaks=c(-4.9,-5.1,-5.3,-5.5))+
  scale_size(range = c(5,15)) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.5)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+theme(#panel.grid = element_blank(),
    axis.text = element_text(angle = 0,size=12,color="black"))



##I added the change data in another script before making this
sf::st_write(PartDat,"CompleteGriddedData.shp", append=FALSE) 

#write.csv(PartDat,"CompleteGriddedData.csv")
