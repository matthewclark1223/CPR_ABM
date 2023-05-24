library(sf)
library(tidyverse)


Pemba<-read_sf("Data/PembaShapeFile.shp")

PartDat<-read_sf("CompleteGriddedData.shp")
names(PartDat)<-c("CellNumber","Mangrove","SumValue","MeanValue","MedValue",
                  "Biomass","SatelliteSUM","SatelliteMEAN","geometry")


#Make a raster of the satellite observed change..Doing SUM here
Satellite_rast<-as(stars::st_rasterize(PartDat %>% dplyr::select(SatelliteSUM, geometry) ),"Raster")

#Make a raster of the biomass product
Biomass_rast<-as(stars::st_rasterize(PartDat %>% dplyr::select(Biomass, geometry) ),"Raster")

#Make a raster of mangrove presence (community reported)
Mang_rast<-as(stars::st_rasterize(PartDat %>% dplyr::select(Mangrove, geometry)),"Raster")

#Make a raster of the mean community reported change
ComChang_rast<-as(stars::st_rasterize(PartDat %>% dplyr::select(MeanValue, geometry)),"Raster")

Rstack<-raster::stack(ComChang_rast,Mang_rast,Biomass_rast,Satellite_rast)

names(Rstack)[1:4]<-c("ComChange","Mang","Biomass","Satellite")

Rstack_df<-raster::as.data.frame(Rstack, xy = TRUE) 
Rstack_df<-na.omit(Rstack_df)
head(Rstack_df)

plot(Rstack_df)

plot(Rstack_df$ComChange,Rstack_df$Satellite)

stdize<-function(x){
  (x-mean(x))/(2*sd(x))
}

ggplot(Rstack_df,aes(x=Biomass,y=Satellite))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(Rstack_df,aes(x=ComChange,y=Satellite))+
  geom_point()+
  geom_smooth(method="lm")


cor(Rstack_df$ComChange,Rstack_df$Satellite)

summary(lm(Satellite~ComChange,data=Rstack_df))
summary(lm(Satellite~stdize(Biomass)+ComChange,data=Rstack_df))



mytheme=theme(legend.position = "right", plot.background = element_rect(fill = "white", colour = NA),
              axis.text=element_text(color="black",size=15),panel.background = element_rect(fill = "white"),
              strip.background = element_rect(fill = "white", color = "white", size = 1),
              strip.text = element_text(colour = "black",face="bold",size=20),axis.title = element_text(color="black",size=20),
              axis.line = element_line(colour = "black", size = 1),
              legend.background = element_rect(fill="white"),legend.text = element_text(color="black",size=12),
              legend.title = element_text(color="black",size=15))

ggplot(Rstack_df,aes(x=ComChange,y=Satellite))+
  geom_jitter(size=2,alpha=0.5,shape=21,color="black",fill="#969696",width=0.2,height=0.2 )+
 geom_abline(intercept=0.29944,slope=0.78931,color="blue",size=1.5,linetype=2,alpha=0.9)+
  ggthemes::theme_clean()+mytheme+xlab("Average community response")+
  ylab("Net satellite observed pixel change")
  
