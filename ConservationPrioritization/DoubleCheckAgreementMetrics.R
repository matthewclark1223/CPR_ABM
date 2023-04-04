library(sf)
library(dplyr)
library(ggplot2)
library(tidyverse)


Pemba<-read_sf("Data/PembaShapeFile.shp")

PartDat<-read_sf("CompleteGriddedData.shp")
names(PartDat)<-c("CellNumber","Mangrove","SumValue","MeanValue","MedValue",
                  "Biomass","SatelliteSUM","SatelliteMEAN","geometry")


Mang<-PartDat%>%filter(Mangrove==1)

#fit<-lm(SatelliteSUM~MeanValue,data=Mang)
#coef(fit)
#ggplot(Mang,aes(x=MeanValue,y=SatelliteSUM))+
#  geom_jitter(size=2,alpha=0.5,shape=21,color="black",fill="#969696",width=0.2,height=0.2 )+
# geom_abline(intercept=0.248,slope=0.6877,color="blue",size=1.5,linetype=2,alpha=0.9)+
#ggthemes::theme_clean()+xlab("Average community response")+
#  ylab("Net satellite observed pixel change")




MangCen<-Mang%>% st_centroid()


###### Different and prob better solution
d<-read.csv("~/Pemba_Project/ParticipatoryMapping/Community Sampling Datasheet.csv")
Sampled<-d[d$Sampled==1,]$Shehia
PemSamp<-Pemba%>%filter(NAME_3%in%Sampled)

MangCen$Shehia<-st_nearest_feature(MangCen, PemSamp)
MangCen$Shehia<-PemSamp[MangCen$Shehia,]$NAME_3

ggplot(Pemba)+
  #geom_sf(data=Mang,fill="green")+
  geom_sf(data=MangCen,aes(color=Shehia ),size=1)+
  scale_color_viridis_d(na.value="red", option="turbo" )


Agreement<-MangCen%>%group_by(Shehia)%>%dplyr::select(SumValue,SatelliteSUM,Shehia )%>%na.omit()%>%
  summarise(Agree=cor(SumValue,SatelliteSUM), n = n())%>%filter(n>=15)%>%
  arrange(Agree)

head(Agreement,n=4) #Worst fit is Ziwani and Tibirinzi
tail(Agreement,n=4)#Best fit is Kengeja and Kisiwani


Agreement<-MangCen%>%group_by(Shehia)%>%select(MeanValue,SatelliteSUM,Shehia )%>%na.omit()%>%
  summarise(Agree=cor(MeanValue,SatelliteSUM), n = n())%>%filter(n>=15)%>%
  arrange(Agree)

head(Agreement,n=4) #Worst fit is Ziwani and Tibirinzi
tail(Agreement,n=4)#Best fit is Kengeja and Kisiwani