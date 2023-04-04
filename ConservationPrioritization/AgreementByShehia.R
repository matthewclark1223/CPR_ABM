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


Agreement<-MangCen%>%group_by(Shehia)%>%select(MeanValue,SatelliteSUM,Shehia )%>%na.omit()%>%
  summarise(Agree=cor(MeanValue,SatelliteSUM), n = n())%>%filter(n>=15)%>%
  arrange(Agree)
  
head(Agreement,n=4) #Worst fit is Ziwani and Tibirinzi
tail(Agreement,n=4)#Best fit is Kengeja and Kisiwani


ggplot(Pemba)+
  #geom_sf(data=Mang,fill="green")+
  geom_sf(data=Agreement,aes(color=Agree ))+
  scale_color_viridis_b(na.value="red" )


MangCen%>%filter(Shehia=="Tibirinzi" )%>%
  ggplot(., aes(x=MeanValue,y=SatelliteSUM))+geom_jitter()

#Need to figure out a way to add the values to the Pem_sampled data



Agreement2<-MangCen%>%group_by(Shehia)%>%select(MeanValue,SatelliteSUM,Shehia )%>%na.omit()%>%
  summarise(Agree=cor(MeanValue,SatelliteSUM), n = n())#%>%filter(n>=15)
Agreement2<-st_as_sf(merge(as.data.frame(Pemba[,"NAME_3"]),as.data.frame(Agreement2)[,1:2],by.x="NAME_3",by.y="Shehia",all.x = TRUE))

library(ggspatial)
ggplot(Agreement2)+
  #geom_sf(data=Mang,fill="green")+
  geom_sf(aes(fill=Agree ),color="grey")+
  scale_fill_viridis_b(na.value="lightgrey",name="Correlation of satellite \n& participatory data", option="D" )+
  scale_x_continuous(breaks=c(39.6,39.8))+
  scale_y_continuous(breaks=c(-4.9,-5.1,-5.3,-5.5))+
  scale_size(range = c(5,15)) +
  annotation_scale(location = "br", width_hint = 0.5)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+theme(#panel.grid = element_blank(),
    axis.text = element_text(angle = 0,size=12,color="black"))
  theme(legend.position = c(.1,.36))+
  theme(legend.key.size = unit(0.25, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.25, 'cm'), #change legend key width
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=6)) #change legend text font size
  




