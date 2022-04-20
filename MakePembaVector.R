library("raster")
library("rgeos")
library("rgdal")
library(tidyverse)
library(sf)
library(magrittr)
#library("rasterVis")
#library(viridis)

#apply shehia specific deforestation rates
#load vector data
sizes<-read_csv("~/Pemba_Project/CoFMASizes.csv")
sizes%<>%group_by(CoFMA)%>%mutate(Total=sum(AlternativeUse,ConservationArea,UtilizationArea))%>%
  mutate(UseArea=sum(AlternativeUse,UtilizationArea))%>%
  mutate(UseAreaPerc=UseArea/Total)%>%mutate(ConsAreaPerc=ConservationArea/Total)

sizes%<>%mutate(ChangeYearlyRateForestLoss_FromCofma=-1*(CoverChange_PercPerYear_2010_2018-CoverChange_PercPerYear_2001_2010))

gadm3_3 <- readRDS("~/Pemba_Project/MapData/gadm36_TZA_3_sf.rds")
#ggplot() + geom_sf(data = gadm3_3) + theme_bw()
Pemba_vector <- st_crop(gadm3_3, xmin = 39.35, xmax = 40.0,
                        ymin = -5.75, ymax = -4.85)

#ggplot() + geom_sf(data = Pemba) + theme_bw()
Pemba_vector[Pemba_vector$NAME_3 == "Mgogoni" & Pemba_vector$NAME_2 =="Chake",]$NAME_3<-"Mgogoni South" #duplicate "mgogni" ward Does NOT have Cofma

CofmaWards<-c("Changaweni", "Fundo", "Gando", "Kambini","Kangani","Kifundi","Kisiwa Panza",
              "Mgelema","Mgogoni","Michenzani","Mjimbini","Mjini Wingwi","Msuka Magharibi","Mtambwe Kaskazini",
              "Mtambwe Kusini","Shumba Mjini","Tondooni","Tumbe Magharibi")


Pemba_vector$CoFMA<-ifelse(Pemba_vector$NAME_3 %in% CofmaWards, "CoFMA","No CoFMA")
sizes$NAME_3<-sizes$CoFMA
temp<-sizes%>%ungroup()%>%select(NAME_3,CoverChange_PercPerYear_2001_2010,CoverChange_PercPerYear_2010_2018,ConsAreaPerc)
Pemba_vector<-merge(Pemba_vector,temp,by="NAME_3",all.x = TRUE)

ggplot() + geom_sf(data = Pemba_vector, aes(fill=CoverChange_PercPerYear_2001_2010))+
  theme_bw() +theme( panel.background = element_rect(fill = "#fff7fb"),panel.grid.major = element_blank(),
                     axis.text=element_blank(), axis.ticks = element_blank())

shehia<-read_csv("~/Pemba_Project/PembaForestCoverAllShehia.csv")
names(shehia)[5]<-"Annual_rate_of_change_2001_2010"
names(shehia)[6]<-"Annual_rate_of_change_2010_2018"

length(Pemba_vector$NAME_3)
length(shehia$Shehia)

Pemba_vector$NAME_3 %in% shehia$Shehia
shehia$Shehia %in% Pemba_vector$NAME_3

Pemba_vector<-rename(Pemba_vector,  Shehia=NAME_3)
Pemba_vector<-merge(Pemba_vector,shehia,by="Shehia",all.x = TRUE)

Pemba_vector<-Pemba_vector%>% select(Shehia, CoFMA, ConsAreaPerc, Annual_rate_of_change_2001_2010, Annual_rate_of_change_2010_2018, Urban_status, geometry)

ggplot() + geom_sf(data = Pemba_vector, aes(fill=Annual_rate_of_change_2010_2018))+
  theme_bw() +theme( panel.background = element_rect(fill = "#fff7fb"),panel.grid.major = element_blank(),
                     axis.text=element_blank(), axis.ticks = element_blank())

